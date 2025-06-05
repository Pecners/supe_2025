library(tidyverse)
library(tigris)
library(sf)
library(glue)
library(mapboxapi)
library(leaflet)
library(janitor)
library(colorspace)
library(scales)

vf <- "data/mke_county_dist_4_2024.csv"

d <- read_csv(vf) |> 
  # handle cases where wards are accidentally separated by . instead of ,
  # as in 4.9 instead of 4,9
  mutate(original_ward = Ward,
         Ward = str_replace_all(Ward, "(?<=\\d)\\.(?=\\d)", ","))

nd <- d |> 
  mutate(Ward = str_trim(Ward)) |> 
  mutate(town_type = case_when(
    str_detect(Ward, "City|C\\.") ~ "C",
    str_detect(Ward, "Village|V\\.") ~ "V",
    TRUE ~ "ERROR"
  ),
  these_wards = str_extract(Ward, "(?<=Ward).*") |> 
    str_remove_all("s ") |> 
    str_trim(),
  city = str_remove_all(Ward, "City|Village|of|\\d|(?<=\\d).|Ward|Wards|V\\.|C\\.|,|-|\\.") |> 
    str_remove_all(" s") |> 
    str_trim(),
  city = ifelse(city == "So Milw", "South Milwaukee", city))

full_pads <- map_df(1:nrow(nd), function(i) {
  this <- nd[i,]
  
  if (str_detect(this$these_wards, ",")) {
    these <- str_split_1(this$these_wards, ",")
    padded <- str_pad(these, width = 4, side = "left", pad = "0") |> 
      str_flatten_comma()
    combo <- TRUE
  } else if (str_detect(this$these_wards, "-")) {
    these <- str_split_1(this$these_wards, "-")
    s <- seq(from = these[1], to = these[2])
    padded <- str_pad(s, width = 4, side = "left", pad = "0") |> 
      str_flatten_comma()
    combo <- TRUE
  } else {
    padded <- str_pad(this$these_wards, width = 4, side = "left", pad = "0")
    combo <- FALSE
  }
  this |> 
    mutate(new_lab = glue("{city} - {town_type} {padded}"),
           wards = padded,
           original_ward,
           combo)
})



# wards <- tigris::voting_districts(state = "WI", county = "Milwaukee",
#                                   year = 2024) |>
#   mutate(ward = str_extract(NAME20, "\\d*$"),) |>
#   select(w = NAME20, ward, geometry) |>
#   mutate(w = str_remove_all(w, "\\."),
#          ind = row_number())
new_elex <- read_rds("data/2025_elex_for_jack.rda")

all <- full_pads |> 
  select(1:4, new_lab) |> 
  left_join(new_elex) |> 
  st_as_sf() |> 
  st_transform(crs = 4326) |> 
  clean_names() |> 
  mutate(j_margin = jack_eckblad / (jack_eckblad +
                                      ron_jansen +
                                      write_in),
         jm_count = jack_eckblad - ron_jansen)

# all |> 
#   ggplot() +
#   geom_sf(fill = "red")


pa <- brewer.pal(11, "RdBu")
pal <- colorBin(rev(pa), domain = c(0, 1), bins = 11)

leaflet(all) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 2,
              fillColor = ~pal(j_margin), 
              fillOpacity = 1, 
              color = "white",
              popup = glue("<strong>Ward {all$ward}</strong>",
                           "<br>Eckblad Margin: {label_percent(.1)(all$j_margin)}",
                           "<br>Underly Margin: {label_percent(.1)(all$perc)}"),
              popupOptions = popupOptions(autoPan = TRUE),
              highlight = highlightOptions(color = "black", fillColor = "white",
                                           fillOpacity = .25, bringToFront = TRUE)) |> 
  addLegend(pal = pal, values = 11, position = "topright", title = "Eckblad Share")

pal2 <- colorBin(rev(pa), domain = c(0, 550), bins = 11)

leaflet(all) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 2,
              fillColor = ~pal2(jack_eckblad), 
              fillOpacity = 1, 
              color = "white",
              popup = glue("<strong>Ward {all$ward}</strong>",
                           "<br>Eckblad Margin: {label_percent(.1)(all$j_margin)}",
                           "<br>Underly Margin: {label_percent(.1)(all$perc)}"),
              popupOptions = popupOptions(autoPan = TRUE),
              highlight = highlightOptions(color = "black", fillColor = "white",
                                           fillOpacity = .25, bringToFront = TRUE)) |> 
  addLegend(pal = pal2, values = 11, position = "topright", title = "Eckblad Share")

hh <- all |> 
  mutate(jm_count = jack_eckblad - ron_jansen) |> 
  filter(round(j_margin, .01) >= .60 &
           jm_count > 100)

sum(hh$jm_count) / (sum(all$jack_eckblad) - sum(all$ron_jansen))
  
leaflet(hh) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 2,
              fillColor = ~pal(j_margin), 
              fillOpacity = 1, 
              color = "white",
              popup = glue("<strong>Ward {hh$ward}</strong>",
                           "<br>Eckblad Margin: {label_percent(1)(hh$j_margin)}",
                           "<br>Underly Margin: {label_percent(1)(hh$perc)}"),
              popupOptions = popupOptions(autoPan = TRUE),
              highlight = highlightOptions(color = "black", fillColor = "white",
                                           fillOpacity = .25, bringToFront = TRUE)) |> 
  addLegend(pal = pal, values = 11, position = "topright", title = "Eckblad Share")

all |> 
  mutate(u_marg = jill_underly / (brittany_kinser+jill_underly)) |> 
  ggplot(aes(u_marg, j_margin, size = jack_eckblad)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = .5) +
  geom_vline(xintercept = .5)


all |> 
  ggplot(aes(kinser_over, j_margin)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = .5) 



ta <- all |> 
  filter(!is.na(j_margin)) |> 
  mutate(j = j_margin * 100,
         u = perc * 100)

cor(ta$j_margin, ta$perc)

model <- lm(u ~ j, data = ta)

resids <- all |> 
  filter(!is.na(j_margin)) |> 
  bind_cols(resid = model$residuals) |> 
  bind_cols(est = model$fitted.values)

resids |> 
  ggplot(aes(fill = resid)) +
  geom_sf() +
  scale_fill_continuous_diverging()


# JILL's margin

leaflet(all) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 2,
              fillColor = ~pal(j_margin), 
              fillOpacity = 1, 
              color = "white",
              popup = glue("<strong>Ward {all$ward}</strong>",
                           "<br>Eckblad Margin: {label_percent(.1)(all$j_margin)}",
                           "<br>Jack: {all$jack_eckblad}",
                           "<br>Ron: {all$ron_jansen}",
                           "<br>Underly Margin: {label_percent(.1)(all$perc)}"),
              popupOptions = popupOptions(autoPan = TRUE),
              highlight = highlightOptions(color = "black", fillColor = "white",
                                           fillOpacity = .25, bringToFront = TRUE)) |> 
  addLegend(pal = pal, values = 11, position = "topright", title = "Eckblad Share")


# with population census

w_demo <- read_csv("data/reporting-unit-demographics-2020census.csv")

these <- left_join(
  all |> 
    mutate(reporting_unit = str_to_upper(original_ward) |> 
             str_trim()),
  w_demo
)

