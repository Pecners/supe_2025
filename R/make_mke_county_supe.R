library(tidyverse)
library(tigris)
library(rnaturalearth)
library(sf)
library(glue)

vf <- "data/dpi_supe_results_2025.csv"

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


to_combine <- full_pads |> 
  filter(combo) |> 
  select(city, wards, original_ward)
  
  
# wards <- tigris::voting_districts(state = "WI", county = "Milwaukee",
#                                   year = 2024) |>
#   mutate(ward = str_extract(NAME20, "\\d*$"),) |>
#   select(w = NAME20, ward, geometry) |>
#   mutate(w = str_remove_all(w, "\\."),
#          ind = row_number())

wards <- st_read("data/wi_municipal_wards/WI_Municipal_Wards_(July_2024).shp")
# mke_wards <- wards |> filter(CNTY_NAME == "Milwaukee")
wards <- wards |> 
  mutate(ward = str_extract(LABEL, "\\d*$"),) |>
  select(w = LABEL,
         ward, geometry,
         county = CNTY_NAME,
         city = MCD_NAME) |>
  mutate(w = str_remove_all(w, "\\."),
         ind = row_number(),
         city = str_remove_all(city, "City|Village|of|\\d|Ward|Wards|V\\.|C\\.|,|-|\\.") |> 
           str_remove_all(" s") |> 
           str_trim(),
         city = ifelse(city == "So Milw", "South Milwaukee", city))

# wards |> 
#   filter(w %in% full_pads$new_lab &
#            county == "Milwaukee") |> 
#   ggplot() +
#   geom_sf(fill = "red")

combo_wards <- map_df(1:nrow(to_combine), function(i) {
  groups <- to_combine[i,]
  get_these <- str_split_1(groups$wards, ", ")
  
  to_group <- wards |> 
    filter(city == groups$city &
             ward %in% get_these)
  
  # inds <- str_flatten_comma(to_group$ind)
  
  city <- str_remove(to_group[[1,1]], ".{5}$")
  geo <- st_union(to_group)
  tibble(city,
         new_lab = glue("{city} {groups$wards}"),
         geometry = geo,
         inds = to_group$ind)
})

inds_of_grouped <- combo_wards |> 
  pull(inds)

slim_combo <- combo_wards |> 
  select(-inds) |> 
  unique()

# slim_combo |> 
#   st_as_sf() |> 
#   ggplot() +
#   geom_sf(fill = "red")


updated_wards <- wards[-inds_of_grouped,] |> 
  transmute(city = str_remove(w, ".{5}$"),
            new_lab = w,
            geometry) |> 
  bind_rows(slim_combo) |> 
  select(-city) |> 
  group_by(new_lab) |> 
  summarise()


all <- full_pads |> 
  select(2:last_col()) |> 
  left_join(updated_wards) |> 
  st_as_sf()

all |> 
  ggplot() +
  geom_sf(fill = "red")


all |> 
  pivot_longer(cols = c(1:3), names_to = "candidate", values_to = "votes") |> 
  group_by(new_lab) |> 
  mutate(perc = votes / sum(votes)) |> 
  filter(perc == max(perc)) |> 
  ggplot(aes(fill = candidate)) +
  geom_sf()

saveRDS(all, "data/cleaned_supe_race_data.rda")

