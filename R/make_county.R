library(tidyverse)
library(tigris)
library(rnaturalearth)
library(sf)
library(glue)

d <- read_csv("data/votes.csv")

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
  city = str_remove_all(Ward, "City|Village|of|\\d|Ward|Wards|V\\.|C\\.|,|-|\\.") |> 
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
           combo)
})


to_combine <- full_pads |> 
  filter(combo) |> 
  select(city, wards)
  
  
wards <- tigris::voting_districts(state = "WI", county = "Milwaukee",
                                  year = 2024) |>
  mutate(ward = str_extract(NAME20, "\\d*$"),) |>
  select(w = NAME20, ward, geometry) |> 
  mutate(w = str_remove_all(w, "\\."),
         ind = row_number())

combo_wards <- map_df(1:nrow(to_combine), function(i) {
  groups <- to_combine[i,]
  get_these <- str_split_1(groups$wards, ", ")
  
  to_group <- wards |> 
    filter(str_detect(w, groups$city) &
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

updated_wards <- wards[-inds_of_grouped,] |> 
  transmute(city = str_remove(w, ".{5}$"),
            new_lab = w,
            geometry) |> 
  bind_rows(slim_combo) |> 
  select(-city) |> 
  group_by(new_lab) |> 
  summarise()


all <- full_pads |> 
  select(3:last_col()) |> 
  left_join(updated_wards) |> 
  st_as_sf()

l <- ne_download(type = "lakes", category = "physical", scale = "large")  %>%
  st_as_sf(., crs = st_crs(states))

lakes <- c("Lake Erie",
           "Lake Michigan",
           "Lake Superior",
           "Lake Huron",
           "Lake Ontario")
gl <- l %>%
  filter(name %in% lakes) %>%
  st_transform(crs = st_crs(all)) |> 
  st_union()


all_skinny <- st_difference(all, gl)


all_skinny |> 
  pivot_longer(cols = c(1:4), names_to = "candidate", values_to = "votes") |> 
  group_by(new_lab) |> 
  mutate(perc = votes / sum(votes)) |> 
  filter(perc == max(perc)) |> 
  ggplot(aes(fill = candidate)) +
  geom_sf()


