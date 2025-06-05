library(tidyverse)
library(tigris)
library(rnaturalearth)
library(sf)
library(glue)

vf <- "data/mps_ref_2024_results.csv"

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



wards <- st_read("data/wi_municipal_wards/WI_Municipal_Wards_(July_2024).shp")
# mke_wards <- wards |> filter(CNTY_NAME == "Milwaukee")
wards <- wards |> 
  mutate(ward = str_extract(LABEL, "\\d*$")) |>
  select(w = LABEL,
         ward, geometry,
         county = CNTY_NAME,
         city = MCD_NAME) |>
  filter(city == "Milwaukee") |> 
  mutate(w = case_when(
    w == "Milwaukee - C 0355" ~ "Milwaukee - C 0348", 
    w == "Milwaukee - C 0356" ~ "Milwaukee - C 0282",
    TRUE ~ w)) |> 
  group_by(w, ward, county, city) |> 
  summarise() |> 
  mutate(w = str_remove_all(w, "\\."),
         ind = row_number(),
         city = str_remove_all(city, "City|Village|of|\\d|Ward|Wards|V\\.|C\\.|,|-|\\.") |> 
           str_remove_all(" s") |> 
           str_trim(),
         city = ifelse(city == "So Milw", "South Milwaukee", city))





updated_wards <- wards |> 
  transmute(city = str_remove(w, ".{5}$"),
            new_lab = w,
            geometry) |> 
  select(-city)


all <- full_pads |> 
  select(2:last_col()) |> 
  rename(ref_yes = Yes,
         ref_no = No) |> 
  left_join(updated_wards) |> 
  st_as_sf() |> 
  st_transform(crs = 4326)

all |> 
  ggplot() +
  geom_sf(fill = "red")


saveRDS(all, "data/cleaned_ref_data.rda")

