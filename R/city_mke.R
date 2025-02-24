library(tidyverse)
library(sf)
library(RColorBrewer)
library(colorspace)
library(leaflet)
library(mapboxapi)
library(mapboxer)
library(glue)
library(scales)

cols <- c(
  "#1F77B4",
  "#FF7F0E",
  "#2CA02C"
)

d <- read_csv("data/votes.csv") |> 
  mutate(ward = str_remove(Ward, "City of Milwaukee Ward ") |> 
           str_trim()) |> 
  pivot_longer(cols = c(3:6), names_to = "candidate", values_to = "votes") |> 
  select(-c(id, Ward)) |> 
  group_by(ward) |> 
  mutate(perc = votes / sum(votes),
         col = case_when(
           candidate == "Jeff Wright" ~ cols[1],
           candidate == "Brittany Kinser" ~ cols[2],
           candidate == "Jill Underly" ~ cols[3]
         ))

w <- st_read("data/ward/ward.shp", quiet = TRUE) |> 
  transmute(ward = WARD,
            geometry)


all <- left_join(w, d) |>
  st_transform(crs = 4326)

wins <- all |> 
  as_tibble() |> 
  group_by(ward) |> 
  filter(perc == max(perc)) |> 
  select(ward, 
         winner = candidate,
         perc,
         col)

wider <- all |> 
  select(-c(votes, col)) |> 
  pivot_wider(names_from = candidate, values_from = perc) |> 
  left_join(wins)


leaflet(wider) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 2,
              fillColor = wider$col, fillOpacity = .9,
              color = "white",
              popup = glue("<strong>Ward {wider$ward}</strong>",
                           "<br>Wright: {label_percent(.1)(wider$`Jeff Wright`)}",
                           "<br>Kinser: {label_percent(.1)(wider$`Brittany Kinser`)}",
                           "<br>Underly: {label_percent(.1)(wider$`Jill Underly`)}"),
              popupOptions = popupOptions(autoPan = TRUE),
              highlight = highlightOptions(color = "black", fillColor = "white",
                                           fillOpacity = .25, bringToFront = TRUE)) |> 
  addLegend(colors = cols, opacity = 1, 
            labels = c("Wright", "Kinser", "Underly"), 
            position = "topright")

  