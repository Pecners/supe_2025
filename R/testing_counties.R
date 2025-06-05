f <- wards |> 
  mutate(co = ifelse(str_detect(w, "Fredonia"), "red", "blue")) |> 
  st_transform(crs = 4326)

leaflet(f) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 2,
              fillColor = f$co, 
              fillOpacity = .9, 
              color = "white",
              popup = glue("<strong>Ward {f$w}</strong>"),
              popupOptions = popupOptions(autoPan = TRUE),
              highlight = highlightOptions(color = "black", fillColor = "white",
                                           fillOpacity = .25, bringToFront = TRUE))


wider_n |> 
  filter(str_detect(original_ward, "City of Milwaukee")) |> 
  as_tibble() |> 
  summarise(b_total = sum(brad_schimel),
            s_total = sum(susan_crawford),
            wi_total = sum(write_in.y)) |> 
  mutate(perc = b_total / (b_total +
                             s_total +
                             wi_total),
         sperc = s_total / (b_total +
                             s_total +
                             wi_total))
