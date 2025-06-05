library(tigris)

mke <- places("wi") |> 
  filter(NAME == "Milwaukee")
mke |> 
  st_transform(crs = 4326) |> 
  saveRDS("data/mke.rda")

leaflet(wider) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 2,
              fillColor = wider$col, fillOpacity = .9,
              color = "white",
              popup = glue("<strong>Ward {wider$new_lab}</strong>",
                           "<br>Kinser: {label_percent(.1)(wider$brittany_kinser)}",
                           ", ({label_comma()(wider_n$brittany_kinser)})",
                           "<br>Underly: {label_percent(.1)(wider$jill_underly)}",
                           ", ({label_comma()(wider_n$jill_underly)})"),
              popupOptions = popupOptions(autoPan = TRUE),
              highlight = highlightOptions(color = "black", fillColor = "white",
                                           fillOpacity = .25, bringToFront = TRUE)) |> 
  addPolylines(data = mke,
              weight = 3,
              color = "black") |> 
  addLegend(colors = cols, opacity = 1, 
            labels = c("Kinser", "Underly"), 
            position = "topright")

sum(wider_n$brittany_kinser) / (sum(wider_n$brittany_kinser) + 
                                  sum(wider_n$jill_underly) +
                                  sum(wider_n$write_in.x))
