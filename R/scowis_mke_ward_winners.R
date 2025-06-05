mke <- wider_n |> 
  filter(city == "Milwaukee")
  
leaflet(mke) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 2,
              fillColor = mke$sc_col, fillOpacity = .9,
              color = "white",
              popup = glue("<strong>Ward {mke$new_lab}</strong>",
                           "<br>Crawford: {label_comma()(mke$susan_crawford)}",
                           "<br>Schimel: {label_comma()(mke$brad_schimel)}"),
              popupOptions = popupOptions(autoPan = TRUE),
              highlight = highlightOptions(color = "black", fillColor = "white",
                                           fillOpacity = .25, bringToFront = TRUE)) |> 
  addLegend(colors = cols, opacity = 1, 
            labels = c("Crawford", "Schimel"), 
            position = "topright")


wider_n |> 
  filter(city == "Milwaukee") |> 
  ggplot() +
  geom_sf(fill = alpha("blue", .25))
