this <- wider_n |> 
  filter(perc >= .75 & u_marg >= 375) 

sum(this$u_marg) / sum(wider_n$u_marg)

nrow(this) / nrow(wider_n)

range(this$perc)

pa <- sequential_hcl(5, "YlOrRd")
pal <- colorBin(rev(pa), domain = c(0, 1))

leaflet(this) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 2,
              fillColor = ~pal(perc), 
              fillOpacity = .9, 
              color = "white",
              popup = glue("<strong>Ward: {wider_n$new_lab}</strong>",
                           "<br>Crawford: {wider_n$susan_crawford}",
                           "<br>Underly: {wider_n$jill_underly}",
                           "<br>Gap: {wider_n$under_under}"),
              popupOptions = popupOptions(autoPan = TRUE),
              highlight = highlightOptions(color = "black", fillColor = "white",
                                           fillOpacity = .25, bringToFront = TRUE)) |> 
  addLegend(pal = pal, values = ~n, position = "topright", title = "Underly Margin")


range(wider_n$kinser_metric, na.rm = TRUE)

pa <- sequential_hcl(5, "YlOrRd")
pal <- colorBin(rev(pa), domain = c(-.1, .51), bins = 3)

leaflet(wider_n) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 2,
              fillColor = ~pal(kinser_metric), 
              fillOpacity = .9, 
              color = "white",
              popup = glue("<strong>Ward {wider_n$ward}</strong>",
                           "<br>Kinser: {wider_n$brittany_kinser}",
                           "<br>Underly: {wider_n$jill_underly}",
                           "<br>Schimel: {wider_n$brad_schimel}",
                           "<br>Crawford: {wider_n$susan_crawford}",
                           "<br>Crawford-Kinser: {wider_n$kinser_over}"),
              popupOptions = popupOptions(autoPan = TRUE),
              highlight = highlightOptions(color = "black", fillColor = "white",
                                           fillOpacity = .25, bringToFront = TRUE)) |> 
  addLegend(pal = pal, values = ~n, position = "topright", title = "Crawford-Kinser Share")



leaflet(wards) |>
  addMapboxTiles(style_id = "light-v11",
                 username = "mapbox",
                 scaling_factor = "0.5x") |>
  addPolygons(weight = 2,
              popup = glue("<strong>Ward {wards$w}</strong>"))
