this <- wider_n |> 
  filter(city == "Milwaukee")

range(this$kinser_metric, na.rm = TRUE)

pa <- sequential_hcl(5, "YlOrRd")
pal <- colorBin(rev(pa), domain = c(0, .5), bins = 5)

leaflet(this) |>
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
