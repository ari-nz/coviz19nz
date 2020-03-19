server = function(input,output,session){



  output$map <- renderLeaflet({

    pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain = case_locations$count,
      na.color = NA,
      reverse = TRUE
    )

    case_locations_temp = case_locations %>%
      dplyr::mutate(count = ifelse(count == 0, NA, count))

    leaflet::leaflet(case_locations_temp) %>%
      leaflet::addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      leaflet::setView(
        lng = 175,
        lat = -41,
        zoom = 6)%>%
      leaflet::addPolygons(
        stroke = FALSE,
        color = "#00000000",
        fillColor = ~pal(count),
        fillOpacity = 0.7
      ) %>%
      addLegend(
        "bottomleft",
        pal = pal,
        values = ~count,
        title = "Number of Cases",
        opacity = 1
      )

  })





}
