server = function(input,output,session){



  output$map <- renderLeaflet({


    # colors <- viridis::viridis(n = 256, option = 'viridis')

    pal <- leaflet::colorNumeric(
      palette = 'YlOrRd',
      domain = case_locations$count,
      na.color = NA,
      reverse = FALSE
    )

    case_locations_temp = case_locations %>%
      dplyr::mutate(count = ifelse(count == 0, NA, count))


    label_content <- paste0("<strong>",case_locations_temp$REGC2017_NAME,"</strong><br>",
                            dplyr::coalesce(case_locations_temp$count,0), " cases")

    map = leaflet::leaflet(case_locations_temp,
                           options = leafletOptions(zoomControl = FALSE,
                                                    minZoom = 5,
                                                    maxZoom = 8,
                                                    dragging = TRUE)) %>%
      leaflet::addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      leaflet::setView(
        lng = 175,
        lat = -41,
        zoom = 6)%>%
      leaflet::addPolygons(
        stroke = TRUE,
        weight = 0,
        color = "#00000000",
        fillColor = ~pal(count),
        fillOpacity = 0.7,
        label = lapply(label_content,htmltools::HTML)
        # popup = leafpop::popupGraph(tb)
      ) %>%
      addLegend(
        "bottomleft",
        pal = pal,
        values = ~count,
        title = "Number of Cases",
        opacity = 1,
        # labFormat = labelFormat(transform = function(x) {sort(x, decreasing = TRUE)}),
        className = "info legend maplegend"
      )



      map


  })



  observeEvent(input$map_shape_click,{


    showModal(modalDialog(
      highchartOutput('regional_growth'),

      size = 'm',
      easyclose = TRUE
    ))


  })

  output$regional_growth = renderHighchart({
    req(input$map_shape_click)
    clk = st_point(c(input$map_shape_click$lng, input$map_shape_click$lat))
    selection = as.numeric(sf::st_intersects(clk, nz_regions_simp))

    createRegionalHighchart(selection)
  })







  output$ncase_counts = renderHighchart({



     highcharter::highchart() %>%
      hc_add_series(dated_cases, "line", hcaes(x = date_of_arrival , y = cum_count), name = "Cumulative Cases") %>%
      hc_add_series(dated_cases, "column", hcaes(x = date_of_arrival , y = persons), name = "# of Cases") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_xAxis(labels = list(format = '{value:%b %d}')) %>%
      hc_yAxis(allowDecimals = FALSE) %>%
      hc_title(
        text = "Covid-19 cases over time"
      ) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE,
                 headerFormat = "<b>{point.x:%B %d}</b><br />"
                 # pointFormat = "x: {point.x:%B:%d} <br> y: {point.y}"
      )

  })




  output$age_counts = renderHighchart({



     highcharter::highchart() %>%
      hc_add_series(age_cases, "column", hcaes(x = Age , y = n), name = "# of Cases") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_xAxis(categories = age_cases$Age) %>%
      hc_yAxis(allowDecimals = FALSE) %>%
      hc_title(
        text = "Covid-19 cases by age"
      ) %>%
      hc_tooltip(crosshairs = TRUE) %>%
      hc_colors("#2980b9")

  })



}
