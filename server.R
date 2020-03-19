server = function(input,output,session){



  output$map <- renderLeaflet({


    # colors <- viridis::viridis(n = 256, option = 'viridis')

    pal <- leaflet::colorNumeric(
      palette = 'viridis',
      domain = case_locations$count,
      na.color = NA,
      reverse = TRUE
    )

    case_locations_temp = case_locations %>%
      dplyr::mutate(count = ifelse(count == 0, NA, count))


    label_content <- paste0("<strong>",case_locations_temp$REGC2017_NAME,"</strong><br>",
                            dplyr::coalesce(case_locations_temp$count,0), " cases")

    map = leaflet::leaflet(case_locations_temp) %>%
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



  # observeEvent(input$map_shape_click,{
  #
  #   browser()
  #
  # tb = lapply(1:nrow(case_locations_temp), function(r){
  #   print(r)
  #   region_name = case_locations_temp$REGC2017_NAME[r]
  #   regional_cases = clean_cases %>%
  #     dplyr::left_join(mapping_loc, by = c('Location' = 'moh_loc')) %>%
  #     dplyr::filter(map_loc == region_name) %>%
  #     dplyr::mutate(Location = map_loc) %>%
  #     dplyr::select(-map_loc)
  #
  #
  #
  #   if(nrow(regional_cases) == 0){
  #
  #     dates =seq(first_data_point, Sys.Date(), by='1 day')
  #     df = data.frame(x = dates, y = rep(0, length(dates)))
  #
  #     h = highchart() %>%
  #       hc_add_series(df, "line", hcaes(x = x,y=y), name = "Cumulative Cases") %>%
  #       hc_add_series(df, "column", hcaes(x = x,y=y), name = "# of Cases") %>%
  #       hc_add_theme(hc_theme_smpl()) %>%
  #       hc_xAxis(labels = list(format = '{value:%b %d}')) %>%
  #       hc_yAxis(allowDecimals = FALSE) %>%
  #       hc_title(
  #         text = paste("Covid-19 cases over time in", region_name)
  #       ) %>%
  #       hc_tooltip(crosshairs = TRUE, shared = TRUE,
  #                  headerFormat = "<b>{point.x:%B %d}</b><br />"
  #                  # pointFormat = "x: {point.x:%B:%d} <br> y: {point.y}"
  #       )
  #
  #   } else {
  #
  #     regional_dated_cases = regional_cases%>%
  #       dplyr::arrange(date_of_arrival) %>%
  #       tidyr::replace_na(list(date_of_arrival = as.Date(last_updated))) %>%
  #       dplyr::mutate(person = 1) %>%
  #       dplyr::group_by(date_of_arrival) %>%
  #       dplyr::summarise(persons = sum(person)) %>%
  #       tidyr::complete(date_of_arrival = seq(date_of_arrival[1], Sys.Date(), by = "1 day")) %>%
  #       tidyr::replace_na(list(persons = 0)) %>%
  #       dplyr::mutate(cum_count = cumsum(persons)) %>%
  #       tidyr::fill(date_of_arrival)
  #
  #
  #     h = highchart() %>%
  #       hc_add_series(regional_dated_cases, "line", hcaes(x = date_of_arrival , y = cum_count), name = "Cumulative Cases") %>%
  #       hc_add_series(regional_dated_cases, "column", hcaes(x = date_of_arrival , y = persons), name = "# of Cases") %>%
  #       hc_add_theme(hc_theme_smpl()) %>%
  #       hc_xAxis(labels = list(format = '{value:%b %d}')) %>%
  #       hc_yAxis(allowDecimals = FALSE) %>%
  #       hc_title(
  #         text = paste("Covid-19 cases over time in", region_name)
  #       ) %>%
  #       hc_tooltip(crosshairs = TRUE, shared = TRUE,
  #                  headerFormat = "<b>{point.x:%B %d}</b><br />"
  #                  # pointFormat = "x: {point.x:%B:%d} <br> y: {point.y}"
  #       )
  #   }
  #   h
  # })
  #
  # })




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
