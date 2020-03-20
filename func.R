dataModal <- function(region = NULL, failed = FALSE) {


}




createRegionalHighchart<-function(regionId){

  region_name = case_locations$REGC2017_NAME[regionId]
  regional_cases = clean_cases %>%
    dplyr::left_join(mapping_loc, by = c('Location' = 'moh_loc')) %>%
    dplyr::filter(map_loc == region_name) %>%
    dplyr::mutate(Location = map_loc) %>%
    dplyr::select(-map_loc)



  if(nrow(regional_cases) == 0){

    dates =seq(first_data_point, Sys.Date(), by='1 day')
    df = data.frame(x = dates, y = rep(0, length(dates)))

    h = highchart() %>%
      hc_add_series(df, "line", hcaes(x = x,y=y), name = "Cumulative Cases") %>%
      hc_add_series(df, "column", hcaes(x = x,y=y), name = "# of Cases") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_xAxis(labels = list(format = '{value:%b %d}')) %>%
      hc_yAxis(allowDecimals = FALSE) %>%
      hc_title(
        text = paste("Covid-19 cases over time in", region_name)
      ) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE,
                 headerFormat = "<b>{point.x:%B %d}</b><br />"
                 # pointFormat = "x: {point.x:%B:%d} <br> y: {point.y}"
      )

  } else {

    regional_dated_cases = regional_cases%>%
      dplyr::arrange(date_of_arrival) %>%
      tidyr::replace_na(list(date_of_arrival = as.Date(last_updated))) %>%
      dplyr::mutate(person = 1) %>%
      dplyr::group_by(date_of_arrival) %>%
      dplyr::summarise(persons = sum(person)) %>%
      tidyr::complete(date_of_arrival = seq(date_of_arrival[1], Sys.Date(), by = "1 day")) %>%
      tidyr::replace_na(list(persons = 0)) %>%
      dplyr::mutate(cum_count = cumsum(persons)) %>%
      tidyr::fill(date_of_arrival)


    h = highchart() %>%
      hc_add_series(regional_dated_cases, "line", hcaes(x = date_of_arrival , y = cum_count), name = "Cumulative Cases") %>%
      hc_add_series(regional_dated_cases, "column", hcaes(x = date_of_arrival , y = persons), name = "# of Cases") %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_xAxis(labels = list(format = '{value:%b %d}')) %>%
      hc_yAxis(allowDecimals = FALSE) %>%
      hc_title(
        text = paste("Covid-19 cases over time in", region_name)
      ) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE,
                 headerFormat = "<b>{point.x:%B %d}</b><br />"
                 # pointFormat = "x: {point.x:%B:%d} <br> y: {point.y}"
      )
  }
  h
}
