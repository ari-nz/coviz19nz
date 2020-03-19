# nz_regions = sf::st_read('nz-regions.gpkg', quiet = TRUE)
# nz_regions_simple = rmapshaper::ms_simplify(nz_regions, 0.001)
nz_regions_simp = sf::st_read('nz_regions_1pc.gpkg', quiet = TRUE, stringsAsFactors = FALSE)
nz_regions_simp$REGC2017_NAME = gsub(" Region", "", nz_regions_simp$REGC2017_NAME)







url <- "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases"
cases <- url %>%
  xml2::read_html() %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE) %>%
  .[[1]]


clean_cases = cases %>%
  dplyr::mutate(date_of_arrival = stringr::str_extract(`Travel details`, "\\d{1,2}(th)? (January|February|March|April|May|June|July|August|September|October|November|December)")) %>%
  dplyr::mutate(date_of_arrival = gsub("th", "", date_of_arrival)) %>%
  dplyr::mutate(date_of_arrival = paste0(date_of_arrival, " 2020")) %>%
  dplyr::mutate(date_of_arrival = lubridate::dmy(temp$date_of_arrival))

saveRDS(clean_cases, 'data/cleaned_cases.rds')

