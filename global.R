
library("leaflet")
library("leafpop")
library("rlang")
library("vctrs")
library("highcharter")
library("RColorBrewer")
library("scales")
library("lattice")
library("dplyr")
library("shiny")
library("tidyr")
library("sf")
library("rvest")
library("readr")
library("lubridate")

# nz_regions = sf::st_read('nz-regions.gpkg', quiet = TRUE)
# nz_regions_simple = rmapshaper::ms_simplify(nz_regions, 0.001)
nz_regions_simp = sf::st_read('data/nz_regions_1pc.gpkg', quiet = TRUE, stringsAsFactors = FALSE)
nz_regions_simp$REGC2017_NAME = gsub(" Region", "", nz_regions_simp$REGC2017_NAME)


last_updated = lubridate::round_date(file.info('data/cleaned_cases.rds')$mtime, unit= 'hour')
first_data_point = as.Date('2020-02-23')



population_2018_census <- read_csv("data/population-2018-census.csv")


clean_cases = readRDS('data/cleaned_cases.rds')

mapping_loc = tibble::tribble(
  ~moh_loc          , ~map_loc
  , "Queenstown"    , "Otago"
  , "Rotorua"       , "Waikato"
  , "Dunedin"       , "Otago"
  , "Invercargill"  , "Southland"
  , "Southern DHB"  , "Southland"
  , "Auckland"      , "Auckland"
  , "Northland"     , "Northland"
  , "Taranaki"      , "Taranaki"
  , "Waikato"       , "Waikato"
  , "Canterbury"    , "Canterbury"
  , "Wellington"    , "Wellington"
  , "Hawkes Bay"    , "Hawke's Bay"
)


loc_cases = clean_cases %>%
  dplyr::left_join(mapping_loc, by = c('Location' = 'moh_loc')) %>%
  group_by(map_loc) %>%
  summarise(count = n())


case_locations = nz_regions_simp %>%
  left_join(population_2018_census, by = c("REGC2017_NAME" = 'Region')) %>%
  left_join(loc_cases, by = c("REGC2017_NAME" = 'map_loc')) %>%
  tidyr::replace_na(list(count = 0))




dated_cases = clean_cases %>%
  dplyr::arrange(date_of_arrival) %>%
  tidyr::replace_na(list(date_of_arrival = as.Date(last_updated))) %>%
  dplyr::mutate(person = 1) %>%
  dplyr::group_by(date_of_arrival) %>%
  dplyr::summarise(persons = sum(person)) %>%
  tidyr::complete(date_of_arrival = seq(date_of_arrival[1], Sys.Date(), by = "1 day")) %>%
  tidyr::replace_na(list(persons = 0)) %>%
  dplyr::mutate(cum_count = cumsum(persons)) %>%
  tidyr::fill(date_of_arrival)


age_cases = clean_cases %>%
  dplyr::group_by(Age) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(Age = dplyr::case_when(Age == "Teens" ~ "10s",
                                       Age == "" ~ "Unknown",
                                       TRUE ~ Age)) %>%
  arrange(Age)


source('func.R')
