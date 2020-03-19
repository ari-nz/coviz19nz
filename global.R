
library(leaflet)
library(highcharter)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(sf)
library("rvest")
library(readr)

# nz_regions = sf::st_read('nz-regions.gpkg', quiet = TRUE)
# nz_regions_simple = rmapshaper::ms_simplify(nz_regions, 0.001)
nz_regions_simp = sf::st_read('data/nz_regions_1pc.gpkg', quiet = TRUE, stringsAsFactors = FALSE)
nz_regions_simp$REGC2017_NAME = gsub(" Region", "", nz_regions_simp$REGC2017_NAME)


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
)


loc_cases = clean_cases %>%
  dplyr::left_join(mapping_loc, by = c('Location' = 'moh_loc')) %>%
  group_by(map_loc) %>%
  summarise(count = n())


case_locations = nz_regions_simp %>%
  left_join(population_2018_census, by = c("REGC2017_NAME" = 'Region')) %>%
  left_join(loc_cases, by = c("REGC2017_NAME" = 'map_loc')) %>%
  tidyr::replace_na(list(count = 0))








# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)
