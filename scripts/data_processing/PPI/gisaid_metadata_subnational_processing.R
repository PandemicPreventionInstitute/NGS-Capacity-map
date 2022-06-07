#### Process metadata from GISAID & aggregate by subnational administrative divisions ####
#### Jordan Klein

#### 1) Setup ####

#### Run GISAID metadata extracting script
source("auto_extract_gisaid_metadata.R")

#### Load packages
library(tidyverse) # data wrangling
library(tibble) # data wrangling
library(janitor) # column naming
library(countrycode) # country c?des
library(lubridate) # date times
library(readxl) # excel import
library(zoo) # calculate rolling averages
library(R.utils) # R utilities
library(stringr) # to parse strings in R
library(dplyr) # data wrangling
library(readr) # read_csv

#### Clear environment & load metadata
rm(list = ls())
Metadata_raw <- read_csv("../../../data/raw/metadata.csv") # from extracted datastream

#### 2) Get set of unique geolocations present in the metadata ####
#### Split location string into geographies at each level (up to 7)
Locations_split <-  str_split(Metadata_raw$location, "  |/", simplify = T) %>% 
    as_tibble() %>%
    mutate(across(everything(), ~str_trim(.))) %>%
    mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
colnames(Locations_split) <- paste0("geo_l", 1:7)

### Combine back with metadata
Metadata <- bind_cols(Metadata_raw, Locations_split)
# give each location a unique id
Metadata <-tibble(location = unique(Metadata$location), 
                  loc_id = seq(1, nrow(unique(select(Metadata, location))))) %>% 
    full_join(Metadata)
           
#### Get unique geography combinations
Geo_keys <- select(Metadata, loc_id, continent = geo_l1, country, geo_l2:geo_l7) %>% 
    unique()

### For cases where geography level 2 is a subdivisions rather than a country, shift all geographies down 1 level
Geo_keys[Geo_keys$country != Geo_keys$geo_l2 & !is.na(Geo_keys$country), paste0("geo_l", 3:7)] <- 
    Geo_keys[Geo_keys$country != Geo_keys$geo_l2 & !is.na(Geo_keys$country), paste0("geo_l", 2:6)]
# With country fixed, don't need geo level 2 anymore
Geo_keys <- select(Geo_keys, -geo_l2)

#### Export Geo keys
write_csv(Geo_keys, '../../../data/raw/geo_keys.csv')
