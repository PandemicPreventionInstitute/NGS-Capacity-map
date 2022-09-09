#### Creates data files on daily cases & deaths for 26 countries ####
#### Jordan Klein

#### 1) Setup ####
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
library(stringi)
library(data.table)
library(fuzzyjoin)
library(stringdist)
library(sf)
library(choroplethrAdmin1)
library(httr)
library(XML)
library(rvest)
library(mapcan)

#### Clear environment
rm(list = ls())
gc()

##### Load data basemap data
#### Basemap files
### Data linking file
keys <- read_csv("../../../data/raw/basemap/keys.csv")

### Geospatial files
## Countries
countries <- st_read("../../../data/raw/basemap/countries.geojson")
## ADM1
adm1 <- st_read("../../../data/raw/basemap/adm1.geojson")

#### 2) Load JHU data ####
#### JHU CSSE COVID-19 daily reports
# Get list of files
JHU_files <- list.files("../../../JHU-CSSE-COVID-19/csse_covid_19_data/csse_covid_19_daily_reports") %>% 
    .[grepl(".csv", .)]
## Import data
JHU_data_list <- lapply(JHU_files, function(filename) {
    paste("../../../JHU-CSSE-COVID-19/csse_covid_19_data/csse_covid_19_daily_reports", filename, sep = "/") %>% 
        read_csv()
})

#### Timeseries data
### Cases
## Global
global_cases_ts <- read_csv("../../../JHU-CSSE-COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") 
## Global
us_cases_ts <- read_csv("../../../JHU-CSSE-COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") 

### Deaths
## Global
global_deaths_ts <- read_csv("../../../JHU-CSSE-COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") 
## Global
us_deaths_ts <- read_csv("../../../JHU-CSSE-COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") 

#### 3) Create country level dataset ####
#### Create a clean global timeseries datafile
global_ts <- pivot_longer(global_cases_ts, cols = `1/22/20`:colnames(global_cases_ts)[ncol(global_cases_ts)], 
                          names_to = "date", values_to = "cases") %>% 
    left_join(pivot_longer(global_deaths_ts, cols = `1/22/20`:colnames(global_deaths_ts)[ncol(global_deaths_ts)], 
                           names_to = "date", values_to = "deaths")) %>% 
    # Keep countries of interest
    filter(countrycode(`Country/Region`, "country.name", "iso3c") %in% keys$iso3) %>% 
    mutate(date = mdy(date), iso3 = countrycode(`Country/Region`, "country.name", "iso3c")) %>% 
    # Drop DNK, NLD, GBR overseas possessions
    filter(!grepl("DNK|NLD|GBR", iso3) | is.na(`Province/State`))

#### Count daily cases & deaths by country
countries_epi <- group_by(global_ts, iso3, date) %>% 
    summarise(cases = sum(cases), deaths = sum(deaths))

#### 4) Clean JHU daily report data ####
#### Clean & standardize variable names 
JHU_data_list <- lapply(JHU_data_list, function(data) {
    # change names
    names(data) <- str_to_lower(names(data)) %>% 
        str_replace_all("[:punct:]| ", "_")
    # standardize data types
    data <- mutate(data, last_update = ymd(last_update))
    
    return(data)
})
### Name data by date
names(JHU_data_list) <- str_remove_all(JHU_files, ".csv")

#### Combine list into a single tibble
JHU_data <- bind_rows(JHU_data_list, .id = "date")

## Get country codes & keep countries of interest
JHU_data_clean <- mutate(JHU_data, iso3 = countrycode(country_region, "country.name", "iso3c")) %>% 
    mutate(province_state = if_else(country_region == "North Ireland", "Northern Ireland", province_state), 
           iso3 = if_else(country_region == "North Ireland", "GBR", iso3)) %>% 
    filter(iso3 %in% keys$iso3) %>% 
    ## Drop all diamond princess cases & repatriated/quarantined/recovered travelers
    filter(!grepl("diamond|princess|repatriated|quarantine|recover|evacuee", province_state, ignore.case = T))

## Keep needed variables
JHU_data_clean <- select(JHU_data_clean, iso3, province_state, lat, long_, latitude, longitude, date, confirmed, deaths) %>% 
    mutate(latitude = case_when(!is.na(latitude) ~ latitude, !is.na(lat) ~ lat), 
           longitude = case_when(!is.na(longitude) ~ longitude, !is.na(long_) ~ long_)) %>% 
    select(-lat, -long_) %>% 
    mutate(date = mdy(date))

#### 5) Identify admin divisions in the data ####
#### Get unique locations in the data 
Locations <- select(JHU_data_clean, iso3, province_state, latitude, longitude) %>% unique()

#### ID observations that only have country level data
Locations <- mutate(Locations, level = ifelse(is.na(province_state) | province_state == "Unknown" | 
                                                  (countrycode(province_state, "country.name", "iso3c") == iso3 & 
                                                  iso3 != "AUS"), 
                                                        "adm0", NA_character_))

#### Try to direct name match with adm1 file
Locations <- st_drop_geometry(adm1) %>% 
    select(iso3, province_state = adm1_name, adm1_id) %>% 
    left_join(Locations, .)

#### Name match with all caps & cleaned names
## Create all-caps & clean version of name
Locations <- mutate(Locations, adm1_name = str_to_upper(province_state) %>% 
                                 str_replace_all("&", "AND") %>% 
                                 str_replace_all("-|–|_", " ") %>% 
                                 str_remove_all("[[:punct:]]") %>% 
                                 str_remove_all("[[:symbol:]]") %>% 
                                 stri_trans_general(id = "Latin-ASCII") %>%
                                 str_squish())
## Match w/ adm1
Locations <- st_drop_geometry(adm1) %>% 
    select(iso3, adm1_name, adm1_id) %>% 
    mutate(adm1_name = str_to_upper(adm1_name) %>% 
               str_replace_all("&", "AND") %>% 
               str_replace_all("-|–|_", " ") %>% 
               str_remove_all("[[:punct:]]") %>% 
               str_remove_all("[[:symbol:]]") %>% 
               stri_trans_general(id = "Latin-ASCII") %>%
               str_squish()) %>% 
    left_join(select(filter(Locations, is.na(level)), -adm1_id), .) %>% 
    bind_rows(filter(Locations, !is.na(level))) %>% 
    mutate(level = if_else(!is.na(adm1_id), "adm1", level))

#### Name match with adm level 2 ids
Locations <- select(keys, iso3, province_state = adm2_id, adm1_id) %>% 
    left_join(select(filter(Locations, is.na(level)), -adm1_id), .) %>%
    mutate(level = if_else(!is.na(adm1_id), "adm2", level)) %>%
    bind_rows(filter(Locations, !is.na(level)))

#### Match canadian provinces based on codes
Locations <- provinces_territories %>% select(province_state = pr_english, adm1_id = pr_alpha) %>% 
    unique() %>% 
    left_join(select(filter(Locations, is.na(level)), -adm1_id), .) %>%
    mutate(level = if_else(!is.na(adm1_id), "adm1", level)) %>%
    bind_rows(filter(Locations, !is.na(level)))

#### Remaining AUS, DNK, NLD, GBR unmatched + US island territories = overseas terrotories
Locations <- mutate(Locations, level = if_else((grepl("AUS|DNK|NLD|GBR", iso3) | 
                                                    grepl("mariana|virgin is", province_state, ignore.case = T)) 
                                               & is.na(level), 
                                       "OT", level))

#### Extract states from US counties
Locations <- filter(Locations, iso3 == "USA" & is.na(level)) %>% 
    mutate(state.abb = str_remove(province_state, "^.*, ")) %>% 
    select(-adm1_id) %>%
    left_join(tibble(state.abb, state.name)) %>% 
    left_join(st_drop_geometry(mutate(select(adm1, iso3, state.name = adm1_name, adm1_id), 
                                      state.name = str_to_title(state.name)))) %>% 
    select(iso3:adm1_name, adm1_id) %>% 
    mutate(level = if_else(!is.na(adm1_id), "adm1", level)) %>%
    bind_rows(filter(Locations, !is.na(level) | iso3 != "USA"))
   
#### 6) Find remaining unmatched with geolocations ####
#### Unmatched locations that have coords
unmatched_geo <- filter(Locations, is.na(level) & !is.na(latitude)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") %>% 
    select(-adm1_id)

#### Obtain matches- polygon contains coordinates
Geo.matches <- mapply(function(points, polygons) {
    rownames(polygons) <- seq(1:nrow(polygons))
    
    # Find matches
    matches <- st_contains(polygons, points)
    
    # Join matching locations to adm1_id's
    locations_matched <- sapply(t(matches), function(x) {
        st_drop_geometry(polygons) %>%
            select(adm1_id) %>%
            filter(rownames(.) %in% x)
    }) %>%
        sapply(function(x) if(identical(x, character(0))) NA_character_ else x) %>%
        unlist() %>%
        tibble(points, adm1_id = .)
    
    return(locations_matched)
}, points = split(unmatched_geo, unmatched_geo$iso3), 
polygons = split(adm1, adm1$iso3)[grepl(paste(unique(unmatched_geo$iso3), 
                                              collapse = "|"), 
                                        names(split(adm1, adm1$iso3)))], SIMPLIFY = F) %>% 
    bind_rows() %>% 
    st_as_sf()

#### Get matches w/in a certain distance
Geo.windist <- mapply(function(points, polygons) {
    rownames(polygons) <- seq(1:nrow(polygons))
    
    # Find matches
    matches <-  st_is_within_distance(polygons, points, dist = 10000)
    
    # Join matching locations to adm1_id's
    locations_matched <- sapply(t(matches), function(x) {
        st_drop_geometry(polygons) %>%
            select(adm1_id) %>%
            filter(rownames(.) %in% x)
    }) %>%
        sapply(function(x) if(identical(x, character(0))) NA_character_ else x) %>%
        unlist() %>%
        tibble(points, adm1_id = .)
    
    return(locations_matched)
}, points = filter(Geo.matches, is.na(adm1_id)) %>%
    select(-adm1_id) %>%
    split(.$iso3), 
polygons = split(adm1, adm1$iso3)[grepl(paste(unique(select(filter(Geo.matches, is.na(adm1_id)), -adm1_id)$iso3), 
                                              collapse = "|"), 
                                        names(split(adm1, adm1$iso3)))], SIMPLIFY = F) %>% 
    bind_rows() %>% 
    st_as_sf()

### Add back into main data
Locations <- filter(Geo.matches, !is.na(adm1_id)) %>% bind_rows(Geo.windist) %>% 
    mutate(level = if_else(!is.na(adm1_id), "adm1", level)) %>% 
    st_drop_geometry() %>% 
    bind_rows(filter(Locations, !is.na(level) | is.na(latitude)))

#### Manually assign level 1 admin divisions to the rest
Locations <- mutate(Locations, adm1_id = 
                        case_when(is.na(level) & grepl("Kagoshima|Okinawa", province_state) ~ "Kyushu", 
                                  is.na(level) & grepl("Chicago", province_state) ~ "ILLINOIS", 
                                  is.na(level) & grepl("Kuala Lumpur", province_state) ~ "W_P__KUALA_LUMPUR", 
                                  is.na(level) & grepl("Labuan", province_state) ~ "W_P__LABUAN",
                                  is.na(level) & grepl("Putrajaya", province_state) ~ "W_P__PUTRAJAYA", 
                                  is.na(level) & grepl("Bavaria", province_state) ~ "Bayern", 
                                  is.na(level) & grepl(", ON", province_state) ~ "ON", 
                                  is.na(level) & grepl(", QC", province_state) ~ "QC", 
                                  !is.na(level) ~ adm1_id))
## Give remaining observations missing a level adm1
Locations <- mutate(Locations, level = if_else(is.na(level), "adm1", level))

#### 7) Combine coded locations back into time series data & get aggregated adm1 level data ####
#### Get coded JHU data
JHU_data_coded <- select(Locations, iso3, province_state, adm1_id, level) %>% unique() %>% 
    left_join(select(JHU_data_clean, -latitude, -longitude), .) %>% 
    mutate(level = if_else(level == "adm2", "adm1", level)) %>% 
    mutate(level = if_else(iso3 == "USA" & grepl("AMERICAN_SAMOA|GUAM|PUERTO_RICO|UNITED_STATES_VIRGIN_ISLANDS", adm1_id), 
                           "OT", level)) %>%
    filter(level != "OT") %>% 
    select(iso3, adm1_id, level, date, confirmed, deaths)

#### Adm 1 level data
adm1_epi <- filter(JHU_data_coded, level == "adm1") %>% 
    group_by(iso3, adm1_id, date) %>% 
    summarise(cases = sum(confirmed, na.rm = T), deaths = sum(deaths, na.rm = T))

#### 8) Export data ####
# Country level
write_csv(countries_epi, "../../../data/raw/basemap/epi_countries.csv")
# Admin 1 level
write_csv(adm1_epi, "../../../data/raw/basemap/epi_adm1.csv")
