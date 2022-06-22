#### Process & clean unique set of geokeys from gisaid metadata ####
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

#### Clear environment & load data
rm(list = ls())
Geo_keys <- read_csv("../../../data/raw/geo_keys.csv", col_types = "dccccccc")

#### 2) Clean geo keys ####
#### Unescape unicode
Geo_keys_clean <- Geo_keys %>% 
    mutate(across(geo_l3:geo_l7, ~stri_unescape_unicode(.)))
## Check to make sure all UTF-8
select(Geo_keys_clean, geo_l3:geo_l7) %>% 
    sapply(., function(x) {
        stri_enc_isutf8(x) %>% 
            table()
    })

### Add country isocode
Geo_keys_clean <- Geo_keys_clean %>% 
    add_column(country.isocode = countrycode(.$country, "country.name", "iso2c"), .after = "country")
## Manually add codes for countries that didn't match (https://laendercode.net/en/2-letter-list.html)
Geo_keys_clean <- Geo_keys_clean %>% 
    mutate(country.isocode = case_when(grepl("Kosovo", country) ~ "XK", 
                                       grepl("Saint Martin", country) ~ "MF", 
                                       grepl("Bonaire|Sint Eustatius", country) ~ "BQ", 
                                       !grepl("Kosovo|Saint Martin|Bonaire|Sint Eustatius", country) ~ country.isocode))
# ## Set French island country codes to france
# Geo_keys_clean <- mutate(Geo_keys_clean, 
#                          geo_l4 = if_else(grepl("BL|MF|GP", country.isocode), geo_l3, geo_l4), 
#                          geo_l3 = if_else(grepl("BL|MF|GP", country.isocode), country, geo_l3), 
#                          country = if_else(grepl("BL|MF|GP", country.isocode), "France", country), 
#                          country.isocode = if_else(grepl("BL|MF|GP", country.isocode), "FR", country.isocode))

#### Convert subnational geolocations to upper case & remove punctuation
Geo_keys_clean <- Geo_keys_clean %>% 
    mutate(across(geo_l3:geo_l7, ~str_to_upper(.) %>% 
                      str_replace_all("&", "AND") %>% 
                      str_replace_all("-|–|_", " ") %>% 
                      str_remove_all("[[:punct:]]") %>% 
                      str_remove_all("[[:symbol:]]") %>% 
                      stri_trans_general(id = "Latin-ASCII") %>%
                      str_squish())) %>% 
    mutate(across(geo_l3:geo_l7, ~ifelse(. == "", NA, .)))

#### If country = subnational division, clear subnational division
Geo_keys_clean <- mutate(Geo_keys_clean, 
                         across(geo_l3:geo_l7, ~ifelse(str_to_upper(country) == ., NA, .)))

#### If a lower level division isn't blank but a higher level one is, move the location name up 1 level
Geo_keys_clean <- Geo_keys_clean %>% 
    mutate(geo_l3 = if_else(is.na(geo_l3) & !is.na(geo_l4), geo_l4, geo_l3)) %>% 
    mutate(geo_l4 = if_else(is.na(geo_l4) & !is.na(geo_l5), geo_l5, geo_l4)) %>% 
    mutate(geo_l5 = if_else(is.na(geo_l5) & !is.na(geo_l6), geo_l6, geo_l5)) %>% 
    mutate(geo_l6 = if_else(is.na(geo_l6) & !is.na(geo_l7), geo_l7, geo_l6))

### Create unique IDs for each location level
# 3
Geo_keys_clean <- select(Geo_keys_clean, country.isocode, geo_l3) %>% 
    filter(!is.na(country.isocode) & !is.na(geo_l3)) %>% 
    unique() %>% 
    mutate(geol3_id = seq(1, nrow(unique(filter(select(Geo_keys_clean, country.isocode, geo_l3), 
                                                !is.na(country.isocode) & !is.na(geo_l3)))))) %>% 
    full_join(Geo_keys_clean, .)
# 4
Geo_keys_clean <- select(Geo_keys_clean, country.isocode, geo_l3, geo_l4) %>% 
    filter(!is.na(country.isocode) & !is.na(geo_l3) & !is.na(geo_l4)) %>% 
    unique() %>% 
    mutate(geol4_id = seq(1, nrow(unique(filter(select(Geo_keys_clean, country.isocode, geo_l3, geo_l4), 
                                                !is.na(country.isocode) & !is.na(geo_l3) & !is.na(geo_l4)))))) %>% 
    full_join(Geo_keys_clean, .)
# 5
Geo_keys_clean <- select(Geo_keys_clean, country.isocode, geo_l3:geo_l5) %>% 
    filter(!is.na(country.isocode) & !is.na(geo_l3) & !is.na(geo_l4) & !is.na(geo_l5)) %>% 
    unique() %>% 
    mutate(geol5_id = seq(1, nrow(unique(filter(select(Geo_keys_clean, country.isocode, geo_l3:geo_l5), 
                                                !is.na(country.isocode) & !is.na(geo_l3) & !is.na(geo_l4) & !is.na(geo_l5)))))) %>% 
    full_join(Geo_keys_clean, .)
# 6
Geo_keys_clean <- select(Geo_keys_clean, country.isocode, geo_l3:geo_l6) %>% 
    filter(!is.na(country.isocode) & !is.na(geo_l3) & !is.na(geo_l4) & !is.na(geo_l5) & !is.na(geo_l6)) %>% 
    unique() %>% 
    mutate(geol6_id = seq(1, nrow(unique(filter(select(Geo_keys_clean, country.isocode, geo_l3:geo_l6), 
                                                !is.na(country.isocode) & !is.na(geo_l3) & !is.na(geo_l4) & !is.na(geo_l5)
                                                & !is.na(geo_l6)))))) %>% 
    full_join(Geo_keys_clean, .)
# 7
Geo_keys_clean <- select(Geo_keys_clean, country.isocode, geo_l3:geo_l7) %>% 
    filter(!is.na(country.isocode) & !is.na(geo_l3) & !is.na(geo_l4) & !is.na(geo_l5) & !is.na(geo_l6) & !is.na(geo_l7)) %>% 
    unique() %>% 
    mutate(geol7_id = seq(1, nrow(unique(filter(select(Geo_keys_clean, country.isocode, geo_l3:geo_l7), 
                                                !is.na(country.isocode) & !is.na(geo_l3) & !is.na(geo_l4) & !is.na(geo_l5)
                                                & !is.na(geo_l6) & !is.na(geo_l7)))))) %>% 
    full_join(Geo_keys_clean, .)

### Check unique locations in data after cleaning
select(Geo_keys_clean, continent, country, country.isocode:geo_l7) %>%
    unique() %>%
    dim()
