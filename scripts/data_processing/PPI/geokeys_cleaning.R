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

#### Convert subnational geolocations to upper case & remove punctuation
Geo_keys_clean <- Geo_keys_clean %>% 
    mutate(across(geo_l3:geo_l7, ~str_to_upper(.) %>% 
                      str_replace_all("&", "AND") %>% 
                      str_replace_all("-|–|_", " ") %>% 
                      str_remove_all("[[:punct:]]") %>% 
                      str_remove_all("[[:symbol:]]") %>% 
                      str_squish())) %>% 
    mutate(across(geo_l3:geo_l7, ~ifelse(. == "", NA, .)))


### *I think it's clean enough to start trying to fuzzy match w/ google* ###
