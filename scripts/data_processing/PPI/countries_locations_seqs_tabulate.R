#### Create file with sequences collected by 26 countries of interest in the previous 12 months ####
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
library(stringi)

#### Clear environment & load metadata
rm(list = ls())
gc()
Metadata_raw <- read_csv("../../../data/raw/metadata.csv") # from extracted datastream

### Load country codes-world bank dictionary
dictionary <- read_csv("../../../data/NGS_Data_Tables/Timeseries/gisaid_countries.csv")

### Load basemap data linking file
keys <- read_csv("../../../data/raw/basemap/keys.csv")

#### 2) Clean metadata to get frequency table of sequences collected by 26 countries in past 12 months ####
#### Line list of locations collected in previous 12 months in these countries
### Join dictionary & metadata
Countries_seq <- left_join(Metadata_raw, dictionary) %>% 
    select(country, country_code, location, submission_date, collection_date) %>% 
    ### Only keep observations in 26 countries
    filter(country_code %in% keys$iso3) %>% 
    ### Only keep observations collected in previous 12 months
    filter(collection_date >= today() - months(12) & collection_date <= today() 
           & submission_date <= today()) %>% 
    select(iso3 = country_code, location)

#### Frequency table of sequences collected in these countries
seqs_countries <- group_by(Countries_seq, iso3) %>% 
    summarise(seqs = n())

#### 3) Export data ####
#### Line list of locations collected in previous 12 months
write_csv(Countries_seq, "../../../data/raw/gisaid_26countries_locations_lastyear.csv")

#### Sequences submitted in the previous 12 months by country
write_csv(seqs_countries, "../../../data/raw/basemap/seqs_countries.csv")

