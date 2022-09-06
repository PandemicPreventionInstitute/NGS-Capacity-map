#### Combine seqs data layer files & basemap layers to create data for flourish maps of NGS capacity ####
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

#### Clear environment
rm(list = ls())
gc()

##### Load data
#### Sequencing
## Country level
seqs_countries <- read_csv("../../../data/raw/basemap/seqs_countries.csv")
## ADM1 level
seqs_adm1 <- read_csv("../../../data/raw/basemap/seqs_adm1.csv")

#### Pop-geospatial
## Country level
countries <- st_read("../../../data/raw/basemap/countries.geojson")
## ADM1 level
adm1 <- st_read("../../../data/raw/basemap/adm1.geojson")

#### File joining keys
keys <- read_csv("../../../data/raw/basemap/keys.csv")

#### 2) Create data ####
#### Countries
countries_combined <- left_join(countries, seqs_countries) %>% 
    # Calculate sequences per capita
    mutate(seqs_per_100k = round(100000*seqs/pop,3))

#### ADM1 divisions
adm1_combined <- left_join(adm1, seqs_adm1) %>% 
    # divisions with no sequences -> set to 0
    mutate(seqs = if_else(is.na(seqs), 0, seqs)) %>% 
    # Calculate sequences per capita
    mutate(seqs_per_100k = round(100000*seqs/pop,3))

#### Count observations dropped when go from country to adm1 level
Dropped <- st_drop_geometry(adm1_combined) %>% group_by(iso3) %>% 
    summarise(adm1_seqs = sum(seqs)) %>% 
    full_join(st_drop_geometry(select(countries_combined, iso3, country_seqs = seqs))) %>% 
    mutate(nseqs_dropped = country_seqs - adm1_seqs) %>% 
    select(iso3, nseqs_dropped)
### Add to adm1 level data
adm1_combined <- left_join(adm1_combined, Dropped)

#### 3) Export data ####
#### Country level
st_write(countries_combined, "../../../data/processed/NGS_capacity_adm0.geojson", append = F)
#### Adm 1 level
st_write(adm1_combined, "../../../data/processed/NGS_capacity_adm1.geojson", append = F)


