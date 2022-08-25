#### Add sequences submitted by country to find_map.csv ####
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

#### Clear environment
rm(list = ls())
gc()

#### Import data
## Seqs by country
country_codes_nseqs <- read_csv("../../../data/processed/total_seqs_by_country.csv")

## Find map
find_map <- read_csv("https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/ngs_find_map/find_map.csv")

#### 2) Add seqs by country to find map data ####
find_map_wseqs <- left_join(find_map, country_codes_nseqs, by = "code")

#### 3) Export data ####
write_csv(find_map_wseqs, "../../../data/processed/find_map_w_totalseqs.csv")
