#### Tabulate sequences submitted by country since the beginning of the pandemic ####
#### Jordan Klein

#### 1) Setup ####
#### Run GISAID metadata extracting script
source("../PPI/auto_extract_gisaid_metadata.R")

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

### Load sequencing metadata
Metadata_raw <- read_csv("../../../data/raw/metadata.csv") # from extracted datastream

## Free up memory
gc()

#### 2) Clean data ####
#### Tabulate countries
country_tab <- group_by(Metadata_raw, country) %>% 
    summarise(total_seqs = n())

#### Add country codes
country_tab <- mutate(country_tab, code = countrycode(country, "country.name", "iso3c"))

#### Clean
# Drop missing country
country_tab_clean <- filter(country_tab, !is.na(country)) %>% 
    # Use world bank codes for countries without ISO 3-c codes
    mutate(code = case_when(is.na(code) ~ countrycode(country, "country.name", "wb"), 
                            !is.na(code) ~ code))
### remaining uncoded countries
# manually assign micronesia -> fsm
country_tab_clean <- mutate(country_tab_clean, code = case_when(country == "Micronesia" ~ "FSM", 
                                                                country != "Micronesia" ~ code))
## Dutch dependencies
# Get list of dutch dependencies
NLD.dep <- c("Sint Eustatius", "Bonaire")
# code these dependencies as NLD
country_tab_clean <- mutate(country_tab_clean, code = 
                            case_when(grepl(paste0(NLD.dep, collapse = "|"), country) ~ "NLD", 
                                      !grepl(paste0(NLD.dep, collapse = "|"), country) ~ code))
# Saint Martin
country_tab_clean <- mutate(country_tab_clean, code = 
                            case_when(country == "Saint Martin" ~ "MAF", country != "Saint Martin" ~ code))

## Aggregate by country code
country_codes_nseqs <- group_by(country_tab_clean, code) %>% 
    summarise(total_seqs = sum(total_seqs))

#### 3) Export data ####
write_csv(country_codes_nseqs, "../../../data/processed/total_seqs_by_country.csv")
