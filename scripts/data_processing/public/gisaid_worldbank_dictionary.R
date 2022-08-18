#### Countries dictionary- country names, codes, & world bank income groups for countries that have submitted samples to GISAID/have testing data from FIND ####
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

#### Clear environment & load data
rm(list = ls())
gc()
# GISAID
Metadata_raw <- read_csv("../../../data/raw/metadata.csv") # from extracted datastream
# FIND
find_raw <- read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv")

#### 2) Import & clean WB data ####
#### Get World Bank SES groups
world_bank_background_raw <- read_csv('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/WB_class_data.csv') %>%
  # Standardize names with this janitor function
  clean_names()

# Remove buffer rows by iso3 code and select code and testing capacity columns
world_bank_background_clean <- world_bank_background_raw %>%
  # Drop all columns except iso3 code and world_bank_economies
  select('code','income_group')%>%
  filter(code != 'x') 
# Rename columns code and world_bank_economies
colnames(world_bank_background_clean) <- c("code", "world_bank_economies")

#replace missing values with 'No income data' in world_bank_economies column
world_bank_background_clean$world_bank_economies <- world_bank_background_clean$world_bank_economies %>% replace_na('No income data')

#### 3) Create dictionary ####
#### List of countries in GISAID data
gisaid_countries <- select(Metadata_raw, country) %>% unique %>% 
  filter(!is.na(country))

#### List of countries in FIND data
find_countries <- find_raw %>%
  filter(set == "country") %>%
  select(country = name) %>% unique %>% 
  filter(!is.na(country))

#### Get World Bank codes for these countries
## Identical to ISO 3166-1 alpha-3 codes, except identifies Kosovo & doesn't identify small dependencies of UK, Netherlands, & France
# GISAID
gisaid_countries <- mutate(gisaid_countries, country_code = countrycode(country, "country.name", "wb"))
# FIND 
find_countries <- mutate(find_countries, country_code = countrycode(country, "country.name", "wb"))
## Combine countries
all_countries <- filter(find_countries, !(country_code %in% gisaid_countries$country_code) | is.na(country_code)) %>% 
  bind_rows(gisaid_countries, .)

### Combine countries & World Bank
all_countries <- left_join(all_countries, world_bank_background_clean, 
                           by = c("country_code" = "code"))

#### Deal with remaining uncoded
## List dependencies not in world bank data
FRA.dep <- c("Reunion", "Saint Barthelemy", "Guadeloupe", "Mayotte", "French Guiana", "Martinique", "Wallis and Futuna Islands", "Saint Martin")
NLD.dep <- c("Sint Eustatius", "Bonaire")
GBR.dep <- c("Anguilla", "Montserrat")

## Use ISO 3166-1 alpha-3 codes for countries w/o a world bank code
all_countries <- mutate(all_countries, country_code = 
                          case_when(is.na(country_code) ~ countrycode(country, "country.name", "iso3c"), 
                                    !is.na(country_code) ~ country_code))

## remaining uncoded countries
# manually assign micronesia -> fsm
all_countries  <- mutate(all_countries, country_code = 
                           case_when(country == "Micronesia" ~ "FSM", country != "Micronesia" ~ country_code))
# Dutch dependencies
all_countries <- mutate(all_countries, country_code = 
                          case_when(grepl(paste0(NLD.dep, collapse = "|"), country) ~ "BES", 
                                    !grepl(paste0(NLD.dep, collapse = "|"), country) ~ country_code))
# Saint Martin
all_countries <- mutate(all_countries, country_code = 
                          case_when(country == "Saint Martin" ~ "MAF", country != "Saint Martin" ~ country_code))

### Assign British, French, & Dutch dependencies to high income
all_countries <- mutate(all_countries, world_bank_economies = 
                             case_when(grepl(paste0(c(FRA.dep,NLD.dep,GBR.dep), collapse = "|"), country) ~ "High income", 
                                       !grepl(paste0(c(FRA.dep,NLD.dep,GBR.dep), collapse = "|"), country) ~ world_bank_economies))

#### Manually add Western Sahara- it has its own ISO 3166-1 alpha-3 code but its economy is integrated w/ Morocco's
all_countries$world_bank_economies[all_countries$country == "Western Sahara"] <- all_countries$world_bank_economies[all_countries$country == "Morocco"]

#### Add world bank income groups for any countries still missing that were coded manually
all_countries <- filter(all_countries, is.na(world_bank_economies)) %>% 
  select(-world_bank_economies) %>%
  left_join(world_bank_background_clean, by = c("country_code" = "code")) %>% 
  bind_rows(filter(all_countries, !is.na(world_bank_economies)))

#### Export
write_csv(all_countries, "../../../data/processed/gisaid_countries.csv")
