#### GISAID countries dictionary- country names, codes, & world bank income groups for countries that have submitted samples to GISAID ####
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

#### Clear environment & load metadata
rm(list = ls())
gc()
Metadata_raw <- read_csv("../../../data/raw/metadata.csv") # from extracted datastream

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

#### Get World Bank codes for these countries
# Identical to ISO 3166-1 alpha-3 codes, except identifies Kosovo & doesn't identify small dependencies of UK, Netherlands, & France
gisaid_countries <- mutate(gisaid_countries, country_code = countrycode(country, "country.name", "wb"))
## For remaining uncoded countries (small dependencies)- manually assign to the countries they are dependencies of
# Lists of dependencies
FRA.dep <- c("Reunion", "Saint Barthelemy", "Guadeloupe", "Mayotte", "French Guiana", "Martinique", "Wallis and Futuna Islands", "Saint Martin")
NLD.dep <- c("Sint Eustatius", "Bonaire")
GBR.dep <- c("Anguilla", "Montserrat")
# Assign codes
gisaid_countries <- mutate(gisaid_countries, country_code = 
                             case_when(grepl(paste0(FRA.dep, collapse = "|"), country) ~ "FRA",
                                       grepl(paste0(NLD.dep, collapse = "|"), country) ~ "NLD", 
                                       grepl(paste0(GBR.dep, collapse = "|"), country) ~ "GBR", 
                                       !grepl(paste0(c(FRA.dep,NLD.dep,GBR.dep), collapse = "|"), country) ~ country_code))

### Combine GISAID & World Bank
gisaid_countries <- left_join(gisaid_countries, world_bank_background_clean, 
                      by = c("country_code" = "code"))

#### Export
write_csv(gisaid_countries, "../../../data/processed/gisaid_countries.csv")
