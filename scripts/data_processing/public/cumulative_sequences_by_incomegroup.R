#### Create data for stacked bar chart of cumulative sequences over time ####
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
Metadata_raw <- read_csv("../../../data/raw/metadata.csv") # from extracted datastream

#### 2) Clean data gisaid metadata ####
#### Exclude collections/submissions before 2019-12-01, after 2022-06-30
gisaid_t <- filter(Metadata_raw, submission_date >= ymd("2019-12-01") & collection_date >= ymd("2019-12-01") & 
                     submission_date < ymd("2022-07-01") & collection_date < ymd("2022-07-01")) %>% 
  # only keep variables I need
  select(country, submission_date)

### Add iso3 country codes
gisaid_t <- mutate(gisaid_t, country_code = countrycode(country, "country.name", "iso3c"))
## Manually add codes for countries that didn't match (https://laendercode.net/en/countries.html)
gisaid_t <- gisaid_t %>% 
  mutate(country_code = case_when(grepl("Kosovo", country) ~ "XKX", 
                                     grepl("Saint Martin", country) ~ "MAF", 
                                     grepl("Bonaire|Sint Eustatius", country) ~ "BES", 
                                     !grepl("Kosovo|Saint Martin|Bonaire|Sint Eustatius", country) ~ country_code))

#### 3) Import & clean WB data ####
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

#### 4) Get timeseries of cumulative sequences submitted over time ####
#### Gisaid data- add WB income group & month
gisaid_t <- left_join(gisaid_t, world_bank_background_clean, 
                      by = c("country_code" = "code")) %>% 
  mutate(month = as.yearmon(submission_date))
## Check remaining unmatched countries in gisaid data
filter(gisaid_t, is.na(world_bank_economies)) %>% 
  select(country) %>% 
  unique
### These are all British, French, or Dutch dependencies, so classify as high income
gisaid_t <- gisaid_t %>% 
  mutate(world_bank_economies = case_when(is.na(world_bank_economies) ~ "High income", 
                                          !is.na(world_bank_economies) ~ world_bank_economies))

#### Calculate monthly sum of sequences by income group
global_sequencing <- gisaid_t %>% 
  group_by(world_bank_economies, month) %>% 
  summarise(sequences = n())
## Expand grid to fill in missing income level-month combinations
global_sequencing <- expand_grid(global_sequencing$world_bank_economies, global_sequencing$month) %>% unique %>% 
  full_join(global_sequencing, ., by = c("world_bank_economies" = "global_sequencing$world_bank_economies", 
                                         "month" = "global_sequencing$month"))
global_sequencing$sequences[is.na(global_sequencing$sequences)] <- 0

### Calculate cumulative sum of sequences by income group
global_sequencing <- group_by(global_sequencing, world_bank_economies) %>%
  mutate(cum_seq = cumsum(sequences))
global_sequencing$cum_seq[global_sequencing$sequences == 0] <- 0

### Pivot to wider so usable with flourish
global_sequencing_wide <- select(global_sequencing, -sequences) %>% 
  pivot_wider(names_from = world_bank_economies, values_from = cum_seq) %>% 
  clean_names()

#### 3) Export ####
write_csv(global_sequencing_wide, "../../../data/processed/cumulative_sequences.csv")

