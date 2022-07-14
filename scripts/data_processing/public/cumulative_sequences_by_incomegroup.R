#### Create data for stacked bar chart of cumulative sequences over time ####
#### Jordan Klein

#### 1) Setup ####
rm(list = ls())
#### Load packages
library(tidyverse) # Data wrangling
library(tibble) # Data wrangling
library(janitor) # Column naming
library(countrycode) # Country codes
library(lubridate) # Date times
library(readxl) # Excel import
library(zoo) # Calculate rolling averages
library(R.utils) # R utilities
library(stringr) # To parse strings in R
library(magrittr) # Needs to be run every time you start R and want to use %>%
library(dplyr) # Data wrangling
library(scales) # Comma formatting
library(bpa) # To get the trim_ws working, which will allow you to join the lat and long files
library(reshape2) # melt function for wrangling

#### Load raw gisaid-owid merged data
gisaid_raw <- read_csv('../../../data/processed/gisaid_owid_merged.csv')

# Select important columns: collection date, country name, number of new sequences, OWID
# covid-19 case data, OWID population data, country code, OWID location
gisaid_t <- gisaid_raw%>%
  select(collection_date = gisaid_collect_date, gisaid_country, n_new_sequences,
         owid_new_cases, owid_population, country_code, owid_location)

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

#### 2) Get timeseries of cumulative sequences submitted over time ####
#### Gisaid data- add WB income group & month
gisaid_t <- left_join(gisaid_t, world_bank_background_clean, 
                      by = c("country_code" = "code")) %>% 
  filter(collection_date > ymd("2019-12-31")) %>%
  mutate(month = as.yearmon(collection_date))

#### Calculate monthly sum of sequences by income group
global_sequencing <- gisaid_t %>% 
  filter(!is.na(world_bank_economies)) %>%
  group_by(world_bank_economies, month) %>% 
  summarise(sequences = sum(n_new_sequences)) %>% 
  mutate(cum_seq = cumsum(sequences))

### Pivot to wider so usable with flourish
global_sequencing_wide <- select(global_sequencing, -sequences) %>% 
  pivot_wider(names_from = world_bank_economies, values_from = cum_seq) %>% 
  clean_names()

#### 3) Export ####
write_csv(global_sequencing_wide, "../../../data/processed/cumulative_sequences.csv")

