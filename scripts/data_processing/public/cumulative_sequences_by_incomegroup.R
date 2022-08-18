#### Create data for stacked bar chart of cumulative sequences over time ####
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

### Load country codes-world bank dictionary
dictionary <- read_csv("../../../data/processed/gisaid_countries.csv")

#### 2) Clean data gisaid metadata ####
#### Exclude collections/submissions before 2019-12-01, after the last day of the previous month
gisaid_t <- filter(Metadata_raw, submission_date >= ymd("2019-12-01") & collection_date >= ymd("2019-12-01") & 
                     submission_date < floor_date(today(), "month") & collection_date < floor_date(today(), "month")) %>% 
  # only keep variables I need
  select(country, submission_date)

#### 3) Get timeseries of cumulative sequences submitted over time ####
#### Gisaid data- add WB income group & month
gisaid_t <- left_join(gisaid_t, dictionary) %>% 
  mutate(month = as.yearmon(submission_date))

#### Calculate monthly sum of sequences by income group
global_sequencing <- gisaid_t %>% 
  group_by(world_bank_economies, month) %>% 
  summarise(sequences = n())
## Expand grid to fill in missing income level-month combinations
global_sequencing <- expand_grid(global_sequencing$world_bank_economies, global_sequencing$month) %>% unique %>% 
  full_join(global_sequencing, ., by = c("world_bank_economies" = "global_sequencing$world_bank_economies", 
                                         "month" = "global_sequencing$month"))
global_sequencing$sequences[is.na(global_sequencing$sequences)] <- 0
## Arrange by month within income groups
global_sequencing <- arrange(global_sequencing, month, .by_group = T)

### Calculate cumulative sum of sequences by income group
global_sequencing <- group_by(global_sequencing, world_bank_economies) %>%
  mutate(cum_seq = cumsum(sequences))

### Pivot to wider so usable with flourish
# By month
global_sequencing_wide <- select(global_sequencing, -cum_seq) %>% 
  pivot_wider(names_from = world_bank_economies, values_from = sequences)

# Cumulative
global_sequencing_wide_cum <- select(global_sequencing, -sequences) %>% 
  pivot_wider(names_from = world_bank_economies, values_from = cum_seq)

#### 3) Export ####
# By month
write_csv(global_sequencing_wide, "../../../data/processed/sequences_by_month.csv")

# Cumulative
write_csv(global_sequencing_wide_cum, "../../../data/processed/cumulative_sequences.csv")
