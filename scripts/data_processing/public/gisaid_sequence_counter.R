#### Get timeseries of sequences that have been submitted by date x that were collected in the past 30 days ####
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

#### 2) Extract data from gisaid ####
#### Exclude collections from before 2020-01-01
gisaid_t <- filter(Metadata_raw, collection_date >= ymd("2020-01-01")) %>% 
  # only keep variables I need
  select(country, collection_date, submission_date)
# Clear memory
rm(Metadata_raw)
gc()

### Get sequence of date range of gisaid submissions
date_seq <- seq.Date(min(gisaid_t$submission_date, na.rm = T), max(gisaid_t$submission_date, na.rm = T), by = "day")

#### Split gisaid data into list
gisaid_t <- split(gisaid_t, gisaid_t$country)

## Create empty vector for results
results <- vector(mode = 'integer',
                  length = length(date_seq))

# Loop through calculation ------------------------------------------------ (Takes a really long time)
country_seqs <- lapply(gisaid_t, function(x) {
  for (i in 1:length(date_seq)){
    
    day_iter = ymd(date_seq[i])
    
    results[i] <-  x %>% 
      filter(collection_date >= day_iter - days(29),
             collection_date <= day_iter,
             submission_date <= day_iter) %>% 
      nrow()
  }
  df <- tibble(date = ymd(date_seq), n = results)
})

#### Create dataframe
country_seqs_df <- bind_rows(country_seqs, .id = "country") %>% 
  rename(seqs_n = n)

#### Export dataframe
write_csv(country_seqs_df, "../../../data/processed/countries_seqs.csv")
