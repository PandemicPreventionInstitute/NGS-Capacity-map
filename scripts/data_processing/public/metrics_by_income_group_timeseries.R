#### Create data for time series plot of sequencing & testing metrics over time ####
#### Jordan Klein

#### 1) Setup ####
#### Clear environment
rm(list = ls())
gc()

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

### Load country codes-world bank dictionary
dictionary <- read_csv("../../../data/NGS_Data_Tables/Timeseries/gisaid_countries.csv")

### Load OWID
owid_raw <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')

### Load raw FIND data 
find_raw <- read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv")

### Load processed data- countries n sequences past 30 days
gisaid_nseqs <- read_csv("../../../data/NGS_Data_Tables/Timeseries/countries_seqs.csv")

#### 2) Combine country dictionary & population data ####
#### Get population data from OWID
owid_pop <- unique(select(owid_raw, iso_code, population)) %>% 
  mutate(iso_code = case_when(iso_code == "OWID_KOS" ~ "XKX", iso_code != "OWID_KOS" ~ iso_code)) %>%
  filter(!iso_code %in% c("OWID_AFR","OWID_ASI", "OWID_EUR", "OWID_EUN",
                          "OWID_INT", "OWID_NAM", "OWID_OCE",
                          "OWID_SAM", "OWID_WRL",
                          "OWID_CYN", "OWID_HIC", "OWID_KOS", "OWID_LIC",
                          "OWID_LMC", "OWID_UMC"))

#### Population for all countries in gisaid data
dictionary_pop <- left_join(dictionary, owid_pop, by = c("country_code" = "iso_code"))
# Manually add countries owid doesn't have population for (https://data.worldbank.org/indicator/SP.POP.TOTL?locations)
dictionary_pop$population[dictionary_pop$country_code == "ASM"] <- 55197
dictionary_pop$population[dictionary_pop$country_code == "MAF"] <- 38659

## Manually assign french overseas regions to France
dictionary_pop$country_code[grepl(c("Reunion|Guadeloupe|Mayotte|French Guiana|Martinique|Saint Barthelemy"), dictionary_pop$country)] <- "FRA"
dictionary_pop$population[grepl(c("Reunion|Guadeloupe|Mayotte|French Guiana|Martinique|Saint Barthelemy"), dictionary_pop$country)] <- 
  dictionary_pop$population[dictionary_pop$country == "France"]
# Rename country code -> code for copatability with find data
dictionary_pop <- rename(dictionary_pop, code = country_code)

#### 3) Run code chunks from get_test_and_ngs_metrics_by_country.R to start processing FIND data ####
## Select and rename necessary columns
find_testing_t <- find_raw %>%
  # Filter for country set
  filter(set == "country") %>%
  # Select the smoothed and raw test and case data by day (case data is also from OWID)
  select(name, time, unit, pop_100k, all_new_tests, all_new_cases, 
         pos, new_tests_orig, new_cases_orig) %>%
  # Rename columns as date, code, pop_100k, new_tests_smoothed, cum_tests, cum_cases, new_cases_smoothed
  rename(country = name, date = time, code = unit, new_tests_smoothed = all_new_tests, 
         new_cases_smoothed = all_new_cases) %>%
  
  # Parse date as date class
  # Data parsing is the process of taking data in one format & transforming it to another format
  mutate(date = as.Date(date, format = "%Y-%m-%d"), #reformatting date
         pop = pop_100k*100000, # get full pop
         # make country code column for joining
         code = countrycode(country, origin = 'country.name', destination = 'iso3c')) 

# New_tests_smoothed runs only up until the most recent date of test reporting. Therefore, we need cases to also be truncated
# at this point to calculate the test positivity rate properly. We replace all case data after the data of tests last being reported
# as NA (last day of smoothed test data)

# Add a new column that has the new cases smoothed with NA entries when new_tests_smoothed is NA
find_testing_t['new_cases_smoothed_truncated']<-find_testing_t$new_cases_smoothed
find_testing_t$new_cases_smoothed_truncated[is.na(find_testing_t$new_tests_smoothed)]<-NA

# Inserts missing country codes manually 
find_testing_t$code[find_testing_t$country == "Kosovo"] <- "XXK"
find_testing_t$code[find_testing_t$country == "Namibia"] <- "NAM"

# FILL IN MISSING DATES (just in case dates are shy)
# Set start date
first_date<-min(find_testing_t$date, na.rm = TRUE)
date <- seq.Date(first_date, today(), by = "day")
code <-unique(find_testing_t$code)
date_country<-expand_grid(date, code)

# Join the find_test_t dataframe with filled dates df
find_testing_t<-left_join(date_country,find_testing_t, by = c("code", "date"))

#### 4) Extract testing data from FIND ####
#### Only keep the variables I need
find_testing_t <- select(find_testing_t, code, date, new_cases_smoothed_truncated, new_tests_smoothed)

#### Find rolling 30 sums of cases & tests by country
find_testing_t <- group_by(find_testing_t, code) %>% 
  mutate(cases_30days = rollapply(new_cases_smoothed_truncated, width = 30, partial = 8, FUN = sum, align = "right", fill = NA), 
         tests_30days = rollapply(new_tests_smoothed, width = 30, partial = 8, FUN = sum, align = "right", fill = NA))

#### 5) Combine testing & sequencing metrics w/ country dictionary ####
#### Combine sequencing data w/ country dictionary- consolidate sequences from locations with the same country code
gisaid_nseqs_consol <- left_join(gisaid_nseqs, dictionary_pop) %>% 
  group_by(code, date) %>% 
  summarise(seqs_n = sum(seqs_n))

#### Combine testing & sequencing metrics
metrics_full <- full_join(select(find_testing_t, -new_cases_smoothed_truncated, -new_tests_smoothed),  gisaid_nseqs_consol) 
# Add expand the grid for missing dates/countries
metrics_full <- left_join(expand_grid(code = unique(metrics_full$code), date = unique(metrics_full$date)), 
                          metrics_full)

### Add income group & population from world bank
metrics_full <- select(dictionary_pop, -country) %>% 
  unique() %>% 
  left_join(metrics_full)

#### 6) Calculate daily time series by income group ####
#### Empty dataframe of income groups & dates
IG_metrics <- select(metrics_full, world_bank_economies, date) %>% 
  unique() %>% 
  arrange(date)

#### Get mean per capita cases in the last 30 days
IG_metrics <- filter(metrics_full, !is.na(cases_30days)) %>% 
  group_by(world_bank_economies, date) %>% 
  summarise(cases_per_1000 = (sum(cases_30days)/sum(population/1000))/30) %>% 
  left_join(IG_metrics, .)
#### Get mean per capita tests in the last 30 days
IG_metrics <- filter(metrics_full, !is.na(tests_30days)) %>% 
  group_by(world_bank_economies, date) %>% 
  summarise(tests_per_1000 = (sum(tests_30days)/sum(population/1000))/30) %>% 
  left_join(IG_metrics, .)
#### Get mean per capita sequences in the last 30 days
IG_metrics <- filter(metrics_full, !is.na(seqs_n)) %>% 
  group_by(world_bank_economies, date) %>% 
  summarise(sequences_per_100k = (sum(seqs_n)/sum(population/100000))/30) %>% 
  left_join(IG_metrics, .)
#### Get positivity rate
IG_metrics <- filter(metrics_full, !is.na(cases_30days) & !is.na(tests_30days)) %>% 
  group_by(world_bank_economies, date) %>% 
  summarise(tpr = sum(cases_30days)/sum(tests_30days)*100) %>% 
  left_join(IG_metrics, .)
#### Get % of cases sequenced
IG_metrics <- filter(metrics_full, !is.na(cases_30days) & !is.na(seqs_n)) %>% 
  group_by(world_bank_economies, date) %>% 
  summarise(pct_seq = sum(seqs_n)/sum(cases_30days)*100) %>% 
  left_join(IG_metrics, .)

### Pivot to wider so usable with flourish
IG_metrics_wide <- pivot_longer(IG_metrics, cases_per_1000:pct_seq, names_to = "metric", 
                                values_to = "value", values_drop_na = F) %>% 
  pivot_wider(names_from = world_bank_economies, values_from = value) %>% 
  select(date, metric, `High income`, `Upper middle income`, `Lower middle income`, `Low income`) %>% 
  mutate(metric = case_when(metric == "cases_per_1000" ~ "Daily cases per 1,000", 
                            metric == "tests_per_1000" ~ "Daily tests per 1,000", 
                            metric == "sequences_per_100k" ~ "Daily sequences per 100,000", 
                            metric == "tpr" ~ "Test positivity (%)", 
                            metric == "pct_seq" ~ "Cases sequenced (%)")) %>% 
  ## Cut off dates before 5/1/2020
  filter(date >= ymd("2020-05-01")) %>% 
  ## Add date as character variable for popups
  mutate(date_popup = as.character(date))

#### 7) Export data ####
write_csv(IG_metrics_wide, "../../../data/NGS_Data_Tables/Timeseries/metrics_timeseries.csv")
