#### Create data for stacked bar chart of archetypes over time ####
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

### Load country codes-world bank dictionary
dictionary <- read_csv("../../../data/NGS_Data_Tables/Timeseries/gisaid_countries.csv")

### Load OWID
owid_raw <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')

### Load raw FIND data 
find_raw <- read_csv("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv")

### Load sequencing metadata
Metadata_raw <- read_csv("../../../data/raw/metadata.csv") # from extracted datastream

### Load NGS facility access data
test_seq_raw <- read_csv('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/Sequencing_labs_data.csv') %>% 
  clean_names() 

## Free up memory
gc()

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

#### 3) Extract sequencing data from gisaid ####
#### Exclude collections from before 2020-01-01
gisaid_t <- filter(Metadata_raw, collection_date >= ymd("2020-01-01")) %>% 
  # only keep variables I need
  select(country, collection_date, submission_date)
# Clear memory
rm(Metadata_raw)
gc()

### Get sequence of months to calculate sequence submissions over
date_seq <- seq.Date(floor_date(min(gisaid_t$submission_date, na.rm = T)+years(1), "months"), 
                     floor_date(max(gisaid_t$submission_date, na.rm = T), "months"), by = "month")

#### Split gisaid data into list
gisaid_t <- split(gisaid_t, gisaid_t$country)

## Create empty vector for results
results <- vector(mode = 'integer',
                  length = length(date_seq))

# Loop through calculation ------------------------------------------------ 
country_seqs <- lapply(gisaid_t, function(x) {
  for (i in 1:length(date_seq)){
    
    day_iter = ymd(date_seq[i])
    
    results[i] <-  x %>% 
      filter(collection_date >= day_iter - months(12),
             collection_date <= day_iter,
             submission_date <= day_iter) %>% 
      nrow()
  }
  df <- tibble(date = ymd(date_seq), n = results)
})

#### Create dataframe
gisaid_nseqs <- bind_rows(country_seqs, .id = "country") %>% 
  rename(seqs_n = n) %>% 
  left_join(dictionary_pop) %>% 
  group_by(code, date) %>% 
  summarise(seqs_n = sum(seqs_n))

#### 4) Get NGS facility data ####
# Get only NGS facility data
ngs_clean <- test_seq_raw %>%
  select(contains("code"), starts_with("ngs_capacity_"))

# Define capacity column for later reference
ngs_capacity_column <- colnames(ngs_clean)[max(ncol(ngs_clean))]

# New variable is ngs_capacity, and writing this function on ngs_capacity_column
ngs_clean$ngs_capacity <- case_when(
  ngs_clean[ , ngs_capacity_column] == 0 ~ 0,
  ngs_clean[ , ngs_capacity_column] == "0 - No NGS facilities" ~ 0,
  ngs_clean[ , ngs_capacity_column] == 1 ~ 1,
  ngs_clean[ , ngs_capacity_column] == "1 - 1-3 NGS facilities or equivalent" ~ 1,
  ngs_clean[ , ngs_capacity_column] == 2 ~ 2,
  ngs_clean[ , ngs_capacity_column] == "2 - >3 NGS facilities or equivalent" ~ 2
)
# Make a new binary facility access variable
ngs_clean<-ngs_clean%>%mutate(
  facility_access = case_when(
    ngs_capacity ==0 ~ F,
    (ngs_capacity ==1 | ngs_capacity == 2) ~ T)) %>% 
  select(code = country_code, facility_access)

#### 5) Run code chunks from get_test_and_ngs_metrics_by_country.R to start processing FIND data ####
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
first_date<-ymd("2020-01-01")
date <- seq.Date(first_date, today(), by = "day")
code <-unique(find_testing_t$code)
date_country<-expand_grid(date, code)

# Join the find_test_t dataframe with filled dates df
find_testing_t<-left_join(date_country,find_testing_t, by = c("code", "date"))

#### 6) Extract testing data from FIND ####
#### Only keep the variables I need
find_testing_t <- select(find_testing_t, code, date, new_cases_smoothed_truncated, new_tests_smoothed)

#### Split into list of rolling 12 month windows
find_l <- lapply(date_seq, function(x) {
  data <- find_testing_t %>% 
    filter(date >= x - months(12), 
           date <= x)
  return(data)
})

#### Calculate completeness for each country in these rolling windows
find_l <- lapply(find_l, function(x) {
  data <- group_by(x, code) %>% 
    summarise(cases_missing = sum(is.na(new_cases_smoothed_truncated))/n(), 
              tests_missing = sum(is.na(new_tests_smoothed))/n()) %>% 
    left_join(x, .)
  return(data)
})

#### Calculate cases & tests over these windows (total & means)
## Cases
cases_ls <- lapply(find_l, function(x) {
  filter(x, cases_missing <= .25) %>% 
    group_by(code) %>% 
    summarise(cases_total = sum(new_cases_smoothed_truncated, na.rm = T), 
              cases_mean = mean(new_cases_smoothed_truncated, na.rm = T))
})
names(cases_ls) <- date_seq
## Tests
tests_ls <- lapply(find_l, function(x) {
  filter(x, tests_missing <= .25) %>% 
    group_by(code) %>% 
    summarise(tests_total = sum(new_tests_smoothed, na.rm = T), 
              tests_mean = mean(new_tests_smoothed, na.rm = T))
})
names(tests_ls) <- date_seq

#### Combine results
## Empty dataframe of country codes & dates
find_results <- expand_grid(code = unique(find_testing_t$code), date = date_seq)
## Combine in case data
find_results <- bind_rows(cases_ls, .id = "date") %>% 
  mutate(date = ymd(date)) %>% 
  left_join(find_results, .)
## Combine in test data
find_results <- bind_rows(tests_ls, .id = "date") %>% 
  mutate(date = ymd(date)) %>% 
  left_join(find_results, .)

#### 7) Combine metrics & classify countries into archetypes ####
#### Combine testing, sequences, & NGS capacity metrics
Metrics_full <- full_join(find_results, gisaid_nseqs) %>% 
  left_join(., ngs_clean)

#### Add income group & population from world bank
Metrics_full <- select(dictionary_pop, -country) %>% 
  unique() %>% 
  left_join(Metrics_full)

## Quick fix- any countries where cases sequenced > cases, set cases sequenced = cases
Metrics_full$seqs_n[Metrics_full$seqs_n > Metrics_full$cases_total & !is.na(Metrics_full$seqs_n) & !is.na(Metrics_full$cases_total)] <- 
  Metrics_full$cases_total[Metrics_full$seqs_n > Metrics_full$cases_total & !is.na(Metrics_full$seqs_n) & !is.na(Metrics_full$cases_total)]

#### Calculate key metrics
### % of cases sequenced
Metrics_full <- Metrics_full %>% 
  mutate(pct_seq = seqs_n/cases_total*100) %>% 
  ### sequences per 100k
  mutate(sequences_per_100k = seqs_n/population*100000) %>% 
  ### Daily tests per 1,000
  mutate(tests_per_1000 = tests_mean/population*1000) %>% 
  ### Test positivity
  mutate(tpr = cases_total/tests_total*100)

#### Classification into archetypes
Archetypes <- Metrics_full %>% 
  mutate(seq_arch = case_when(pct_seq >= .5 & sequences_per_100k >= 10 ~ "Sustain", 
                              (pct_seq < .5 | sequences_per_100k < 10) & facility_access == T ~ "Strengthen/Leverage", 
                              (pct_seq < .5 | sequences_per_100k < 10) & facility_access == F ~ "Connect/Build"), 
         test_arch = case_when(tpr < 20 & tests_per_1000 >= .5 ~ "Sustain", 
                               tpr >= 20 | tests_per_1000 < .5 ~ "Test")) %>% 
  select(-c(cases_total:tpr)) %>% 
  mutate(seq_arch = case_when(is.na(seq_arch) ~ "Insufficient data", !is.na(seq_arch) ~ seq_arch), 
         test_arch = case_when(is.na(test_arch) ~ "Insufficient data", !is.na(test_arch) ~ test_arch)) %>% 
  mutate(seq_arch = as.factor(seq_arch), test_arch = as.factor(test_arch))

#### 8) Format for Flourish ####
##### Tabulate archetypes by income group by date
#### Count n countries in each archetype
## Sequencing archetypes
Arch_seqtab <- Archetypes %>% 
  group_by(date, world_bank_economies, seq_arch, .drop = F) %>% 
  summarise(number = n()) %>% 
  pivot_wider(names_from = seq_arch, values_from = number)
# Add table of all income groups
Arch_seqtab <- ungroup(Arch_seqtab) %>% 
  group_by(date) %>% 
  summarise(`Insufficient data` = sum(`Insufficient data`), 
            `Connect/Build` = sum(`Connect/Build`), 
            `Strengthen/Leverage` = sum(`Strengthen/Leverage`), 
            Sustain = sum(Sustain)) %>% 
  add_column(world_bank_economies = "All", .after = "date") %>% 
  bind_rows(Arch_seqtab)
Arch_seqtab <- Arch_seqtab[order(match(Arch_seqtab$world_bank_economies, c("All", "High income", "Upper middle income", "Lower middle income", "Low income"))), ] %>% 
  arrange(date)

## Testing archetypes
Arch_testtab <- Archetypes %>%
  group_by(date, world_bank_economies, test_arch, .drop = F) %>% 
  summarise(number = n()) %>% 
  pivot_wider(names_from = test_arch, values_from = number)
# Add table of all income groups
Arch_testtab <- ungroup(Arch_testtab) %>% 
  group_by(date) %>% 
  summarise(`Insufficient data` = sum(`Insufficient data`), 
            Test = sum(Test), 
            Sustain = sum(Sustain)) %>% 
  add_column(world_bank_economies = "All", .after = "date") %>% 
  bind_rows(Arch_testtab)
Arch_testtab <- Arch_testtab[order(match(Arch_testtab$world_bank_economies, c("All", "High income", "Upper middle income", "Lower middle income", "Low income"))), ] %>% 
  arrange(date)

#### Count population in each archetype
## Sequencing archetypes
Arch_seqtab_pop <- Archetypes %>% 
  group_by(date, world_bank_economies, seq_arch, .drop = F) %>% 
  summarise(population = sum(population)) %>% 
  pivot_wider(names_from = seq_arch, values_from = population)
# Add table of all income groups
Arch_seqtab_pop <- ungroup(Arch_seqtab_pop) %>% 
  group_by(date) %>% 
  summarise(`Insufficient data` = sum(`Insufficient data`), 
            `Connect/Build` = sum(`Connect/Build`), 
            `Strengthen/Leverage` = sum(`Strengthen/Leverage`), 
            Sustain = sum(Sustain)) %>% 
  add_column(world_bank_economies = "All", .after = "date") %>% 
  bind_rows(Arch_seqtab_pop)
Arch_seqtab_pop <- Arch_seqtab_pop[order(match(Arch_seqtab_pop$world_bank_economies, c("All", "High income", "Upper middle income", "Lower middle income", "Low income"))), ] %>% 
  arrange(date)

## Testing archetypes
Arch_testtab_pop <- Archetypes %>%
  group_by(date, world_bank_economies, test_arch, .drop = F) %>% 
  summarise(population = sum(population)) %>% 
  pivot_wider(names_from = test_arch, values_from = population)
# Add table of all income groups
Arch_testtab_pop <- ungroup(Arch_testtab_pop) %>% 
  group_by(date) %>% 
  summarise(`Insufficient data` = sum(`Insufficient data`), 
            Test = sum(Test), 
            Sustain = sum(Sustain)) %>% 
  add_column(world_bank_economies = "All", .after = "date") %>% 
  bind_rows(Arch_testtab_pop)
Arch_testtab_pop <- Arch_testtab_pop[order(match(Arch_testtab_pop$world_bank_economies, c("All", "High income", "Upper middle income", "Lower middle income", "Low income"))), ] %>% 
  arrange(date)

#### 9) Export ####
#### Archetypes by N(countries)
### NGS 
write_csv(Arch_seqtab, "../../../data/NGS_Data_Tables/Timeseries/sequencing_archetypes.csv")
### Testing
write_csv(Arch_testtab, "../../../data/NGS_Data_Tables/Timeseries/testing_archetypes.csv")

#### Archetypes by population
### NGS 
write_csv(Arch_seqtab_pop, "../../../data/NGS_Data_Tables/Timeseries/sequencing_archetypes_pop.csv")
### Testing
write_csv(Arch_testtab_pop, "../../../data/NGS_Data_Tables/Timeseries/testing_archetypes_pop.csv")

#### Alternate versions of datasets excluding category with "All" countries
### NGS
filter(Arch_seqtab_pop, world_bank_economies != "All") %>% 
  write_csv("../../../data/NGS_Data_Tables/Timeseries/sequencing_archetypes_byincome.csv")
### Testing
filter(Arch_testtab_pop, world_bank_economies != "All") %>% 
  write_csv("../../../data/NGS_Data_Tables/Timeseries/testing_archetypes_byincome.csv")
