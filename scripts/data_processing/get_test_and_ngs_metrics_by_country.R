# Original Author: Nathan Poland
# Updated: Briana Thrift & Zach Susswein
# Current Author: Kaitlyn Johnson

# Date updated: 11-15-2021




#install.packages("janitor")
#install.packages("countrycode")
#install.packages("lubridate")
#install.packages("tsoutliers")
 
library(tidyverse) # data wrangling
library(tibble) # data wrangling
library(janitor) # column naming
library(countrycode) # country codes
library(lubridate) # date times
library(readxl) # excel import
library(zoo) # calculate rolling averages
library(R.utils) # R utilities
library(stringr) # to parse strings in R
library(tsoutliers) # remove outliers
library(dplyr) # data wrangling
 
### EXTRACT, TRANSFORM, LOAD -----
rm(list = ls())
## Set local file path names
WHO_REGIONS_PATH<-'../data/who_regions.csv'
FIND_TESTING_SEQ_RAW_PATH<- '../data/2021_04_04_FIND_capacity_mapping_data_sources.xlsx'
OWID_TESTING_RAW_PATH<- '../data/covid-19-testing-policy.csv'
ALL_DATA_PATH<- '../data/data_all.csv'
GISAID_RAW_METADATA_PATH<-'../data/GISAID_raw_metadata_10_18_2021.tsv'
PROVISION_PATH<-'../data/provision.csv'
VARIANT_CALL_PATH<-'../data/GISAID_update_10_29_weekly_file.csv'
ECONOMY_PATH<-'../data/CLASS.xls'

LAST_DATA_PULL_DATE<-as.Date("2021-10-18") # enter here "YYYY-10-18"
TIME_WINDOW <- 90

# adjust script so that _clean refers to data sets that are in the format country and the metric (i.e. do not contain time series)
# intermediates with time series are designated with _t


 
## WHO Region Background
 
# import csv of WHO region countries 
find_clean <- read.csv(WHO_REGIONS_PATH) %>%
# standardize names with this janitor function
  clean_names() %>%
  # rename columns name
  rename(name = country_name)
 
 
## WHO testing and sequencing capacity data
 
who_raw <- read_excel(FIND_TESTING_SEQ_RAW_PATH,
                              sheet = "Country classification",
                              skip = 1) %>%
# standardize names with this janitor function
  clean_names()

# select only testing capacity data
# remove buffer rows by iso code and select code and testing capacity columns
who_testing_clean <- who_raw %>%
  # remove rows with country code x
  filter(country_code != "x") %>%
  # remove rows with country code ""
  filter(country_code != "") %>%
  # remove rows with country code " "
  filter(country_code != " ") %>%
  # drop all columns except iso code and WHO testing capacity
  select(contains("code"), starts_with("testing_capacity_"))
 
# define who testing capacity column for later reference
who_testing_column <- colnames(who_testing_clean)[max(ncol(who_testing_clean))]
 
# Convert to binary testing capacity
who_testing_clean$who_testing_capacity <- case_when(
  # 0: "0 - No reliable testing capacity" or 0
  # 1: "1 - Reliable testing capacity" or 1
  who_testing_clean[ , who_testing_column] == 0 ~ 0,
  who_testing_clean[ , who_testing_column] == "0 - No reliable testing capacity" ~ 0,
  who_testing_clean[ , who_testing_column] == 1 ~ 1,
  who_testing_clean[ , who_testing_column] == "1 - Reliable testing capacity" ~ 1
) 
 
# select only code and who_testing_capacity variables
who_testing_clean <- who_testing_clean %>%
  select(contains("code"), matches("who_testing_capacity"))
 
# find_clean: merge WHO testing data into template
find_clean <- left_join(find_clean, who_testing_clean, by = c("code" = "country_code"))
 
## Our World In data testing policy data
# import data countaining country, DATE, and testing policy on that date (numbered 0 +) 
owid_testing_raw <- read.csv(OWID_TESTING_RAW_PATH) %>%
  # standardize names with this janitor function
  clean_names() %>%
  select(date, code = country_code, testing_policy = h2_testing_policy) %>%
  filter(is.na(testing_policy) == F)
 
# add dashes to date
owid_testing_raw$date <- paste(substr(owid_testing_raw$date, 1, 4), substr(owid_testing_raw$date, 5, 6), substr(owid_testing_raw$date, 7, 8), sep = '-')
 
# select most recent testing policy entry for each country and select country code and testing policy columns
owid_testing_clean <- owid_testing_raw %>%
  # group by country code
  group_by(code) %>%
  # select most recent entry for each country
  filter(as.Date(date) == max(as.Date(date)))
 
# deduplicate any repeated countries
owid_testing_clean <- owid_testing_clean[!duplicated(owid_testing_clean$code),]
 
# create owid_testing_capacity binary (0 or 1) variable 
owid_testing_clean <- owid_testing_clean %>%
  # 0: "No testing policy"
  # 1: any testing policy
  mutate(
     owid_testing_capacity = case_when(
      testing_policy >= 1 ~ 1,
      testing_policy == 0 ~ 0
    )
  ) %>%
  # select code and owid_testing_capacity columns
  select(code, owid_testing_capacity)
 
# find_clean: merge OWID testing capacity data into template
find_clean <- left_join(find_clean, owid_testing_clean, by = "code")
 
 
## FIND Testing Metric
 
# import csv from FIND containing country, date, population size, tests, cases, deaths (new and cumulative), 
# Q: What does the cap refer to?? Is it tests/cases/deaths per 100k? 
find_raw <- read.csv(ALL_DATA_PATH) %>%
# standardize names with this janitor function
  clean_names()
 
# select and rename necessary columns, selecting only those related to testing
find_testing_t <- find_raw %>%
  # filter for country set
  filter(set == "country") %>%
  # select time, code, new_tests_corrected, pop_100k, cap_new_tests, all_new_tests, 
  select(name, time, unit, pop_100k, cap_new_tests, cap_cum_tests, all_new_tests, all_new_cases, all_cum_cases, all_cum_tests) %>%
  # rename columns as date, code, pop_100k, new_tests_cap, new_tests_all
  rename(country = name, date = time, code= unit, pop_100k = pop_100k, new_tests_cap = cap_new_tests, new_tests_all = all_new_tests, 
         new_cases_all = all_new_cases, cap_cum_tests = cap_cum_tests, all_cum_cases = all_cum_cases, all_cum_tests = all_cum_tests) %>%
  # parse date as date class
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

#cap cum tests is number of cumulative tests per 100k 
 
# create country code column
find_testing_t <- find_testing_t %>%
  mutate(code = countrycode(country, origin = 'country.name', destination = 'iso3c'))
 
# INSERT A TEST TO PULL OUT THE COUNTRIES MISSING CODES???

# inserts missing country codes manually -- HOW DID WE KNOW THESE WERE MISSING?
find_testing_t$code[find_testing_t$country == "Kosovo"] <- "XKX"
find_testing_t$code[find_testing_t$country == "Namibia"] <- "NAM"

# CHECK THAT DATA SET HAS COMPLETED DATE TIME SERIES


# Find metrics for cases in the past TIME WINDOW days 
find_testing_recent<-find_testing_t%>%filter(date>=(LAST_DATA_PULL_DATE -TIME_WINDOW) & date<= LAST_DATA_PULL_DATE)%>%
  group_by(code)%>%
  summarise(recent_cases = sum(new_cases_all))

cases_in_last_7_days<-find_testing_t%>%filter(date>=(LAST_DATA_PULL_DATE - 6) & date <= LAST_DATA_PULL_DATE)%>%
  group_by(code)%>%
  summarise(cases_newly_reported_in_last_7_days_per_100000_population = sum(new_cases_all)/max(pop_100k))
 
# create variable for 30 day rolling average of new tests per capita
find_testing_t <- find_testing_t %>%
  # group rows by country code
  group_by(code) %>%
  # create column for 30 day rolling average
  mutate(
    new_tests_cap_avg = round(zoo::rollmean(new_tests_cap, 30, fill = NA),2)
  )
 
 
# display max 30 day rolling average of new tests per capita for each country
# removes time element so now just country and metrics
find_testing_clean <- find_testing_t %>%
  group_by(code) %>%
  summarise(
    max_new_tests_cap_avg = max(new_tests_cap_avg, na.rm = T),
    cap_cum_tests = max(cap_cum_tests, na.rm = T),
    cum_tpr = max(all_cum_cases, na.rm = T)/max(all_cum_tests, na.rm = T),
    pop_100k = max(pop_100k, na.rm = T)
  )
 
# add recent cases by joining
find_testing_clean<-left_join(find_testing_clean, find_testing_recent, by = "code")
find_testing_clean<-left_join(find_testing_clean, cases_in_last_7_days, by = "code")
 
# remove any -Inf
find_testing_clean$max_new_tests_cap_avg <- ifelse(find_testing_clean$max_new_tests_cap_avg < 0, NA, find_testing_clean$max_new_tests_cap_avg)
 
# remove any Inf
find_testing_clean$cum_tpr <- ifelse(find_testing_clean$cum_tpr < 0 | find_testing_clean$cum_tpr > 1, NA, find_testing_clean$cum_tpr)
 
# select code, pop_100k, cap_cum_tests, and max_new_tests_cap_avg columns
find_testing_clean <- find_testing_clean %>%
  select(code, pop_100k, max_new_tests_cap_avg, cap_cum_tests, cum_tpr, recent_cases, cases_newly_reported_in_last_7_days_per_100000_population)
 
# find_clean: merge FIND testing data into template
find_clean <- left_join(find_clean, find_testing_clean, by = "code")
 
# quartile division of max_new_tests_cap_avg
find_testing_quantile <- quantile(find_testing_clean$max_new_tests_cap_avg, na.rm = T)
 
## Dx Testing Capacity Classifier
 
# create Dx Testing Capacity Classifier variable
# Variable indicating highest level of diagnostic throughput (Low testing capacity, Medium testing capacity, High testing capacity)
# Variable is based on FIND testing dashboard
# If FIND source missing for that country, take source OWD
find_clean <- find_clean %>%
  # create new variable for Dx testing capacity based on:
  # FIND testing capacity
  # OWID testing Capacity
  mutate(
    # "0-49.99": < 50 max_new_tests_cap_avg in FIND
    # "50-99.99": >= 10 & < 100 max_new_tests_cap_avg in FIND
    # "100+": >= 100 max_new_tests_cap_avg in FIND
    # "0-49.99": NA in FIND
    dx_testing_capacity = case_when(
      max_new_tests_cap_avg >= 50 & who_testing_capacity == 1 ~ "Reliable testing capacity",
      max_new_tests_cap_avg >= 50 & who_testing_capacity == 0 ~ "Reliable testing capacity",
      max_new_tests_cap_avg >= 50 & is.na(who_testing_capacity) == T ~ "Reliable testing capacity",
      max_new_tests_cap_avg < 50 & who_testing_capacity == 1 ~ "Reliable testing capacity",
      max_new_tests_cap_avg < 50 & who_testing_capacity == 0 ~ "Unreliable testing capacity",
      max_new_tests_cap_avg < 50 & is.na(who_testing_capacity) == T ~ "Unreliable testing capacity",
      is.na(max_new_tests_cap_avg) == T & who_testing_capacity == 1 ~ "Reliable testing capacity",
      is.na(max_new_tests_cap_avg) == T & who_testing_capacity == 0 ~ "Unreliable testing capacity",
      is.na(max_new_tests_cap_avg) == T & is.na(who_testing_capacity) == T ~ "Unreliable testing capacity"
    )
  )
 
## WHO NGS Capacity + NGS Install Bases

#select the sequencing capacity column

 
# remove buffer rows by iso code and select code and ngs capacity columns
ngs_clean <- who_raw %>%
  # remove rows with country code x
  filter(country_code != "x") %>%
  # remove rows with country code ""
  filter(country_code != "") %>%
  # remove rows with country code " "
  filter(country_code != " ") %>%
  # drop all columns except iso code and NGS capacity
  select(contains("code"), starts_with("ngs_capacity_"))
 
# define NGS capacity column for later reference
ngs_capacity_column <- colnames(ngs_clean)[max(ncol(ngs_clean))]
 
# assign ngs_capacity variable a label
ngs_clean$ngs_capacity <- case_when(
  # 0: "0 - 0 NGS facilities" or 0
  # 1: "1 - 1-3 NGS facilities or equivalent" or 1
  # 2: "2 - 4+ NGS facilities or equivalent" or 2
  ngs_clean[ , ngs_capacity_column] == 0 ~ 0,
  ngs_clean[ , ngs_capacity_column] == "0 - 0 NGS facilities" ~ 0,
  ngs_clean[ , ngs_capacity_column] == 1 ~ 1,
  ngs_clean[ , ngs_capacity_column] == "1 - 1-3 NGS facilities or equivalent" ~ 1,
  ngs_clean[ , ngs_capacity_column] == 2 ~ 2,
  ngs_clean[ , ngs_capacity_column] == "2 - >3 NGS facilities or equivalent" ~ 2
)
 
# select only code and who_testing_capacity variables
ngs_clean <- ngs_clean %>%
  select(contains("code"), ends_with("ngs_capacity"))
 
# find_clean: merge WHO testing data into template
find_clean <- left_join(find_clean, ngs_clean, by = c("code" = "country_code"))
 
## Sequencing Capacity Classifier variable
 
# create Sequencing Capacity Classifier variable
# Variable indicating evidence of Sequencing Capacity
# Variable is based on WHO slides on facilities, GISRS data for extraction of capacity & testing data
# Variable is based on confidential manufacturer data on install bases
find_clean <- find_clean %>%
  # create new variable for Sequencing Capacity based on:
  # WHO slides, GISRS data
  # confidential manufacturer data on install bases
  mutate(
    # "0 NGS facilities or equivalent": 0 ngs_capacity
    # "1-3 NGS facilities or equivalent": 1 ngs_capacity
    # "4+ NGS facilities or equivalent": 2 ngs_capacity
    sequencing_capacity = case_when(
      ngs_capacity == 0 ~ "0 NGS facilities or equivalent",
      ngs_capacity == 1 ~ "1-3 NGS facilities or equivalent",
      ngs_capacity == 2 ~ "4+ NGS facilities or equivalent",
      is.na(ngs_capacity) == T ~ "0 NGS facilities or equivalent"
    )
  )
 
## Raw GISAID metadata
 
# import csv from GISAID (www.epicov.org)
gisaid_metadata_raw <- read.csv(GISAID_RAW_METADATA_PATH,
                                sep = '\t',
                                na.strings = c("", "?")) %>%
# standardize names with this janitor function
  clean_names()
 
# remove non human samples
gisaid_metadata_raw <- gisaid_metadata_raw %>%
  filter(host == "Human")
 
# separate location into continent, country, division, and location
gisaid_metadata_raw <- gisaid_metadata_raw %>%
  separate(location,
           into = c("continent", "country", "division", "location"),
           sep = " / | /|/ |/")
 
# select accession_id, collection_date, continent, country, division, location, pango_lineage, variant, submission_date column
gisaid_metadata_raw <- gisaid_metadata_raw %>%
  select(accession_id, collection_date, continent, country, division, location, pango_lineage, variant, submission_date)
 
# replace USA acronym with United States
gisaid_metadata_raw$country[gisaid_metadata_raw$country == "USA"] <- "United States"
 
# replace Usa acronym with United States
gisaid_metadata_raw$country[gisaid_metadata_raw$country == "Usa"] <- "United States"
 
# replace DC with District of Columbia
gisaid_metadata_raw$division[gisaid_metadata_raw$division == "DC"] <- "District of Columbia"
 
# capitalize first letter of country
gisaid_metadata_raw$country <- capitalize(gisaid_metadata_raw$country)
 
# correct mispelling
gisaid_metadata_raw$country[gisaid_metadata_raw$country == "Cote dIvoire"] <- "Cote d'Ivoire"
 
# correct mispelling
gisaid_metadata_raw$country[gisaid_metadata_raw$country == "Niogeria"] <- "Nigeria"
 
# correct mispelling
gisaid_metadata_raw$country[gisaid_metadata_raw$country == "Republic of the Congo"] <- "Congo"
 
# correct mispelling
gisaid_metadata_raw$country[gisaid_metadata_raw$country == "Czech republic"] <- "Czech Republic"
 
# correct misentry of Lithuania
gisaid_metadata_raw$country[gisaid_metadata_raw$country == "Jonavos apskritis"] <- "Lithuania"
 
# correct misreading
gisaid_metadata_raw$country[gisaid_metadata_raw$country == "M?xico"] <- "Mexico"
 
# assign dates missing a day to 15 of the month
gisaid_metadata_raw$collection_date <- ifelse(nchar(as.character(gisaid_metadata_raw$collection_date)) == 7, paste(as.character(gisaid_metadata_raw$collection_date), "15", sep = "-"), as.character(gisaid_metadata_raw$collection_date))
 
# parse collection dates as dates
# any observations with only year become NA
gisaid_metadata_raw$collection_date <- as.Date(as.character(gisaid_metadata_raw$collection_date), format = "%Y-%m-%d")
 
# parse submission dates as dates
gisaid_metadata_raw$submission_date <- as.Date(as.character(gisaid_metadata_raw$submission_date), format = "%Y-%m-%d")
 
# exclude submissions earlier than 2019-12-01
gisaid_metadata_raw <- gisaid_metadata_raw[gisaid_metadata_raw$collection_date >= as.Date("2019-12-01", format = "%Y-%m-%d"),]
 
# exclude submissions dated to the future
gisaid_metadata_raw <- gisaid_metadata_raw[gisaid_metadata_raw$collection_date <= as.Date(Sys.Date(), format = "%Y-%m-%d"),]
 
# generate country codes from GISAID country names
gisaid_metadata_raw$code <- countrycode(gisaid_metadata_raw$country, origin = 'country.name', destination = 'iso3c')
 
# find 30 day rolling average of sequencing numbers
# create variable for 30 day rolling average of new tests per capita
gisaid_metadata_t <- gisaid_metadata_raw %>%
  # group rows by country code
  group_by(code, collection_date) %>%
  # create column for 30 day rolling average
  summarise(n_new_sequences = n())

# set start date
first_date<-min(gisaid_metadata_t$collection_date, na.rm = TRUE)

# fill in all dates that no submissions occur
gisaid_metadata_t<-gisaid_metadata_t %>% complete(collection_date = seq.Date(first_date, LAST_DATA_PULL_DATE, by = "day"))

# replace NAs with 0s
gisaid_metadata_t$n_new_sequences[is.na(gisaid_metadata_t$n_new_sequences)]<-0

# find 90 day average of new sequences
gisaid_metadata_t <- gisaid_metadata_t %>%
  # group rows by country code
  group_by(code) %>%
  # create column for 90 day rolling average
  mutate(
    seq_cap_avg = round(zoo::rollmean(n_new_sequences, TIME_WINDOW, fill = NA),2)
  )

# Maximum 90 day average of new sequences
gisaid_max_seq<-gisaid_metadata_t%>%
  group_by(code)%>%
  summarise(max_average_new_seq = max(seq_cap_avg, na.rm = TRUE))


# find number of sequences in past X days
gisaid_metadata_recent<-gisaid_metadata_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE- TIME_WINDOW) & collection_date<= LAST_DATA_PULL_DATE)%>%
  group_by(code)%>%
  summarise(recent_sequences = sum(n_new_sequences))

gisaid_metrics<-left_join(gisaid_metadata_recent, gisaid_max_seq, by = "code")


# dataframe for total sequences in GISAID
gisaid_metadata_clean <- gisaid_metadata_raw %>%
  group_by(code) %>%
  summarise(total_sequences = n()
  ) %>%
  select(code, total_sequences)

 
# find_clean: merge GISAID metadata into template
find_clean <- left_join(find_clean, gisaid_metadata_clean, by = c("code" = "code"))

# add in variables for new metrics
find_clean <- left_join(find_clean, gisaid_metrics, by = "code")
 
# replace NA with 0 for total_sequences
find_clean$total_sequences <- ifelse(is.na(find_clean$total_sequences) == T, 0, find_clean$total_sequences)

# make new metrics using the cases in the past X days, sequences in the past X days, and max X day average of new sequences
find_clean<- find_clean%>%mutate(
  max_new_seq_cap_avg = max_average_new_seq/pop_100k,
  percent_of_recent_cases_sequenced = 100*recent_sequences/recent_cases,
  per_capita_seq_rate = recent_sequences/pop_100k
)
 




# # import csv from GISAID provision 
# gisaid_provision <- read.csv(PROVISION_PATH,
#                                 na.strings = c("", "?")) %>%
#   # standardize names with this janitor function
#   clean_names()
#  
# # parse dates as dates
# gisaid_provision$created <- as.Date(as.character(gisaid_provision$created), format = "%Y-%m-%d")
#  
# # generate country codes from GISAID country names
# gisaid_provision$code <- countrycode(gisaid_provision$country, origin = 'country.name', destination = 'iso3c')
#  
# # insert country code for Saint Martin
# gisaid_provision$code[gisaid_provision$country=="Saint Martin"] <- "MAF"
#  
# # insert country code for Guyana
# gisaid_provision$code[gisaid_provision$country=="Guyane"] <- "GUY"
#  
# # insert country code for Kosovo
# gisaid_provision$code[gisaid_provision$country=="Kosovo"] <- "XKX"
#  
# # select columns
# gisaid_provision <- gisaid_provision %>%
#   select(submission_count, code)
#  
# # find_clean: merge GISAID metadata into template
# find_clean <- left_join(find_clean, gisaid_provision, by = c("code" = "code"))
#  
# # replace NA with 0 for submission_count
# find_clean$submission_count <- ifelse(is.na(find_clean$submission_count) == T, 0, find_clean$submission_count)
#  
# # dataframe for median reporting lag for each country after 2021-01-01
# gisaid_metadata_clean <- gisaid_metadata_raw %>%
#    filter(collection_date >= as.Date("2021-01-01", format = "%Y-%m-%d")) %>%
#    group_by(code) %>%
#    mutate(median_reporting_lag = median(as.numeric(submission_date - collection_date,
#                                                            unit = "days"), na.rm = T),
#                                         total_sequences = n()
#    ) %>%
#    select(code, median_reporting_lag, total_sequences)
#  
# # display one row per country
# gisaid_metadata_clean <- gisaid_metadata_clean[!duplicated(gisaid_metadata_clean$code),]
#  
# # remove unnecessary columns and rows
# global_month_lag <- global_month_lag %>%
#    select(code, median_reporting_lag)
#  
# # save diff_mean_reporting_lag column name for later
# sequencing_reporting_lag_name <- colnames(global_month_lag)[2]
 
# define month to month comparison points for reporting lag
month_1_start <- as.Date("2020-01-01", format = "%Y-%m-%d")
month_1_end <- as.Date("2020-12-31", format = "%Y-%m-%d")
month_2_start <- as.Date("2021-01-01", format = "%Y-%m-%d")
month_2_end <- as.Date("2021-12-31", format = "%Y-%m-%d")
 
# dataframe for first month of interest reporting lag
global_month_1_lag <- gisaid_metadata_raw %>%
  filter(collection_date >= as.Date(month_1_start) &
           collection_date <= as.Date(month_1_end)) %>%
  group_by(code) %>%
  mutate(median_reporting_lag_month_1 = median(as.numeric(submission_date - collection_date,
                                                          unit = "days"), na.rm = T)
  ) %>%
  select(code, median_reporting_lag_month_1)
 
# display one row per country
global_month_1_lag <- global_month_1_lag[!duplicated(global_month_1_lag$code),]
 
# create dataframe for second month of interest reporting lag
global_month_2_lag <- gisaid_metadata_raw %>%
  filter(collection_date >= as.Date(month_2_start) &
           collection_date <= as.Date(month_2_end)) %>%
  group_by(code) %>%
  mutate(median_reporting_lag_month_2 = median(as.numeric(submission_date - collection_date,
                                                          unit = "days"), na.rm = T)
  ) %>%
  select(code, median_reporting_lag_month_2)
 
# display one row for each country
global_month_2_lag <- global_month_2_lag[!duplicated(global_month_2_lag$code),]
 
# join first and second month of interest data frames
global_month_lag <- full_join(global_month_2_lag, global_month_1_lag, by = "code")
 
# create reporting lag month to month difference column
global_month_lag$reporting_lag_diff <- as.numeric(global_month_lag$median_reporting_lag_month_2 - global_month_lag$median_reporting_lag_month_1)
 
# remove unnecessary columns and rows
global_month_lag <- global_month_lag %>%
  select(code, reporting_lag_diff)
 
# find_clean: merge GISAID metadata into template
find_clean <- left_join(find_clean, global_month_lag, by = c("code" = "code"))
 
 
## SARS-CoV-2 Sequencing Reporting Lag variable
 
# create SARS-CoV-2 Sequencing Reporting Lag variable
# Variable indicating the status of reporting lag for each country with submissions in GISAID
# find_clean <- find_clean %>%
#   # create new variable for SARS-CoV-2 Sequencing Reporting Lag based on:
#   # diff_mean_reporting_lag variable derived from GISAID metadata
#   mutate(
#     # "Steady or Decreasing": <= 0 diff_mean_reporting_lag
#     # "Increasing": >0 diff_mean_reporting_lag
#     # "Insufficient Reporting": NA diff_mean_reporting_lag
#     sequencing_reporting_lag = case_when(
#       is.na(reporting_lag_diff) == T ~ "Insufficient Reporting",
#       reporting_lag_diff <= 0 ~ "Steady or Decreasing",
#       reporting_lag_diff > 0 ~ "Increasing"
#     )
#   )
 
 
 
# ## GISAID data
#  
# # import csv from CTP (https://github.com/covid-tracking-collab/gisaid-variants/tree/main/data)
# #file_names <- file.info(list.files("/repos/gisaid-variants/data", full.names = T))
# gisaid_raw <- read.csv(VARIANT_CALL_PATH) %>%
# # standardize names with this janitor function
#   clean_names()
#  
# # generate country codes from GISAID country names
# gisaid_raw$country_code <- countrycode(gisaid_raw$gisaid_country, origin = 'country.name', destination = 'iso3c')
#  
# # generate country codes from OWID country names
# gisaid_raw$country_code_owid <- countrycode(gisaid_raw$owid_location, origin = 'country.name', destination = 'iso3c')
#  
# # inserts missing country codes
# gisaid_raw$country_code[gisaid_raw$country == "Micronesia (country)"] <- "FSM"
# gisaid_raw$country_code[gisaid_raw$country == "Timor"] <- "TLS"
# gisaid_raw$country_code[gisaid_raw$country == "Turks and Caicos Islands"] <- "TCA"
# gisaid_raw$country_code[gisaid_raw$country == "Nauru"] <- "NRU"
# gisaid_raw$country_code[gisaid_raw$country == "Kosovo"] <- "XKX"
# gisaid_raw$country_code[gisaid_raw$country == "Guernsey"] <- "GGY"
# gisaid_raw$country_code[gisaid_raw$country == "Falkland Islands"] <- "FLK"
#  
# # parse collection dates as dates
# # any observations with only year or year-month become NA
# gisaid_raw$collect_date <- as.Date(as.character(gisaid_raw$gisaid_collect_date), format = "%Y-%m-%d")
#  
# # parse submission dates as dates
# gisaid_raw$owid_date <- as.Date(as.character(gisaid_raw$owid_date), format = "%Y-%m-%d")
#  
# # create mergeable find_raw
# find_raw <- find_clean %>%
#   select(code, total_sequences, max_new_tests_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_availability, per_capita_seq_rate)
#  
# # merge gisaid_raw and find_raw
# gisaid_raw <- left_join(gisaid_raw, find_raw, by = c("country_code" = "code"))
#  
# # total_viable_sequences: cumulative sequences in GISAID per country
# gisaid_clean <- gisaid_raw %>%
#   group_by(country_code) %>%
#   mutate(
#     total_viable_sequences = sum(all_lineages, na.rm = T),
#     all_cases = sum(owid_new_cases, na.rm = T),
#     percent_cases_sequenced = round(100*(total_sequences/all_cases),2),
#     sequences_per_capita_per_100k = round((total_sequences/(owid_population/100000)),2)
#   )
#  
# # percent_cases_sequenced: cumulative sequences in GISAID per country collected since 2021-01-01/cumulative cases in OWID per country since 2021-01-01
# # gisaid_clean <- gisaid_clean %>%
# #   filter(owid_date >= as.Date("2021-01-01", format = "%Y-%m-%d")) %>%
# #   group_by(country_code) %>%
# #   mutate(
# #     viable_sequences_2021 = sum(all_lineages, na.rm = T),
# #     all_cases_2021 = sum(owid_new_cases, na.rm = T),
# #     percent_cases_sequenced = round(100*(viable_sequences_2021/all_cases_2021),2),
# #     sequences_per_capita_per_100k = round((viable_sequences_2021/(owid_population/100000)),2)
# #   )
#  
# # replace percent_cases_sequenced > 100 or NaN with NA
# gisaid_clean$percent_cases_sequenced[gisaid_clean$percent_cases_sequenced > 100 | gisaid_clean$percent_cases_sequenced == "NaN"] <- NA
#  
# # display one row for each country
# gisaid_clean <- gisaid_clean[!duplicated(gisaid_clean$country_code),]
#  
# # remove unnecessary columns and rows
# gisaid_clean <- gisaid_clean %>%
#   select(country_code, total_viable_sequences, all_cases, percent_cases_sequenced, sequences_per_capita_per_100k)
#  
# # sort alphabetically by country name
# gisaid_clean <- gisaid_clean[order(gisaid_clean$country_code),]
#  
# # find_clean: merge WHO testing data into template
# find_clean <- left_join(find_clean, gisaid_clean, by = c("code" = "country_code"))
 
 
## SARS-CoV-2 Sequencing Classifier variable
 
# Need to think about changing these

# create SARS-CoV-2 Sequencing Classifier variable
# Variable indicating how many submissions from each country appear in GISAID
find_clean <- find_clean %>%
  # create new variable for SARS-CoV-2 Sequencing based on:
  # submission_count variable derived from GISAID provision
  mutate(
    # "0 sequences": 0 submission_count | NA submission_count
    # "1-499 sequences": <500 submission_count
    # "500+ sequences": >= 500 submission_count
    sars_cov_2_sequencing = case_when(
      total_sequences >= 500 ~ "500+ sequences",
      total_sequences < 500 & total_sequences > 0 ~ "1-499 sequences",
      total_sequences == 0 ~ "0 sequences",
      is.na(total_sequences) == T ~ "0 sequences"
    )
  )
 
# ## WHO Case data (https://covid19.who.int/table)
#  
# # import csv from WHO (https://rockfound.box.com/s/qd08kckyuig42rytzncso22if726uajq)
# who_case_raw <- read.csv("/domino/datasets/local/who-cases/WHO_COVID-19_global_table_data.csv") %>%
# # standardize names with this janitor function
#   clean_names()
#  
# # remove first row Global count
# who_case_raw <- who_case_raw[2:nrow(who_case_raw),]
#  
# # turn row names into country column
# who_case_raw <- tibble::rownames_to_column(who_case_raw)
#  
# # add country code to WHO case data
# who_case_raw$country_code <- countrycode(who_case_raw$rowname, origin = 'country.name', destination = 'iso3c')
#  
# # inserts missing country codes
# who_case_raw$country_code[who_case_raw$rowname == "Kosovo[1]"] <- "XKX"
# who_case_raw$country_code[who_case_raw$rowname == "Saint Martin"] <- "MAF"
#  
# # select country code and cases_newly_reported_in_last_7_days_per_100000_population
# who_case_clean <- who_case_raw %>%
#   select(code = country_code, cases_newly_reported_in_last_7_days_per_100000_population = cases_newly_reported_in_last_7_days)
#  
# # find_clean: merge WHO case data into template
# find_clean <- left_join(find_clean, who_case_clean, by = "code")
 
 
## World Bank background data (https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups) (https://covid19.who.int/table)
 
# import xls from FIND sources (https://rockfound.box.com/s/3kj4wkmkgpktmb6z03r49nyta6lb64i5)
world_bank_background_raw <- read_excel(ECONOMY_PATH,
                                        sheet = "List of economies",
                                        skip = 5) %>%
# standardize names with this janitor function
  clean_names()
 
# remove buffer rows by iso code and select code and testing capacity columns
world_bank_background_clean <- world_bank_background_raw %>%
  # drop all columns except iso code and world_bank_economies
  select(4,7)
 
# rename columns code and world_bank_economies
colnames(world_bank_background_clean) <- c("code", "world_bank_economies")
 
# find_clean: merge WHO testing data into template
find_clean <- left_join(find_clean, world_bank_background_clean, by = c("code" = "code"))
 
 
## Archetype Classifier
 
# create Archetype Classifier variable
# Variable indicating archetype each country belongs
# Variable is based on Dx Testing Capacity Classifier, NGS Capacity Classifier, and SARS-CoV-2 Sequencing Classifier
# create new variable for archetype based on:
# Dx Testing Capacity Classifier (dx_testing_capacity)
# NGS Capacity Classifier (ngs_capacity)
# SARS-CoV-2 Sequencing Classifier (sars_cov_2_sequencing)
# WHO background data (world_bank_economies)
# "0 - High Income *": world_bank_economies "High Income" + code CHN, RUS
# "4 - Strengthen"
# "3 - Leverage"
# "2 - Connect"
# "1 - Test"
find_clean <- find_clean %>%
  mutate(
    archetype_full = case_when(
      ((world_bank_economies == "High income") ~ "0 - High Income*"),
      ((code == "CHN") ~ "0 - High Income*"),
      ((code == "RUS") ~ "0 - High Income*"),
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "4 - Strengthen",
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "1 - Test",
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "3 - Leverage",
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "1 - Test",
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "2 - Connect",
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "1 - Test",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "4 - Strengthen",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "1 - Test",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "3 - Leverage",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "1 - Test",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "2 - Connect",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "1 - Test",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "3 - Leverage",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "1 - Test",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "2 - Connect",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "1 - Test",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "2 - Connect",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "1 - Test",
      (dx_testing_capacity == "Unreliable testing capacity") ~ "1 - Test"
    )
  )
 
# clean archetype column
find_clean <- find_clean %>%
  mutate(
    archetype = case_when(
      archetype_full == "0 - High Income*" ~ "High Income*",
      archetype_full == "4 - Strengthen" ~ "Strengthen",
      archetype_full == "3 - Leverage" ~ "Leverage",
      archetype_full == "2 - Connect" ~ "Connect",
      archetype_full == "1 - Test" ~ "Test"
    )
  )
 
# label column
find_clean <- find_clean %>%
  mutate(
    label = case_when(
      archetype == "High Income*" ~ "",
      archetype == "Strengthen" ~ "? Build additional NGS capacity for scale-up",
      archetype == "Leverage" ~ "? Leverage existing NGS capacity",
      archetype == "Connect" ~ "? Set-up sample referral networks or build NGS capacity from scratch",
      archetype == "Test" ~ "? Increase diagnostic testing capacity"
    )
  )
 
# create Dx Testing Capacity Classifier clean variable
# Variable indicating highest level of diagnostic throughput (Low testing capacity, Medium testing capacity, High testing capacity)
# Variable is based on FIND testing dashboard
# If FIND source missing for that country, take source OWD
find_clean <- find_clean %>%
  # create new variable for Dx testing capacity based on:
  # FIND testing capacity
  # OWID testing Capacity
  mutate(
    # "0-49.99": < 50 max_new_tests_cap_avg in FIND
    # "50-99.99": >= 10 & < 100 max_new_tests_cap_avg in FIND
    # "100+": >= 100 max_new_tests_cap_avg in FIND
    # "0-49.99": NA in FIND
    # "High Income*": world_bank_economies == "High income"
    dx_testing_capacity_clean = case_when(
      world_bank_economies == "High income" ~ "High Income*",
      code == "CHN" ~ "High Income*",
      code == "RUS" ~ "High Income*",
      max_new_tests_cap_avg >= 50 & who_testing_capacity == 1 ~ "Reliable testing capacity",
      max_new_tests_cap_avg >= 50 & who_testing_capacity == 0 ~ "Reliable testing capacity",
      max_new_tests_cap_avg >= 50 & is.na(who_testing_capacity) == T ~ "Reliable testing capacity",
      max_new_tests_cap_avg < 50 & who_testing_capacity == 1 ~ "Reliable testing capacity",
      max_new_tests_cap_avg < 50 & who_testing_capacity == 0 ~ "Unreliable testing capacity",
      max_new_tests_cap_avg < 50 & is.na(who_testing_capacity) == T ~ "Unreliable testing capacity",
      is.na(max_new_tests_cap_avg) == T & who_testing_capacity == 1 ~ "Reliable testing capacity",
      is.na(max_new_tests_cap_avg) == T & who_testing_capacity == 0 ~ "Unreliable testing capacity",
      is.na(max_new_tests_cap_avg) == T & is.na(who_testing_capacity) == T ~ "Unreliable testing capacity"
    )
  )
 
# create Sequencing Capacity Classifier clean variable
# Variable indicating evidence of Sequencing Capacity
# Variable is based on WHO slides on facilities, GISRS data for extraction of capacity & testing data
# Variable is based on confidential manufacturer data on install bases
find_clean <- find_clean %>%
  # create new variable for Sequencing Capacity based on:
  # WHO slides, GISRS data
  # confidential manufacturer data on install bases
  mutate(
    # "0 NGS facilities or equivalent": 0 ngs_capacity
    # "1-3 NGS facilities or equivalent": 1 ngs_capacity
    # "4+ NGS facilities or equivalent": 2 ngs_capacity
    sequencing_capacity_clean = case_when(
      world_bank_economies == "High income" ~ "High Income*",
      code == "CHN" ~ "High Income*",
      code == "RUS" ~ "High Income*",
      ngs_capacity == 0 ~ "0 NGS facilities or equivalent",
      ngs_capacity == 1 ~ "1-3 NGS facilities or equivalent",
      ngs_capacity == 2 ~ "4+ NGS facilities or equivalent",
      is.na(ngs_capacity) == T ~ "0 NGS facilities or equivalent"
    )
  )
  
# create SARS-CoV-2 Sequencing Classifier clean variable
# Variable indicating how many submissions from each country appear in GISAID
find_clean <- find_clean %>%
  # create new variable for SARS-CoV-2 Sequencing based on:
  # submission_count variable derived from GISAID provision
  mutate(
    # "0 sequences": 0 submission_count | NA submission_count
    # "1-499 sequences": <500 submission_count
    # "500+ sequences": >= 500 submission_count
    sars_cov_2_sequencing_clean = case_when(
      world_bank_economies == "High income" ~ "High Income*",
      code == "CHN" ~ "High Income*",
      code == "RUS" ~ "High Income*",
      total_sequences >= 500 ~ "500+ sequences",
      total_sequences < 500 & total_sequences > 0 ~ "1-499 sequences",
      total_sequences == 0 ~ "0 sequences",
      is.na(total_sequences) == T ~ "0 sequences"
    )
  )
 
## Clean Archetype Classifier (without HIC archetype)
 
# create Archetype Classifier variable
# Variable indicating archetype each country belongs
# Variable is based on Dx Testing Capacity Classifier, NGS Capacity Classifier, and SARS-CoV-2 Sequencing Classifier
# create new variable for archetype based on:
# Dx Testing Capacity Classifier (dx_testing_capacity)
# NGS Capacity Classifier (ngs_capacity)
# SARS-CoV-2 Sequencing Classifier (sars_cov_2_sequencing)
# "4 - Strengthen"
# "3 - Leverage"
# "2 - Connect"
# "1 - Test"
find_clean <- find_clean %>%
  mutate(
    archetype_clean = case_when(
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "Strengthen",
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "Test",
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "Leverage",
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "Test",
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "Connect",
      (sequencing_capacity == "4+ NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "Test",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "Strengthen",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "Test",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "Leverage",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "Test",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "Connect",
      (sequencing_capacity == "1-3 NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "Test",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "Leverage",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "500+ sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "Test",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "Connect",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "1-499 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "Test",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Reliable testing capacity") ~ "Connect",
      (sequencing_capacity == "0 NGS facilities or equivalent" & sars_cov_2_sequencing == "0 sequences" & dx_testing_capacity == "Unreliable testing capacity") ~ "Test",
      (dx_testing_capacity == "Unreliable testing capacity") ~ "Test"
    )
  )
 
# rename name column as country column
colnames(find_clean)[1] <- "country"

# ZS add deduplication -- THIS IS NOT ROBUST, if there are two *different* rows for a
# given country this will not fix the duplication. At the moment not a big deal bc all
# are NA, but I don't know the generating process for the missingness so may present an
# issue in a future dataset
find_clean <- find_clean %>% 
  filter(code != '') %>% # NAs stored as empty strings, not NAs
  unique()
 
# alphabetize find_clean by country
find_clean <- find_clean[order(find_clean$country),]
 
# add geometry variable as first column
find_clean <- cbind(geometry=NA, find_clean)
 
 
# add padding to who_testing_capacity
find_clean <- find_clean %>%
  mutate(
    who_testing_capacity = case_when(
      who_testing_capacity == 1 ~ "1 - Reliable",
      who_testing_capacity == 0 ~ "0 - Unreliable"
    )
  )
 
# add padding to sequencing_capacity
find_clean <- find_clean %>%
  mutate(
    sequencing_capacity = case_when(
      sequencing_capacity == "4+ NGS facilities or equivalent" ~ "2 - 4+ NGS facilities or equivalent",
      sequencing_capacity == "1-3 NGS facilities or equivalent" ~ "1 - 1-3 NGS facilities or equivalent",
      sequencing_capacity == "0 NGS facilities or equivalent" ~ "0 - 0 NGS facilities or equivalent"
    )
  )
 
# export find_clean to csv
write.csv(find_clean, "../data/find_map.csv", na = "NaN", row.names = FALSE)



# export find_clean Global to csv
write.csv(find_clean %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_average_new_seq, max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_global.csv", na = "NaN", row.names = FALSE)
 
# export find_clean Africa to csv
write.csv(find_clean %>%
            filter(region == "Africa") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_average_new_seq, max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_africa.csv", na = "NaN", row.names = FALSE)
 
# export find_clean Americas to csv
write.csv(find_clean %>%
            filter(region == "Americas") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_average_new_seq, max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_americas.csv", na = "NaN", row.names = FALSE)
 
# export find_clean Eastern Mediterranean to csv
write.csv(find_clean %>%
            filter(region == "Eastern Mediterranean") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_average_new_seq, max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_eastern_mediterranean.csv", na = "NaN", row.names = FALSE)
 
# export find_clean Europe to csv
write.csv(find_clean %>%
            filter(region == "Europe") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_average_new_seq, max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "/../data/find_table_europe.csv", na = "NaN", row.names = FALSE)
 
# export find_clean South-East Asia to csv
write.csv(find_clean %>%
            filter(region == "South-East Asia") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_average_new_seq, max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_south_east_asia.csv", na = "NaN", row.names = FALSE)
 
# export find_clean Western Pacific to csv
write.csv(find_clean %>%
            filter(region == "Western Pacific") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_average_new_seq, max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_western_pacific.csv", na = "NaN", row.names = FALSE)