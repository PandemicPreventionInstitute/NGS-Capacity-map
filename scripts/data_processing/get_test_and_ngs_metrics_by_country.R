# Original Author: Nathan Poland
# Updated: Briana Thrift & Zach Susswein
# Current Author: Kaitlyn Johnson

# Date updated: 11-29-2021

# This script takes in the GISAID metadata and OWID and find data and finds the recent cases, tests, and sequences
# It will be used to put the Omicron sequencing data in context
rm(list = ls())
USE_CASE = Sys.getenv("USE_CASE")
if(USE_CASE == ""){
  USE_CASE<-'local'
}
#USE_CASE = 'domino' # options: 'local' or 'domino'



#---- Libraries----------
if (USE_CASE == 'domino'){
install.packages("tidyverse", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
install.packages("janitor", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
install.packages("tibble", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
install.packages("countrycode", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
install.packages("lubridate", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("readxl", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("zoo", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("R.utils", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("stringr", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("tsoutliers", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("dplyr", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("scales", dependencies=TRUE, repos='http://cran.us.r-project.org')
}


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
library(scales) # comma formatting
 
# ------ Name data paths and set parameters -------------------------------------------
today <- substr(lubridate::now('EST'), 1, 13)
today <- chartr(old = ' ', new = '-', today)
today_date<-lubridate::today('EST')
#today<-"2021-12-22-13"

## Set filepaths
ALL_DATA_PATH<- url("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv")

if (USE_CASE == 'domino'){

GISAID_DAILY_PATH<-'/mnt/data/processed/gisaid_owid_merged.csv' # output from gisaid_metadata_processing.R
SEQUENCES_LAST_30_DAYS<-'/mnt/data/processed/sequences_last_30_days.csv'
SHAPEFILES_FOR_FLOURISH_PATH <- '/mnt/data/static/geometric_country_code_name_master_file.txt'
LAT_LONG_FOR_FLOURISH_PATH<-'/mnt/data/static/country_lat_long_names.csv'
WHO_REGIONS_PATH<-'/mnt/data/static/who_regions.csv'
ECONOMY_PATH<-'/mnt/data/static/CLASS.xls'
OWID_TESTING_POLICY_PATH<- '/mnt/data/static/covid-19-testing-policy.csv'
FIND_TESTING_SEQ_RAW_PATH<- '/mnt/data/static/2021_04_04_FIND_capacity_mapping_data_sources.xlsx'
}

if (USE_CASE == 'local'){

GISAID_DAILY_PATH<-'../../data/processed/gisaid_owid_merged.csv' # output from gisaid_metadata_processing.R
SEQUENCES_LAST_30_DAYS<-'../../data/processed/sequences_last_30_days.csv'
SHAPEFILES_FOR_FLOURISH_PATH <- '../../data/static/geometric_country_code_name_master_file.txt'
LAT_LONG_FOR_FLOURISH_PATH<-'../../data/static/country_lat_long_names.csv'
WHO_REGIONS_PATH<-'../../data/static/who_regions.csv'
ECONOMY_PATH<-'../../data/static/CLASS.xls'
OWID_TESTING_POLICY_PATH<- '../../data/static/covid-19-testing-policy.csv'
FIND_TESTING_SEQ_RAW_PATH<- '../../data/static/2021_04_04_FIND_capacity_mapping_data_sources.xlsx'
}

LAST_DATA_PULL_DATE<-as.Date(substr(lubridate::now('EST'), 1, 10))-days(1) # Make this based off of yesterday!
FIRST_DATE<-"2019-12-01"
TIME_WINDOW <- 29 # since we will include the reference data
TIME_WINDOW_WEEK<- 6 # since will include the reference date
TIME_SERIES_WINDOW<- 89 # last 90 days?
TIME_WINDOW_MAX_PREV<-89
TIME_WINDOW_YEAR<-364
CONF_LEVEL<-0.95 # confidence we want to be that a variant is not greater than our estimated prevalence
# that has been detected at least once (i.e. what variant prevalence would we be able to detect?)

# adjusted script so that _clean refers to data sets that are in the format country and the metric (i.e. do not contain time series)
# intermediates with time series are designated with _t




# ------ WHO countries, regions, and code ---------------------------------------------

# import csv of WHO region countries 
find_clean <- read.csv(WHO_REGIONS_PATH) %>%
  # standardize names with this janitor function
  clean_names() %>%
  # rename columns name
  rename(name = country_name)
# All additional variables will be joined to these





# ------ Categorical testing and sequencing capacity data -------------------------------------

test_seq_raw <- read_excel(FIND_TESTING_SEQ_RAW_PATH,
                           sheet = "Country classification",
                           skip = 1) %>%
  # standardize names with this janitor function
  clean_names()%>%
  # remove rows with country code x
  filter(country_code != "x") %>%
  # remove rows with country code ""
  filter(country_code != "") %>%
  # remove rows with country code " "
  filter(country_code != " ")

# select only testing or sequencing capacity data
testing_clean <- test_seq_raw %>%
  # drop all columns except iso code and WHO testing capacity
  select(contains("code"), starts_with("testing_capacity_"))
ngs_clean <- test_seq_raw %>%
  # drop all columns except iso code and NGS capacity
  select(contains("code"), starts_with("ngs_capacity_"))

# define  capacity column for later reference
ngs_capacity_column <- colnames(ngs_clean)[max(ncol(ngs_clean))]
testing_column <- colnames(testing_clean)[max(ncol(testing_clean))]

# print column names so that user knows when these were last updated 
print(ngs_capacity_column)
print(testing_column)


# assign ngs_capacity variable a label
ngs_clean$ngs_capacity <- case_when(
  # 0: "0 - 0 NGS facilities" or 0
  # 1: "1 - 1-3 NGS facilities or equivalent" or 1
  # 2: "2 - 4+ NGS facilities or equivalent" or 2
  ngs_clean[ , ngs_capacity_column] == 0 ~ 0,
  ngs_clean[ , ngs_capacity_column] == "0 - No NGS facilities" ~ 0,
  ngs_clean[ , ngs_capacity_column] == 1 ~ 1,
  ngs_clean[ , ngs_capacity_column] == "1 - 1-3 NGS facilities or equivalent" ~ 1,
  ngs_clean[ , ngs_capacity_column] == 2 ~ 2,
  ngs_clean[ , ngs_capacity_column] == "2 - >3 NGS facilities or equivalent" ~ 2
)

# Convert to binary testing capacity
testing_clean$who_testing_capacity <- case_when(
  # 0: "0 - No reliable testing capacity" or 0
  # 1: "1 - Reliable testing capacity" or 1
  testing_clean[ , testing_column] == 0 ~ 0,
  testing_clean[ , testing_column] == "0 - No reliable testing capacity" ~ 0,
  testing_clean[ , testing_column] == 1 ~ 1,
  testing_clean[ , testing_column] == "1 - Reliable testing capacity" ~ 1
) 


# select only code and capacity variables
ngs_clean <- ngs_clean %>%
  select(contains("code"), ends_with("ngs_capacity"))
testing_clean <- testing_clean %>%
  select(contains("code"), matches("who_testing_capacity"))

# find_clean: merge testing and sequencing capacity data into template
find_clean <- left_join(find_clean, ngs_clean, by = c("code" = "country_code"))
find_clean <- left_join(find_clean, testing_clean, by = c("code" = "country_code"))


# ------- Sequencing Capacity Classifier variable

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



# ------ FIND testing data to estimate testing metric -------------------------------------
 
# import csv from FIND containing country, date, population size, tests, 
find_raw <- read.csv(ALL_DATA_PATH) %>%
# standardize names with this janitor function
  clean_names()
 
# select and rename necessary columns, selecting only those related to testing
find_testing_t <- find_raw %>%
  # filter for country set
  filter(set == "country") %>%
  # select time, code, new_tests_corrected, pop_100k, cap_new_tests, all_new_tests, all_cum_cases
  select(name, time, unit, pop_100k, cap_new_tests, cap_cum_tests, all_new_tests, all_cum_tests, all_cum_cases, all_new_cases) %>%
  # rename columns as date, code, pop_100k, new_tests_cap, new_tests_all
  rename(country = name, date = time, code= unit, pop_100k = pop_100k, new_tests_cap = cap_new_tests, new_tests_all = all_new_tests, 
          cap_cum_tests = cap_cum_tests, all_cum_tests = all_cum_tests, all_cum_cases = all_cum_cases, new_cases_all = all_new_cases) %>%
  # parse date as date class
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         pop = pop_100k*100000)

#cap cum tests is number of cumulative tests per 100k 
 
# create country code column
find_testing_t <- find_testing_t %>%
  mutate(code = countrycode(country, origin = 'country.name', destination = 'iso3c'))
 
# INSERT A TEST TO PULL OUT THE COUNTRIES MISSING CODES???

# inserts missing country codes manually -- HOW DID WE KNOW THESE WERE MISSING?
find_testing_t$code[find_testing_t$country == "Kosovo"] <- "XKX"
find_testing_t$code[find_testing_t$country == "Namibia"] <- "NAM"

# CHECK THAT DATA SET HAS COMPLETED DATE TIME SERIES
# set start date
first_date<-min(find_testing_t$date, na.rm = TRUE)
date <- seq.Date(first_date, LAST_DATA_PULL_DATE, by = "day")
code <-unique(find_testing_t$code)
date_country<-expand_grid(date, code)
find_testing_t<-left_join(date_country,find_testing_t, by = c("code", "date"))

# Fill in the NAs on the values 
find_testing_t$new_tests_cap[is.na(find_testing_t$new_tests_cap)]<-0
find_testing_t$new_tests_all[is.na(find_testing_t$new_tests_all)]<-0



# create variable for 30 day rolling average of new tests per capita
find_testing_t <- find_testing_t %>%
  # group rows by country code
  group_by(code) %>%
  # create column for 30 day rolling average
  mutate(
    new_tests_cap_avg = round(zoo::rollmean(100000*new_tests_all/pop, 30, fill = NA),2)
  )

 


 
# display max 30 day rolling average of new tests per capita for each country
# removes time element so now just country and metrics
find_testing_clean <- find_testing_t %>%
  group_by(code) %>%
  summarise(
    max_new_tests_cap_avg = max(new_tests_cap_avg, na.rm = T),
    cap_cum_tests = max(cap_cum_tests, na.rm = T),
    cum_tpr = max(all_cum_cases, na.rm = T)/max(all_cum_tests, na.rm = T),
    total_tests = sum(new_tests_cap)
  )

# Find the countries where they have no tests
no_tests<-find_testing_clean%>%filter(total_tests==0)
no_tests<-left_join(no_tests, find_clean)%>%select(name, code, total_tests, who_testing_capacity, ngs_capacity)
if (USE_CASE=='local'){
write.csv(no_tests, '../../data/processed/countries_w_no_FIND_tests.csv')
}
if (USE_CASE == 'domino'){
write.csv(no_tests, '/mnt/data/processed/countries_w_no_FIND_tests.csv')
}


# Add in recent testing capacity metrics ie average test positivity over past 90 days
find_testing_90_days<- find_testing_t %>% filter(date>=(LAST_DATA_PULL_DATE -TIME_WINDOW) & 
                                                   date<= LAST_DATA_PULL_DATE)%>%
  group_by(code) %>%
  summarise(tests_in_last_90_days = sum(new_tests_all))

# Add in recent testing capacity metrics ie average test positivity over past 7 days
find_testing_7_days<- find_testing_t %>% filter(date>=(LAST_DATA_PULL_DATE -TIME_WINDOW_WEEK) & 
                                                  date<= LAST_DATA_PULL_DATE)%>%
  group_by(code) %>%
  summarise(tests_in_last_7_days = sum(new_tests_all))

find_testing_recent<-left_join(find_testing_90_days, find_testing_7_days, by = "code")



# Add in recent testing capacity metrics ie average test positivity over past 90 days
find_testing_last_year<- find_testing_t %>% filter(date>=(LAST_DATA_PULL_DATE -TIME_WINDOW_YEAR) & 
                                                     date<= LAST_DATA_PULL_DATE)%>%
  group_by(code) %>%
  summarise(tests_in_last_year = sum(new_tests_all),
            cases_in_last_year_find = sum(new_cases_all),
            tpr_year_find = cases_in_last_year_find/tests_in_last_year,
            avg_daily_test_per_1000_last_year = 1000*mean(new_tests_all)/max(pop),
            median_daily_tests_per_1000_last_year = 1000*median(new_tests_all/max(pop)))

find_testing_recent<-left_join(find_testing_recent, find_testing_last_year, by = "code")

find_testing_clean<-left_join(find_testing_clean, find_testing_recent, by = "code")


# remove any -Inf
find_testing_clean$max_new_tests_cap_avg <- ifelse(find_testing_clean$max_new_tests_cap_avg < 0, NA, find_testing_clean$max_new_tests_cap_avg)

# remove any Inf
find_testing_clean$cum_tpr <- ifelse(find_testing_clean$cum_tpr < 0 | find_testing_clean$cum_tpr > 1, NA, find_testing_clean$cum_tpr)

# select code, pop_100k, cap_cum_tests, and max_new_tests_cap_avg columns
find_testing_clean <- find_testing_clean %>%
  select(code, max_new_tests_cap_avg, cap_cum_tests, cum_tpr)

# merge the find_testing dataframes
find_testing_clean<-left_join(find_testing_clean, find_testing_recent, by = "code")

# find_clean: merge FIND testing data into template
find_clean <- left_join(find_clean, find_testing_clean, by = "code")

#------Dx Testing Capacity Classifier------------------

# create Dx Testing Capacity Classifier variable (WILL EDIT THIS!)
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


 

# -------- GISAID data on SARS-CoV-2 sequences by country and time --------------------------------

gisaid_raw <- read.csv(GISAID_DAILY_PATH) %>% # raw refers to all variants, by country, by day
  # standardize names with this janitor function
  clean_names()

# parse collection dates as dates (note this uses the imputed day 15 from metadata processing script)
gisaid_raw$collection_date <- as.Date(as.character(gisaid_raw$gisaid_collect_date), format = "%Y-%m-%d")

gisaid_t <- gisaid_raw%>%select(collection_date, gisaid_country, n_new_sequences,
                                         owid_new_cases, owid_population, country_code, owid_location)

# find 7 day average of new sequences
gisaid_t <- gisaid_t %>%
  # group rows by country code
  group_by(country_code) %>%
  # create column for 7 day rolling average
  mutate(
    seq_7davg = round(zoo::rollmean(n_new_sequences, 7, fill = NA),2),
    rolling_cases_last_30_days = rollapplyr(owid_new_cases,30,sum, partial = TRUE, align = "right"),
    rolling_cases_last_7_days = rollapplyr(owid_new_cases, 7, sum, partial = TRUE, align = "right"),
    #rolling_seq_last_30_days = rollapplyr(n_new_sequences, 30, sum, partial = TRUE, align = "right"),
    #percent_of_cases_seq_last_30_days = 100*rolling_seq_last_30_days/rolling_cases_last_30_days
  )


gisaid_t <- gisaid_t %>%filter(collection_date>=FIRST_DATE & 
                                 collection_date<= LAST_DATA_PULL_DATE)
# Make sure that the most recent date is yesterday
stopifnot("GISAID metadata run isnt up to date" = max(gisaid_t$collection_date) == (today_date - days(1)))

# find 90 day average of new sequences
gisaid_t <- gisaid_t %>%
  # group rows by country code
  group_by(country_code) %>%
  # create column for 90 day rolling average
  mutate(
    seq_avg = round(zoo::rollmean(n_new_sequences, 90, fill = NA),2)
  )

gisaid_max_seq<-gisaid_t%>%
  group_by(country_code)%>%
  summarise(max_new_seq_cap_avg = 100000*max(seq_avg, na.rm = TRUE)/max(owid_population),
            total_sequences = sum(n_new_sequences, na.rm = TRUE))

# Subset to only recent data to get recent sequences and cases by country
gisaid_recent_data<-gisaid_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE -89) & 
                                        collection_date<= LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(cases_in_last_90_days = sum(owid_new_cases, na.rm = TRUE),
            sequences_in_last_90_days = sum(n_new_sequences, na.rm = TRUE),
            population_size = max(owid_population, na.rm = TRUE),
            cases_per_100k_last_90_days = 100000*sum(owid_new_cases, na.rm = TRUE)/max(owid_population, na.rm = TRUE))

# Join the 90 day and overall 
gisaid_recent_data<-left_join(gisaid_recent_data, gisaid_max_seq)

# replaces NAs with 0
gisaid_recent_data$sequences_in_last_90_days[is.na(gisaid_recent_data$sequences_in_last_90_days)]<-0
gisaid_recent_data<-gisaid_recent_data%>%
  mutate(percent_of_cases_sequenced_last_90_days = 100*sequences_in_last_90_days/cases_in_last_90_days,
         per_capita_seq_rate_in_last_90_days = 100000*sequences_in_last_90_days/population_size,
         max_prevalence_variant_pct = 100*(1-((1-CONF_LEVEL)^(1/sequences_in_last_90_days))))

# replace percent_cases_sequenced > 100 or NaN with NA
gisaid_recent_data$percent_of_cases_sequenced_last_90_days[gisaid_recent_data$percent_of_cases_sequenced_last_90_days > 100 | gisaid_recent_data$percent_of_cases_sequenced_last_90_days == "NaN"] <- NA

# Subset to last 7 days of data
cases_in_last_7_days<-gisaid_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE - TIME_WINDOW_WEEK) & 
                                          collection_date<=LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(cases_in_last_7_days = sum(owid_new_cases, na.rm = TRUE),
    cases_per_100k_last_7_days = round(100000*sum(owid_new_cases)/max(owid_population, na.rm = TRUE), 1))

# join with 30 day summary 
gisaid_recent_data<-left_join(gisaid_recent_data, cases_in_last_7_days, by = "country_code")

gisaid_last_year<-gisaid_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE -TIME_WINDOW_YEAR) & 
                                      collection_date<= LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(cases_in_last_year = sum(owid_new_cases),
            sequences_in_last_year= sum(n_new_sequences))

gisaid_recent_data<-left_join(gisaid_recent_data, gisaid_last_year, by = "country_code")

# Join both sets of metrics 
find_clean <- left_join(find_clean, gisaid_recent_data, by = c("code" = "country_code"))
find_clean<-find_clean%>%mutate(
  tpr_90_days = cases_in_last_90_days/tests_in_last_90_days,
  tpr_7_days = cases_in_last_7_days/tests_in_last_7_days,
  test_in_the_last_7_days_per_100k = 100000*tests_in_last_7_days/population_size,
  tpr_year = cases_in_last_year/tests_in_last_year
)

find_clean$tpr_90_days[find_clean$tpr_90_days == Inf] <- NA
find_clean$tpr_year[find_clean$tpr_year == Inf] <- NA





# ------ World Bank Economy classifier ------------------------------------------------
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





# -------- SARS-CoV-2 sequencing capacity classifier original -----------------------
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



# -------- Create Original Archetype classifier variable ----------------------------------------

print("FLAG: Eventually we will just change these, but for now, keep as is")
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
# find_clean <- find_clean %>%
#   mutate(
#     sequencing_capacity = case_when(
#       sequencing_capacity == "4+ NGS facilities or equivalent" ~ "2 - 4+ NGS facilities or equivalent",
#       sequencing_capacity == "1-3 NGS facilities or equivalent" ~ "1 - 1-3 NGS facilities or equivalent",
#       sequencing_capacity == "0 NGS facilities or equivalent" ~ "0 - 0 NGS facilities or equivalent"
#     )
#   )

# export find_clean to csv

# Join shapefiles! 
shapefile <- read_delim(SHAPEFILES_FOR_FLOURISH_PATH, delim = "\t") %>%
  rename(code = `3-letter ISO code`) %>%
  select(geometry, Name, code)

find_clean_flourish<-left_join(shapefile, find_clean, by = "code")

if (USE_CASE == 'local'){
  write.csv(find_clean, "../../data/processed/find_clean.csv", na = "NaN", row.names = FALSE)
  write.csv(find_clean_flourish, "../../data/processed/find_map.csv", na = "NaN", row.names = FALSE)
}

if (USE_CASE == 'domino'){
  write.csv(find_clean, "/mnt/data/processed/find_clean.csv", na = "NaN", row.names = FALSE)
  write.csv(find_clean_flourish, "/mnt/data/processed/find_map.csv", na = "NaN", row.names = FALSE)
}





