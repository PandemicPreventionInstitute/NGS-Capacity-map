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
OLD_FIND_MAP_PATH<-url("https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/November_2021/find_map_11.30.2021.csv")

if (USE_CASE == 'domino'){

GISAID_DAILY_PATH<-'/mnt/data/processed/gisaid_owid_merged.csv' # output from gisaid_metadata_processing.R
SHAPEFILES_FOR_FLOURISH_PATH <- '/mnt/data/static/geometric_country_code_name_master_file.txt'
LAT_LONG_FOR_FLOURISH_PATH<-'/mnt/data/static/country_lat_long_names.csv'
WHO_REGIONS_PATH<-'/mnt/data/static/who_regions.csv'
ECONOMY_PATH<-'/mnt/data/static/CLASS.xls'
FIND_TESTING_SEQ_RAW_PATH<- '/mnt/data/static/2021_04_04_FIND_capacity_mapping_data_sources.xlsx'
}

if (USE_CASE == 'local'){

GISAID_DAILY_PATH<-'../../data/processed/gisaid_owid_merged.csv' # output from gisaid_metadata_processing.R
SEQUENCES_LAST_30_DAYS<-'../../data/processed/sequences_last_30_days.csv'
SHAPEFILES_FOR_FLOURISH_PATH <- '../../data/static/geometric_country_code_name_master_file.txt'
LAT_LONG_FOR_FLOURISH_PATH<-'../../data/static/country_lat_long_names.csv'
WHO_REGIONS_PATH<-'../../data/static/who_regions.csv'
ECONOMY_PATH<-'../../data/static/CLASS.xls'
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

ngs_clean<-ngs_clean%>%mutate(
  facility_access = case_when(
    ngs_capacity ==0 ~ "No NGS facilities",
    (ngs_capacity ==1 | ngs_capacity == 2) ~ "1 or more NGS facilities"
  
))

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
  select(contains("code"),  ngs_capacity, facility_access)
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
find_raw <- read.csv(ALL_DATA_PATH) %>%clean_names()
 
# select and rename necessary columns, selecting only those related to testing
find_testing_t <- find_raw %>%
  # filter for country set
  filter(set == "country") %>%
  # select time, code, new_tests_corrected, pop_100k, cap_new_tests, all_new_tests, all_cum_cases
  select(name, time, unit, pop_100k, all_new_tests, all_cum_tests, all_cum_cases, all_new_cases, 
         pos, new_tests_orig, new_cases_orig) %>%
  # rename columns as date, code, pop_100k, new_tests_smoothed, cum_tests, cum_cases, new_cases_smoothed
  rename(country = name, date = time, code= unit, new_tests_smoothed = all_new_tests, 
           cum_tests = all_cum_tests, cum_cases = all_cum_cases, new_cases_smoothed = all_new_cases) %>%
  # parse date as date class
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         pop = pop_100k*100000, # get full pop
         code = countrycode(country, origin = 'country.name', destination = 'iso3c')) # make country code


# Add a column that has the new cases smoothed with NA entries when new_tests_smoothed is NA
find_testing_t['new_cases_smoothed_truncated']<-find_testing_t$new_cases_smoothed
find_testing_t$new_cases_smoothed_truncated[is.na(find_testing_t$new_tests_smoothed)]<-NA


# inserts missing country codes manually 
find_testing_t$code[find_testing_t$country == "Kosovo"] <- "XKX"
find_testing_t$code[find_testing_t$country == "Namibia"] <- "NAM"

# FILL IN MISSING DATES
# set start date
first_date<-min(find_testing_t$date, na.rm = TRUE)
date <- seq.Date(first_date, LAST_DATA_PULL_DATE, by = "day")
code <-unique(find_testing_t$code)
date_country<-expand_grid(date, code)
find_testing_t<-left_join(date_country,find_testing_t, by = c("code", "date"))


find_testing_t <- find_testing_t %>%
  # group rows by country code
  group_by(code) %>%
  # create column for 7 day rolling average from raw FIND data
  mutate(
    #new_tests_cap_avg = round(zoo::rollmean(100000*new_tests_orig/pop, 30, fill = NA),2), 
    cases_7d_avg = round(zoo::rollmean(new_cases_orig, 7, fill = NA),2),
    tests_7d_avg = round(zoo::rollmean(new_tests_orig, 7, fill = NA), 2)
  )

# For each country, find the date they last reported raw new tests
find_test_update_date<-find_testing_t%>%
  group_by(code) %>% select(code, date, new_tests_orig)%>%
  filter(!is.na(new_tests_orig) & new_tests_orig != 0)%>%
  filter(date == max(as.Date(date)))%>%
  mutate(date_tests_last_reported = date)%>%
  select(code, date_tests_last_reported) %>%
  mutate(
    rept_tests_within_last_6_months = as.Date(date_tests_last_reported)> (LAST_DATA_PULL_DATE-180),
    days_since_tests_reported = LAST_DATA_PULL_DATE - as.Date(date_tests_last_reported))


# Unit test for report date
stopifnot('last reported date is more recent than data pull date' = sum(as.Date(find_test_update_date$date_tests_last_reported) > 
                                                                          as.Date(LAST_DATA_PULL_DATE), na.rm = TRUE)== 0)


# Add in recent testing capacity metrics ie average test positivity over past 90 days
find_testing_last_year<- find_testing_t %>% filter(date>=(LAST_DATA_PULL_DATE -TIME_WINDOW_YEAR) & 
                                                     date<= LAST_DATA_PULL_DATE)%>%
  group_by(code) %>%
  summarise(tests_in_last_year_raw = sum(new_tests_orig, na.rm = TRUE),
            cases_in_last_year_raw = sum(new_cases_orig, na.rm = TRUE),
            tests_in_last_year_smoothed = sum(new_tests_smoothed, na.rm = TRUE),
            cases_in_last_year_smoothed = sum(new_cases_smoothed, na.rm = TRUE),
            cases_in_last_year_smoothed_truncated = sum(new_cases_smoothed_truncated, na.rm = TRUE),
            avg_tpr_find = mean(pos, na.rm = TRUE),
            tpr_year_raw = cases_in_last_year_raw/tests_in_last_year_raw,
            tpr_year_smoothed = cases_in_last_year_smoothed/tests_in_last_year_smoothed,
            tpr_year_smoothed_truncated = cases_in_last_year_smoothed_truncated/tests_in_last_year_smoothed,
            avg_daily_test_per_1000_last_year_raw = 1000*mean(new_tests_orig/max(pop), na.rm = TRUE),
            avg_daily_tests_per_1000_last_year_smoothed = 1000*mean(new_tests_smoothed/max(pop), na.rm = TRUE))%>%
  filter(!is.na(code))


# Find the countries where they have no tests, missing average TPR
no_avg_tpr<-find_testing_last_year%>%filter(is.na(avg_tpr_find))

# add unit test around this 
no_avg_daily_tests<-find_testing_last_year%>%filter(is.na(avg_daily_tests_per_1000_last_year_smoothed))

# Join last year with data on test updating
find_testing_clean<-left_join(find_test_update_date, find_testing_last_year, by = "code")
n_not_rept_6_mos= nrow(find_testing_clean%>%filter(!is.na(avg_tpr_find) & rept_tests_within_last_6_months == FALSE))

# unit test on test per 100k per day
stopifnot('Countries reporting greater than a test per person per day' = sum(find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed>1000,na.rm = T)== 0)

# unit test for number of countries with test data over 6 months old
stopifnot('More than 25 countries with data havent reported tests in 6 months' = n_not_rept_6_mos<=25)

# unit test for number of countries that will be removed because avg tpr is NA
stopifnot('More than 35 countries are missing average daily TPR from FIND' = nrow(no_avg_tpr)<=35)


# remove any -Inf
find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed<- ifelse(find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed < 0, NA, 
                                                                   find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed)

# remove any Inf
find_testing_clean$tpr_year_smoothed_truncated <- ifelse(find_testing_clean$tpr_year_smoothed_truncated < 0 | find_testing_clean$tpr_year_smoothed_truncated > 1, NA, 
                                               find_testing_clean$tpr_year_smoothed_truncated)


# Join to find_clean
find_clean<-left_join(find_clean, find_testing_clean, by = "code")


#------Dx Testing Capacity Classifier------------------

# create Dx Testing Capacity Classifier variable (WILL EDIT THIS!)
# Variable indicating highest level of diagnostic throughput (Low testing capacity, Medium testing capacity, High testing capacity)
# Variable is based on FIND testing dashboard
# If FIND source missing for that country, take source OWD
find_clean <- find_clean %>%
  # create new variable for Dx testing capacity based on:
  # TPR over the past year >=0.15
  # Unless average daily tests per 1000 >1
  mutate(
    dx_testing_capacity = case_when(
      (is.na(avg_tpr_find)  | rept_tests_within_last_6_months ==FALSE | avg_tpr_find == "NaN") ~ "Insufficient data",
      tpr_year_smoothed_truncated >= 0.15 & avg_daily_tests_per_1000_last_year_smoothed <= 1 ~"Has not demonstrated testing capacity",
      tpr_year_smoothed_truncated >=0.15 & avg_daily_tests_per_1000_last_year_smoothed > 1 ~"Has demonstrated testing capacity",
      tpr_year_smoothed_truncated < 0.15 ~ "Has demonstrated testing capacity",
    )
  )


 

# -------- GISAID data on SARS-CoV-2 sequences by country and time --------------------------------

gisaid_raw <- read.csv(GISAID_DAILY_PATH) %>% # raw refers to all variants, by country, by day
  # standardize names with this janitor function
  clean_names()

# parse collection dates as dates (note this uses the imputed day 15 from metadata processing script)
gisaid_raw$collection_date <- as.Date(as.character(gisaid_raw$gisaid_collect_date), format = "%Y-%m-%d")

gisaid_t <- gisaid_raw%>%
  select(collection_date, gisaid_country, n_new_sequences,
         owid_new_cases, owid_population, country_code, owid_location)%>%
  filter(collection_date>=FIRST_DATE &
           collection_date<= LAST_DATA_PULL_DATE)

# Make sure that the most recent date is yesterday
stopifnot("GISAID metadata run isnt up to date" = max(gisaid_t$collection_date) == LAST_DATA_PULL_DATE)

# Subset to last 7 days of data to get cases (just a good sanity check)
cases_in_last_7_days<-gisaid_t%>%
  filter(collection_date>=(LAST_DATA_PULL_DATE - TIME_WINDOW_WEEK) & 
           collection_date<=LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(
    cases_in_last_7_days = sum(owid_new_cases, na.rm = TRUE),
    cases_per_100k_last_7_days = round(100000*sum(owid_new_cases)/max(owid_population, na.rm = TRUE), 1),
    population_size = max(owid_population, na.rm = TRUE))

# Subset GISAID data to the last  year
gisaid_last_year<-gisaid_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE -TIME_WINDOW_YEAR) & 
                                      collection_date<= LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(cases_in_last_year = sum(owid_new_cases),
            sequences_in_last_year= sum(n_new_sequences),
            pct_cases_sequenced_in_last_year = round(100*(sequences_in_last_year/cases_in_last_year),2),
            sequences_per_100k_last_year = round(100000*sequences_in_last_year/max(owid_population),3))

gisaid_recent_data<-left_join(gisaid_last_year, cases_in_last_7_days, by = "country_code")

# Join both sets of metrics 
find_clean <- left_join(find_clean, gisaid_recent_data, by = c("code" = "country_code"))





#------- Add in old archetype ------------------------------------------------------
old_find<-read.csv(OLD_FIND_MAP_PATH)%>%select(code,country,dx_testing_capacity_clean, sars_cov_2_sequencing, archetype)%>%
  rename(old_test_archetype = dx_testing_capacity_clean,
         old_archetype = archetype,
         old_sequencing_archetype = sars_cov_2_sequencing)%>%filter(!is.na(code))%>%filter(country != "West Bank and Gaza") # 237 coountries
old_codes<-unique(old_find$code) # 237 of them

# filter to only codes in old data
find_clean<-find_clean%>%filter(code %in% old_codes)

find_clean<-left_join(find_clean, old_find, by = "code")
find_clean<-find_clean%>%filter(name != "West Bank and Gaza")%>%
  filter(code != "UMI") # 237 countries
n_codes <- length(unique(find_clean$code))

# Unit tests!
stopifnot('Incorrect number of countries'= n_codes<=237 | n_codes>=236)
# Subset to LMICs
find_clean_LMICs<-find_clean%>%filter(old_test_archetype != 'High Income*')
# unit test for number of LMICs total
stopifnot('Incorrect number of LMICs' = nrow(find_clean_LMICs)==152)

# unit test for number of LMICs with any FIND data
stopifnot('Incorrect number of LMICs with FIND data' = nrow(find_clean_LMICs%>%filter(!is.na(avg_tpr_find)))==109)

stopifnot('Less than 90 LMICs with avg daily TPR not NA & with tests reported in last 6 months' = 
            nrow(find_clean_LMICs%>%filter(!is.na(avg_tpr_find))%>%filter(rept_tests_within_last_6_months == TRUE)) >= 90)




# Output dataset with the countries that haven't reported tests in 6 months
find_not_reported<-find_clean%>%filter(rept_tests_within_last_6_months==FALSE & !is.na(avg_tpr_find))
if (USE_CASE == "local"){
  write.csv(find_not_reported, '../../data/processed/find_delayed_test_reporting.csv')
}
if (USE_CASE == "domino"){
  write.csv(find_not_reported, '/mnt/data/processed/find_delayed_test_reporting.csv')
}


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

find_clean<-find_clean%>%mutate( LMIC_status = case_when(
  world_bank_economies == 'High income' ~'High Income',
  world_bank_economies != 'High income' ~'LMIC'))







# -------- SARS-CoV-2 sequencing capacity classifier original -----------------------
# Variable indicating how many submissions from each country appear in GISAID
find_clean <- find_clean %>%
  # create new variable for SARS-CoV-2 Sequencing based on:
  # % of cases sequenced in the past year > 0.5%
  # cases sequenced per 100k in the past year >= 50
  mutate(
    sars_cov_2_sequencing = case_when(
      is.na(sequences_per_100k_last_year) ~ "Has not demonstrated sequencing capacity",
      pct_cases_sequenced_in_last_year >= 0.5  ~ "Has demonstrated sequencing capacity",
      pct_cases_sequenced_in_last_year < 0.5 & sequences_per_100k_last_year >= 30 ~ "Has demonstrated sequencing capacity",
      pct_cases_sequenced_in_last_year < 0.5 & sequences_per_100k_last_year <30 ~ "Has not demonstrated sequencing capacity"
    )
  )




# -------- Create Archetype classifier variable ----------------------------------------
# Archetype_orig indicates the original archetype names (Strengthen, Connect, Leverage, Test but with the new definitions)
find_clean<-find_clean%>%mutate(
  archetype_orig = case_when(
    old_archetype == "High Income*" ~ "High Income*",
    dx_testing_capacity == "Insufficient data" ~ "Insufficient Data",
    dx_testing_capacity == "Has not demonstrated testing capacity" ~ "Test",
    dx_testing_capacity == "Has demonstrated testing capacity" & sars_cov_2_sequencing == "Has demonstrated sequencing capacity" ~ "Strengthen",
    (dx_testing_capacity == "Has demonstrated testing capacity" & sars_cov_2_sequencing == "Has not demonstrated sequencing capacity" &
    (ngs_capacity == 2 | ngs_capacity == 1)) ~ "Leverage",
    dx_testing_capacity == "Has demonstrated testing capacity" & sars_cov_2_sequencing == "Has not demonstrated sequencing capacity" & ngs_capacity == 0 ~ "Connect"),
  archetype_orig_w_HICs = case_when(
    dx_testing_capacity == "Insufficient data" ~ "Insufficient Data",
    dx_testing_capacity == "Has not demonstrated testing capacity" ~ "Test",
    dx_testing_capacity == "Has demonstrated testing capacity" & sars_cov_2_sequencing == "Has demonstrated sequencing capacity" ~ "Strengthen",
    (dx_testing_capacity == "Has demonstrated testing capacity" & sars_cov_2_sequencing == "Has not demonstrated sequencing capacity" &
       (ngs_capacity == 2 | ngs_capacity == 1)) ~ "Leverage",
    dx_testing_capacity == "Has demonstrated testing capacity" & sars_cov_2_sequencing == "Has not demonstrated sequencing capacity" & ngs_capacity == 0 ~ "Connect"),
  archetype_new = case_when(
    old_archetype == "High Income*" ~ "High Income*",
    dx_testing_capacity == "Insufficient data" ~ "Insufficient Data",
    dx_testing_capacity == "Has not demonstrated testing capacity" ~ "Test",
    dx_testing_capacity == "Has demonstrated testing capacity" & sars_cov_2_sequencing == "Has demonstrated sequencing capacity" ~ "Sustain",
    dx_testing_capacity == "Has demonstrated testing capacity" & sars_cov_2_sequencing == "Has not demonstrated sequencing capacity" ~ "Sequence")
)

# Edit the date variable so that it is in a universal format
test<-find_clean%>%separate(date_tests_last_reported,
                            into = c("year", "month", "day"),
                            sep = "-")
test$month<-month.name[as.numeric(test$month)]
test$date_tests_last_reported<-paste0(test$month, ' ', test$day, ', ', test$year)
test$date_tests_last_reported[test$date_tests_last_reported == "NA NA, NA"]<-"No tests reported"
find_clean$date_tests_last_reported<-test$date_tests_last_reported



find_clean_LMICs<-find_clean%>%filter(!old_archetype == "High Income*")
n_insufficient_data<- sum(find_clean_LMICs$archetype_orig == "Insufficient Data")
n_not_tests<-sum(find_clean_LMICs$dx_testing_capacity == "Reliable testing capacity")
n_Test<- sum(find_clean_LMICs$archetype_orig == "Test")
n_Strengthen <-sum(find_clean_LMICs$archetype_orig == "Strengthen")
n_Leverage <- sum(find_clean_LMICs$archetype_orig == "Leverage")
n_Connect <- sum(find_clean_LMICs$archetype_orig == "Connect")
n_Sequence<- sum(find_clean_LMICs$archetype_new == "Sequence")

if (USE_CASE == 'local') {
find_clean_LMICs%>%filter(archetype_orig == "Insufficient Data")%>%write.csv('../../data/processed/countries_in_insufficient_data.csv')
find_clean_LMICs%>%filter(archetype_orig == "Test")%>%write.csv('../../data/processed/countries_in_test.csv')
find_clean_LMICs%>%filter(archetype_orig == "Strengthen")%>%write.csv('../../data/processed/countries_in_strengthen.csv')
find_clean_LMICs%>%filter(archetype_orig == "Leverage" | archetype_orig == "Connect")%>%
  write.csv('../../data/processed/countries_in_Lev_or_Connect.csv')
}

if (USE_CASE == 'domino') {
  find_clean_LMICs%>%filter(archetype_orig == "Insufficient Data")%>%write.csv('/mnt/data/processed/countries_in_insufficient_data.csv')
  find_clean_LMICs%>%filter(archetype_orig == "Test")%>%write.csv('/mnt/data/processed/countries_in_test.csv')
  find_clean_LMICs%>%filter(archetype_orig == "Strengthen")%>%write.csv('/mnt/data/processed/countries_in_strengthen.csv')
  find_clean_LMICs%>%filter(archetype_orig == "Leverage" | archetype_orig == "Connect")%>%
    write.csv('/mnt/data/processed/countries_in_Lev_or_Connect.csv')
}


n_given_archetypes = n_Test + n_Strengthen + n_Sequence

stopifnot("Number given archetypes other than insufficient data is less than 90 (should be around 95)"= n_given_archetypes>=90)

find_map<- find_clean %>%select(name, code, population_size, sequencing_capacity, tpr_year_smoothed_truncated, avg_daily_tests_per_1000_last_year_smoothed,
                                dx_testing_capacity, date_tests_last_reported, days_since_tests_reported, pct_cases_sequenced_in_last_year,
                                sequences_per_100k_last_year, sars_cov_2_sequencing, ngs_capacity, facility_access, cases_per_100k_last_7_days, 
                                old_archetype, archetype_orig,
                                archetype_orig_w_HICs, archetype_new, world_bank_economies)


# Make pretty with rounded numbers, and add in the potential new names
find_map<-find_map%>% mutate(
  archetype_full_orig = case_when(
    archetype_orig == "Insufficient Data" ~ "Insufficient Data - Missing key diagnostic or case metrics",
    archetype_orig == "High Income*" ~ "High Income*",
    archetype_orig == "Test" ~ "Test - Increase diagnostic testing capacity",
    archetype_orig == "Strengthen" ~ "Strengthen - Build additional NGS capacity for scale-up",
    archetype_orig == "Leverage" ~ "Leverage - Leverage existing NGS capacity",
    archetype_orig == "Connect" ~ "Connect - Connect to countries with NGS capacity or build NGS capacity from scratch"),
  archetype_full_new = case_when(
    archetype_new == "Insufficient Data" ~ "Insufficient Data- Missing key diagnostic or case metrics",
    archetype_new == "High Income*" ~ "High Income*",
    archetype_new == "Test" ~ "Test - Increase diagnostic testing capacity",
    archetype_new == "Sustain" ~ "Sustain - Sustain diagnostic & sequencing levels",
    archetype_new == "Sequence" ~ "Sequence - Improve sequencing levels"),
  archetype_full_orig_w_HICs = case_when(
    archetype_orig_w_HICs == "Insufficient Data" ~ "Insufficient Data - Missing key diagnostic or case metrics",
    archetype_orig_w_HICs == "Test" ~ "Test - Increase diagnostic testing capacity",
    archetype_orig_w_HICs == "Strengthen" ~ "Strengthen - Build additional NGS capacity for scale-up",
    archetype_orig_w_HICs == "Leverage" ~ "Leverage - Leverage existing NGS capacity",
    archetype_orig_w_HICs == "Connect" ~ "Connect - Connect to countries with NGS capacity or build NGS capacity from scratch"),
  TPR_pct = paste0(round(100*tpr_year_smoothed_truncated, 1), ' %'),
  daily_tests_per_1000 = paste0(round(avg_daily_tests_per_1000_last_year_smoothed,2), ' per 1000 persons'),
  pct_seq = paste0(round(pct_cases_sequenced_in_last_year,2), ' %'),
  seq_per_100k = paste0(round(sequences_per_100k_last_year,1), ' per 100k persons'),
  cases_per_100k = paste0(round(cases_per_100k_last_7_days, 1), ' per 100k persons')
  )

# Replace NAs with language
find_map$TPR_pct[find_map$TPR_pct == "NA %"]<- 'Insufficient Data'
find_map$daily_tests_per_1000[find_map$daily_tests_per_1000 == "NA per 1000 persons"]<- 'Insufficient Data'
find_map$pct_seq[find_map$pct_seq == "NA %" | find_map$pct_seq == "NaN %"]<-'Insufficient Data'
find_map$seq_per_100k[find_map$seq_per_100k == "NA per 100k persons"]<- 'Insufficient Data'
find_map$cases_per_100k[find_map$cases_per_100k == "NA per 100k persons"]<- 'Insufficient Data'

# Make column headers look nice
find_map<-find_map%>%rename(
  Archetype = archetype_full_orig,
  `Archetype*` = archetype_full_orig_w_HICs,
  `Test positivity rate (%) in past year` = TPR_pct,
  `Average daily tests in past year` = daily_tests_per_1000,
  `Date tests last reported` = date_tests_last_reported,
  `Days since tests were reported` = days_since_tests_reported,
  `% of cases sequenced in past year` = pct_seq,
  `Number of sequences in past year` = seq_per_100k,
  `Cases in the last 7 days` = cases_per_100k
)

stopifnot('More than 3 countries missing archetype at final step' = sum(find_map$Archetype == "NaN" |is.na(find_map$Archetype)) <=3)

# Generate datasets for the countries that have sufficient sequencing but insufficient testing 
find_insuff_test_but_have_seq<-find_map%>%filter(old_archetype!= "High Income*")%>%
  filter(dx_testing_capacity == "Has not demonstrated testing capacity")%>%
  filter(sars_cov_2_sequencing == "Has demonstrated sequencing capacity")

find_map<-find_map%>%mutate(
  seq_but_no_test_flag = ifelse(
    (dx_testing_capacity == "Has not demonstrated testing capacity" & 
       sars_cov_2_sequencing == "Has demonstrated sequencing capacity"), 'yes', 'no'
  )
)


# Join shapefiles! 
shapefile <- read_delim(SHAPEFILES_FOR_FLOURISH_PATH, delim = "\t") %>%
  rename(code = `3-letter ISO code`,
         name = Name) %>%
  select(geometry, code)
# Need to deduplicate?? 

duplicates<-shapefile[duplicated(shapefile$code),] 


find_clean_flourish<-left_join(shapefile,find_map, by = "code")
find_clean_flourish<-find_clean_flourish%>%filter(!is.na("code"))

find_insufficient_test_but_have_seq<-left_join(shapefile, find_insuff_test_but_have_seq, by = "code")

# Remove the extraneous columns from the full_dataset
full_dataset<-find_clean%>%select(name, code, population_size, date_tests_last_reported,
                                  rept_tests_within_last_6_months, days_since_tests_reported,
                                  cases_in_last_year_smoothed_truncated, tests_in_last_year_smoothed,
                                  tpr_year_smoothed_truncated, avg_daily_tests_per_1000_last_year_smoothed,
                                  cases_in_last_year, sequences_in_last_year,
                                  pct_cases_sequenced_in_last_year, sequences_per_100k_last_year,
                                  dx_testing_capacity,ngs_capacity, sequencing_capacity,
                                  sars_cov_2_sequencing, world_bank_economies, archetype_orig_w_HICs)%>%
  rename(archetype = archetype_orig_w_HICs)


# Remove extraneous columns from the map dataset 
find_clean_flourish<-find_clean_flourish%>%select(-old_archetype, -archetype_orig, -`Archetype`, 
                                                  -archetype_full_new, -archetype_new)

# Make clean dataset
clean_dataset<-find_map%>%select(name, `Date tests last reported`, `Test positivity rate (%) in past year`,
                                 `Average daily tests in past year`, `% of cases sequenced in past year`,
                                 `Number of sequences in past year`, world_bank_economies,dx_testing_capacity, 
                                 sars_cov_2_sequencing,facility_access, archetype_orig_w_HICs)%>%
  rename(
    `World Bank Economic Status` = world_bank_economies,
    `COVID-19 Diagnostic Testing Capacity` = dx_testing_capacity,
    `SARS-CoV-2 Sequencing Capacity` = sars_cov_2_sequencing,
    `NGS Facility Access` = facility_access,
    `Archetype` = archetype_orig_w_HICs)




if (USE_CASE == 'local'){
  write.csv(full_dataset, "../../data/NGS_region_data/February_2022/full_dataset.csv", na = "NaN", row.names = FALSE)
  write.csv(find_clean_flourish, "../../data/NGS_region_data/February_2022/find_map.csv", na = "NaN", row.names = FALSE)
  write.csv(clean_dataset, "../../data/NGS_Region_data/February_2022/clean_dataset.csv", na = "NaN", row.names = FALSE)
  write.csv(find_insuff_test_but_have_seq, "../../data/NGS_region_data/February_2022/test_but_suff_seq.csv", na = "NaN", row.names = FALSE )
}

if (USE_CASE == 'domino'){
  write.csv(full_dataset, "/mnt/data/processed/full_dataset.csv", na = "NaN", row.names = FALSE)
  write.csv(find_clean_flourish, "/mnt/data/processed/find_map.csv", na = "NaN", row.names = FALSE)
  write.csv(clean_dataset, "/mnt/data/processed/clean_dataset.csv", na = "NaN", row.names = FALSE)
  write.csv(find_insuff_test_but_have_seq, "/mnt/data/processed/test_but_suff_seq.csv", na = "NaN", row.names = FALSE )
}


# Stats for Rick
n_countries_no_sequencing<-sum((find_map$sequences_per_100k_last_year == 0) | is.na(find_map$sequences_per_100k_last_year))
find_map$pct_cases_sequenced_in_last_year[is.na(find_map$pct_cases_sequenced_in_last_year)]<-0
find_map_rm<-find_map%>%filter(!is.na(population_size))%>%filter(!population_size == -Inf)%>%filter(!population_size == Inf)%>%
  filter(!pct_cases_sequenced_in_last_year == Inf)
summary_df<-find_map_rm%>%summarise(
  mean_pct_cases = mean(pct_cases_sequenced_in_last_year*population_size)/sum(population_size))

summary<-find_clean%>%filter(!population_size == Inf)%>%filter(!population_size == -Inf)%>%group_by(LMIC_status)%>%
  summarise(
    mean_pct_cases_sequenced_in_last_year = sum(sequences_in_last_year,na.rm = T)/sum(cases_in_last_year, na.rm = T),
    population = sum(population_size, na.rm = T),
    sequences = sum(sequences_in_last_year, na.rm = T))%>%mutate(
      pct_pop = 100*population/sum(population),
      pct_seq = 100*sequences/sum(sequences)
    )



