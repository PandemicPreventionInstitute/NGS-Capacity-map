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
 
# ------ Name data paths and set parameters -------------------------------------------
rm(list = ls())
## Set local file path names
WHO_REGIONS_PATH<-'../data/who_regions.csv'
FIND_TESTING_SEQ_RAW_PATH<- '../data/2021_04_04_FIND_capacity_mapping_data_sources.xlsx'
OWID_TESTING_POLICY_PATH<- '../data/covid-19-testing-policy.csv'
ALL_DATA_PATH<- url("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv")
  #'../data/data_all.csv'
#GISAID_RAW_METADATA_PATH<-'../data/GISAID_raw_metadata_10_18_2021.tsv'
# PROVISION_PATH<-'../data/provision.csv'
GISAID_DAILY_PATH<-'../data/processed/gisaid_cleaning_output.csv' # this is the file that comes from Briana's processing file
ECONOMY_PATH<-'../data/CLASS.xls'

LAST_DATA_PULL_DATE<-as.Date("2021-12-13") # enter here "YYYY-10-18"
TIME_WINDOW <- 89
TIME_WINDOW_WEEK<- 6 
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
  select(name, time, unit, pop_100k, cap_new_tests, cap_cum_tests, all_new_tests, all_cum_tests, all_cum_cases) %>%
  # rename columns as date, code, pop_100k, new_tests_cap, new_tests_all
  rename(country = name, date = time, code= unit, pop_100k = pop_100k, new_tests_cap = cap_new_tests, new_tests_all = all_new_tests, 
          cap_cum_tests = cap_cum_tests, all_cum_tests = all_cum_tests, all_cum_cases = all_cum_cases) %>%
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
# set start date
first_date<-min(find_testing_t$date, na.rm = TRUE)
date <- seq.Date(first_date, LAST_DATA_PULL_DATE, by = "day")
code <-unique(find_testing_t$code)
date_country<-expand_grid(date, code)
find_testing_t<-left_join(date_country,find_testing_t, by = c("code", "date"))



# Fill in the NAs on the values 
find_testing_t$new_tests_cap[is.na(find_testing_t$new_tests_cap)]<-0
find_testing_t$new_tests_all[is.na(find_testing_t$new_tests_all)]<-0

# Test with a single country
#find_testing_t_USA<-find_testing_t%>%filter(code == "USA")
#find_testing_t_USA<-find_testing_t_USA%>%mutate(
#  new_tests_per_100k = new_tests_all/pop_100k 
#)


# create variable for 30 day rolling average of new tests per capita
find_testing_t <- find_testing_t %>%
  # group rows by country code
  group_by(code) %>%
  # create column for 30 day rolling average
  mutate(
    new_tests_cap_avg = round(zoo::rollmean(new_tests_all/pop_100k, 30, fill = NA),2)
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
write.csv(no_tests, '../data/countries_w_no_FIND_tests.csv')


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
            avg_test_per_1000_last_year = mean(new_tests_cap))

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
 

# -------- GISAID data on SARS-CoV-2 sequences by country and time --------------------------------

gisaid_raw <- read.csv(GISAID_DAILY_PATH) %>% # raw refers to all variants, by country, by day
  # standardize names with this janitor function
  clean_names()

# generate country codes from GISAID country names
gisaid_raw$country_code <- countrycode(gisaid_raw$gisaid_country, origin = 'country.name', destination = 'iso3c')

# Remove any rows that don't contain country codes (i.e. not a valid country)
gisaid_raw<-gisaid_raw[!is.na(gisaid_raw$country_code),]

# generate country codes from OWID country names
gisaid_raw$country_code_owid <- countrycode(gisaid_raw$owid_location, origin = 'country.name', destination = 'iso3c')

# inserts missing country codes
gisaid_raw$country_code[gisaid_raw$country == "Micronesia (country)"] <- "FSM"
gisaid_raw$country_code[gisaid_raw$country == "Timor"] <- "TLS"
gisaid_raw$country_code[gisaid_raw$country == "Turks and Caicos Islands"] <- "TCA"
gisaid_raw$country_code[gisaid_raw$country == "Nauru"] <- "NRU"
gisaid_raw$country_code[gisaid_raw$country == "Kosovo"] <- "XKX"
gisaid_raw$country_code[gisaid_raw$country == "Guernsey"] <- "GGY"
gisaid_raw$country_code[gisaid_raw$country == "Falkland Islands"] <- "FLK"

# parse collection dates as dates
# any observations with only year or year-month become NA
gisaid_raw$collection_date <- as.Date(as.character(gisaid_raw$gisaid_collect_date), format = "%Y-%m-%d")

# parse submission dates as dates
print("FLAG: THIS IS WHAT WILL BE DELETED WHEN WE GET SUBMISSION DATE")
gisaid_raw$submission_date <- as.Date(as.character(gisaid_raw$owid_date), format = "%Y-%m-%d")

gisaid_t <- gisaid_raw%>%select(collection_date,submission_date, gisaid_country, all_lineages,
                                         owid_new_cases, owid_population, country_code, owid_location)%>%
  rename(n_new_sequences = all_lineages)



# CHECK THAT DATA SET HAS COMPLETED DATE TIME SERIES
collection_date <- seq.Date(first_date, LAST_DATA_PULL_DATE, by = "day")
country_code <-unique(gisaid_t$country_code)
date_country<-expand_grid(collection_date, country_code)
gisaid_t<-left_join(date_country,gisaid_t, by = c("country_code", "collection_date"))

# Fill in the NAs on the values 
gisaid_t$n_new_sequences[is.na(gisaid_t$n_new_sequences)]<-0
gisaid_t$owid_new_cases[is.na(gisaid_t$owid_new_cases)]<-0


  
# find 90 day average of new sequences
gisaid_t <- gisaid_t %>%
  # group rows by country code
  group_by(country_code) %>%
  # create column for 90 day rolling average
  mutate(
    seq_avg = TIME_WINDOW*round(zoo::rollmean(n_new_sequences, TIME_WINDOW, fill = NA),2)
  )

# find the maximum of the 90 day rolling average
gisaid_max_seq<-gisaid_t%>%
  group_by(country_code)%>%
  summarise(max_new_seq_cap_avg = 100000*max(seq_avg, na.rm = TRUE)/max(owid_population),
            total_sequences = sum(n_new_sequences))


# Join the 90 day and overall 
gisaid_recent_data<-left_join(gisaid_recent_data, gisaid_max_seq)


gisaid_recent_data<-gisaid_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE -TIME_WINDOW) & 
                                        collection_date<= LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(cases_in_last_90_days = sum(owid_new_cases),
            sequences_in_last_90_days = sum(n_new_sequences),
            population_size = max(owid_population, na.rm = TRUE))


# Make new variables 
gisaid_recent_data<-gisaid_recent_data%>%
  mutate(percent_of_recent_cases_sequenced= 100*sequences_in_last_90_days/cases_in_last_90_days,
         per_capita_seq_rate = 100000*sequences_in_last_90_days/population_size,
         max_prevalence_variant_pct = 100*(1-((1-CONF_LEVEL)^(1/sequences_in_last_90_days))))

# Join the 90 day and overall 
gisaid_recent_data<-left_join(gisaid_recent_data, gisaid_max_seq)

# Subset to last 7 days of data
cases_in_last_7_days<-gisaid_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE - TIME_WINDOW_WEEK) & 
                                           collection_date<=LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(cases_in_last_7_days = sum(owid_new_cases))


# replace percent_cases_sequenced > 100 or NaN with NA
gisaid_recent_data$percent_of_recent_cases_sequenced[gisaid_recent_data$percent_of_recent_cases_sequenced > 100 | gisaid_recent_data$percent_of_recent_cases_sequenced == "NaN"] <- NA


gisaid_clean<-left_join(gisaid_recent_data, cases_in_last_7_days, by = "country_code")

# Subset to last year and join
gisaid_last_year<-gisaid_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE -TIME_WINDOW_YEAR) & 
                                      collection_date<= LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(cases_in_last_year = sum(owid_new_cases),
            sequences_in_last_year= sum(n_new_sequences))

gisaid_clean<-left_join(gisaid_clean, gisaid_last_year)


# Join both sets of metrics 
find_clean <- left_join(find_clean, gisaid_clean, by = c("code" = "country_code"))

find_clean<-find_clean%>%mutate(
  tpr_90_days = cases_in_last_90_days/tests_in_last_90_days,
  tpr_7_days = cases_in_last_7_days/tests_in_last_7_days,
  cases_in_the_last_7_days_per_100k = 100000*cases_in_last_7_days/population_size,
  test_in_the_last_7_days_per_100k = 100000*tests_in_last_7_days/population_size,
  tpr_year = cases_in_last_year/tests_in_last_year
)

find_clean$tpr_90_days[find_clean$tpr_90_days == Inf] <- NA
find_clean$tpr_year[find_clean$tpr_year == Inf] <- NA




#----- Find difference in reporting lag from 2020 to 2021---------------------------

print("FLAG: THIS WILL BE 0 UNTIL WE GET UPDATED DATASET FROM ZACH")

month_1_start <- as.Date("2020-01-01", format = "%Y-%m-%d")
month_1_end <- as.Date("2020-12-31", format = "%Y-%m-%d")
month_2_start <- as.Date("2021-01-01", format = "%Y-%m-%d")
month_2_end <- as.Date("2021-12-31", format = "%Y-%m-%d")

# dataframe for first month of interest reporting lag
global_month_1_lag <- gisaid_t %>%
  filter(collection_date >= as.Date(month_1_start) &
           collection_date <= as.Date(month_1_end)) %>%
  group_by(country_code) %>%
  summarise(median_reporting_lag_month_1 = median(as.numeric(submission_date - collection_date,
                                                          unit = "days"), na.rm = T)
  ) %>%
  select(country_code, median_reporting_lag_month_1)


# create dataframe for second month of interest reporting lag
global_month_2_lag <- gisaid_t %>%
  filter(collection_date >= as.Date(month_2_start) &
           collection_date <= as.Date(month_2_end)) %>%
  group_by(country_code) %>%
  summarise(median_reporting_lag_month_2 = median(as.numeric(submission_date - collection_date,
                                                          unit = "days"), na.rm = T)
  ) %>%
  select(country_code, median_reporting_lag_month_2)


# join first and second month of interest data frames
global_month_lag <- full_join(global_month_2_lag, global_month_1_lag, by = "country_code")

#remove NA
global_month_lag<-na.omit(global_month_lag)

# create reporting lag month to month difference column
global_month_lag$reporting_lag_diff <- as.numeric(global_month_lag$median_reporting_lag_month_2 - global_month_lag$median_reporting_lag_month_1)

# remove unnecessary columns and rows
global_month_lag <- global_month_lag %>%
  select(country_code, reporting_lag_diff)

# find_clean: merge GISAID metadata into template
find_clean <- left_join(find_clean, global_month_lag, by = c("code" = "country_code"))

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
write.csv(find_clean, "../data/find_map.csv", na = "NaN", row.names = FALSE)



# export find_clean Global to csv
write.csv(find_clean %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                  max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                  max_prevalence_variant_pct,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'Prevalence at which we would detect at least 1 variant' = max_prevalence_variant_pct,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_global.csv", na = "NaN", row.names = FALSE)
 
# export find_clean Africa to csv
write.csv(find_clean %>%
            filter(region == "Africa") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   max_prevalence_variant_pct,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'Prevalence at which we would detect at least 1 variant' = max_prevalence_variant_pct,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_africa.csv", na = "NaN", row.names = FALSE)
 
# export find_clean Americas to csv
write.csv(find_clean %>%
            filter(region == "Americas") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                  max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                  max_prevalence_variant_pct,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'Prevalence at which we would detect at least 1 variant' = max_prevalence_variant_pct,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_americas.csv", na = "NaN", row.names = FALSE)
 
# export find_clean Eastern Mediterranean to csv
write.csv(find_clean %>%
            filter(region == "Eastern Mediterranean") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   max_prevalence_variant_pct,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'Prevalence at which we would detect at least 1 variant' = max_prevalence_variant_pct,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_eastern_mediterranean.csv", na = "NaN", row.names = FALSE)
 
# export find_clean Europe to csv
write.csv(find_clean %>%
            filter(region == "Europe") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   max_prevalence_variant_pct,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'Prevalence at which we would detect at least 1 variant' = max_prevalence_variant_pct,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_europe.csv", na = "NaN", row.names = FALSE)
 
# export find_clean South-East Asia to csv
write.csv(find_clean %>%
            filter(region == "South-East Asia") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   max_prevalence_variant_pct,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'Prevalence at which we would detect at least 1 variant' = max_prevalence_variant_pct,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_south_east_asia.csv", na = "NaN", row.names = FALSE)
 
# export find_clean Western Pacific to csv
write.csv(find_clean %>%
            filter(region == "Western Pacific") %>%
            select(country, region, who_testing_capacity, max_new_tests_cap_avg, sequencing_capacity, total_sequences, recent_sequences,
                   cases_newly_reported_in_last_7_days_per_100000_population,
                   max_new_seq_cap_avg, percent_of_recent_cases_sequenced, per_capita_seq_rate,
                   max_prevalence_variant_pct,
                   reporting_lag_diff, archetype_full) %>%
            rename(., "Country" = country, "Region" = region, 'WHO testing Capacity' = who_testing_capacity, 
                   'Max 30-day average of new tests per capita' = max_new_tests_cap_avg, 'Sequencing capacity' = sequencing_capacity, 
                   'Cumulative GISAID sequences' = total_sequences, 
                   'Number of sequences in the last 90 days'= recent_sequences,
                   'Max 90-day average of new sequences per capita' = max_new_seq_cap_avg,
                   'Percent of cases in the last 90 days sequences' = percent_of_recent_cases_sequenced,
                   'Number of recent sequences per capita' = per_capita_seq_rate,
                   'Prevalence at which we would detect at least 1 variant' = max_prevalence_variant_pct,
                   'New cases per capita in the last 7 days per 100k' = cases_newly_reported_in_last_7_days_per_100000_population, 
                   "Archetype" = archetype_full),
          "../data/find_table_western_pacific.csv", na = "NaN", row.names = FALSE)