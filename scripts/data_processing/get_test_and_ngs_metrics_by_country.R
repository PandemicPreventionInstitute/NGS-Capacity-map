# Original Author: Nathan Poland
# Updated: Briana Thrift & Zach Susswein
# Current Author: Kaitlyn Johnson

# Date updated: 11-29-2021

# This script takes in the GISAID metadata and OWID and find data and finds the recent cases, tests, and sequences
# It will be used to put the Omicron sequencing data in context
rm(list = ls())
USE_CASE = 'domino' # options: 'local' or 'domino'



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
#GISAID_DAILY_PATH<-'/mnt/data/processed/gisaid_cleaning_output.csv' # this is the file that comes from Briana's processing file
GISAID_DAILY_PATH<-'/mnt/data/processed/gisaid_owid_merged.csv' # output from gisaid_metadata_processing.R
OMICRON_DAILY_CASES<-'/mnt/data/raw/omicron_gisaid_feed.csv'
#BNO_CASES_BY_COUNTRY_PATH<-paste0('/mnt/data/raw/daily_BNO_file/', today,'.csv')
BNO_CASES_BY_COUNTRY_DATE<-'/mnt/data/raw/BNO_scraped_master.csv'
SEQUENCES_LAST_30_DAYS<-'/mnt/data/processed/sequences_last_30_days.csv'
SHAPEFILES_FOR_FLOURISH_PATH <- '/mnt/data/static/geometric_country_code_name_master_file.txt'
LAT_LONG_FOR_FLOURISH_PATH<-'/mnt/data/static/country_lat_long_names.csv'
}

if (USE_CASE == 'local'){
ALL_DATA_PATH<- url("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv")
#GISAID_DAILY_PATH<-'../data/processed/gisaid_cleaning_output.csv' # this is the file that comes from Briana's processing file
GISAID_DAILY_PATH<-'../data/processed/gisaid_owid_merged.csv' # output from gisaid_metadata_processing.R
OMICRON_DAILY_CASES<-'../data/raw/omicron_gisaid_feed.csv'
#BNO_CASES_BY_COUNTRY_PATH<-paste0('../data/raw/daily_BNO_file/', today,'.csv')
BNO_CASES_BY_COUNTRY_DATE<-'../data/raw/BNO_scraped_master.csv'
SEQUENCES_LAST_30_DAYS<-'../data/processed/sequences_last_30_days.csv'
SHAPEFILES_FOR_FLOURISH_PATH <- '../data/static/geometric_country_code_name_master_file.txt'
LAT_LONG_FOR_FLOURISH_PATH<-'../data/static/country_lat_long_names.csv'
}

LAST_DATA_PULL_DATE<-as.Date(substr(lubridate::now('EST'), 1, 10))-days(1) # Make this based off of yesterday!
FIRST_DATE<-"2019-12-01"
TIME_WINDOW <- 29 # since we will include the reference data
TIME_WINDOW_WEEK<- 6 # since will include the reference date
TIME_SERIES_WINDOW<- 89 # last 90 days?
CONF_LEVEL<-0.95 # confidence we want to be that a variant is not greater than our estimated prevalence
# that has been detected at least once (i.e. what variant prevalence would we be able to detect?)

# adjusted script so that _clean refers to data sets that are in the format country and the metric (i.e. do not contain time series)
# intermediates with time series are designated with _t




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
    cum_tpr = max(all_cum_cases, na.rm = T)/max(all_cum_tests, na.rm = T)
  )


# Add in recent testing capacity metrics ie average test positivity over past 90 days
# Subset to last 7 days of data

tests_recent<-find_testing_t%>%filter(date>=(LAST_DATA_PULL_DATE - TIME_WINDOW_WEEK) & 
                                          date<=LAST_DATA_PULL_DATE)%>%
  group_by(code)%>%
  summarise(tests_in_last_7_days = sum(new_tests_all))

tests_30_days<-find_testing_t%>%filter(date>=(LAST_DATA_PULL_DATE - TIME_WINDOW) & 
                                        date<=LAST_DATA_PULL_DATE)%>%
  group_by(code)%>%
  summarise(tests_in_last_30_days = sum(new_tests_all))

tests_recent<-left_join(tests_recent, tests_30_days, by = "code")

find_testing_clean<-left_join(find_testing_clean, tests_recent, by = "code")

 
# remove any -Inf
find_testing_clean$max_new_tests_cap_avg <- ifelse(find_testing_clean$max_new_tests_cap_avg < 0, NA, find_testing_clean$max_new_tests_cap_avg)
 
# remove any Inf
find_testing_clean$cum_tpr <- ifelse(find_testing_clean$cum_tpr < 0 | find_testing_clean$cum_tpr > 1, NA, find_testing_clean$cum_tpr)
 

 

# -------- GISAID data on SARS-CoV-2 sequences by country and time --------------------------------

gisaid_raw <- read.csv(GISAID_DAILY_PATH) %>% # raw refers to all variants, by country, by day
  # standardize names with this janitor function
  clean_names()

# parse collection dates as dates (note this uses the imputed day 15 from metadata processing script)
gisaid_raw$collection_date <- as.Date(as.character(gisaid_raw$gisaid_collect_date), format = "%Y-%m-%d")

gisaid_t <- gisaid_raw%>%select(collection_date, gisaid_country, n_new_sequences,
                                         owid_new_cases, owid_population, country_code, owid_location)

# # CHECK THAT DATA SET HAS COMPLETED DATE TIME SERIES (commented out because we do this in gisaid_metatdata_processing.R)
# collection_date <- seq.Date(first_date, LAST_DATA_PULL_DATE, by = "day")
# country_code <-unique(gisaid_t$country_code)
# date_country<-expand_grid(collection_date, country_code)
# gisaid_t<-left_join(date_country,gisaid_t, by = c("country_code", "collection_date"))
# 
# # Fill in the NAs on the values 
# gisaid_t$n_new_sequences[is.na(gisaid_t$n_new_sequences)]<-0
# gisaid_t$owid_new_cases[is.na(gisaid_t$owid_new_cases)]<-0

  
# find 7 day average of new sequences
gisaid_t <- gisaid_t %>%
  # group rows by country code
  group_by(country_code) %>%
  # create column for 7 day rolling average
  mutate(
    seq_7davg = round(zoo::rollmean(n_new_sequences, 7, fill = NA),2),
    #gisaid_md_seq_omicron_7davg = round(zoo::rollmean(b_1_1_529, 7, fill = NA), 2),# remove because we don't have omicron feed anymore
    #pct_omicron_7davg = gisaid_md_seq_omicron_7davg/seq_7davg, 
    rolling_cases_last_30_days = rollapplyr(owid_new_cases,30,sum, partial = TRUE, align = "right"),
    rolling_cases_last_7_days = rollapplyr(owid_new_cases, 7, sum, partial = TRUE, align = "right"),
    rolling_seq_last_30_days = rollapplyr(n_new_sequences, 30, sum, partial = TRUE, align = "right"),
    percent_of_cases_seq_last_30_days = 100*rolling_seq_last_30_days/rolling_cases_last_30_days
  )


# filter to last 60 days 
gisaid_t <- gisaid_t %>%filter(collection_date>=(LAST_DATA_PULL_DATE -TIME_SERIES_WINDOW) & 
                                 collection_date<= LAST_DATA_PULL_DATE)
# Make sure that the most recent date is yesterday
stopifnot("GISAID metadata run isnt up to date" = max(gisaid_t$collection_date) == (today_date - days(1)))
#write.csv(gisaid_t, "../data/gisaid_t.csv")




# Subset to only recent data to get recent sequences and cases by country
gisaid_recent_data<-gisaid_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE -TIME_WINDOW) & 
                                        collection_date<= LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(cases_in_last_30_days = sum(owid_new_cases, na.rm = TRUE),
            sequences_in_last_30_days = sum(n_new_sequences, na.rm = TRUE),
            population_size = max(owid_population, na.rm = TRUE),
            cases_per_100k_last_30_days = 100000*sum(owid_new_cases, na.rm = TRUE)/max(owid_population, na.rm = TRUE))

# replaces NAs with 0
gisaid_recent_data$sequences_in_last_30_days[is.na(gisaid_recent_data$sequences_in_last_30_days)]<-0
gisaid_recent_data<-gisaid_recent_data%>%
  mutate(percent_of_cases_sequenced_last_30_days = 100*sequences_in_last_30_days/cases_in_last_30_days,
         per_capita_seq_rate_in_last_30_days = 100000*sequences_in_last_30_days/population_size,
         max_prevalence_variant_pct = 100*(1-((1-CONF_LEVEL)^(1/sequences_in_last_30_days))))



# Subset to last 7 days of data
cases_in_last_7_days<-gisaid_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE - TIME_WINDOW_WEEK) & 
                                          collection_date<=LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(cases_per_100k_last_7_days = round(100000*sum(owid_new_cases)/max(owid_population, na.rm = TRUE), 1))

# join with 30 day summary 
gisaid_recent_data<-left_join(gisaid_recent_data, cases_in_last_7_days, by = "country_code")

gisaid_recent_data<-left_join(gisaid_recent_data, find_testing_clean, by = c("country_code"="code"))

gisaid_recent_data<-gisaid_recent_data%>%
  mutate(tests_per_100k_in_last_7_days= 100000*tests_in_last_7_days/population_size,
         tests_per_100k_in_last_30_days = 100000*tests_in_last_30_days/population_size,
         positivity_in_last_7_days = cases_per_100k_last_7_days/tests_per_100k_in_last_7_days,
         positivity_in_last_30_days = cases_per_100k_last_30_days/tests_per_100k_in_last_30_days)







# -------- Merge GISAID Omicron & BNO Omicron by country  --------------------------------

BNO_omicron_t<-read_csv(BNO_CASES_BY_COUNTRY_DATE)
BNO_omicron_t<-unique(BNO_omicron_t)
BNO_omicron_t<-BNO_omicron_t%>%drop_na(confirmed)
BNO_omicron_t<-BNO_omicron_t%>%rename(BNO_confirmed = confirmed, BNO_probable = probable)%>%
  select(code, BNO_confirmed, BNO_probable, timestamp)
BNO_omicron_t$date<-as.Date(substr(BNO_omicron_t$timestamp, 1, 10))
# Records time of scrape 
BNO_omicron_t$hour_of_day<-as.numeric(substr(BNO_omicron_t$timestamp, 12,13)) 
BNO_omicron_t$min_of_day<-as.numeric(substr(BNO_omicron_t$timestamp, 15,16))/60 
BNO_omicron_t$time_of_day<- BNO_omicron_t$hour_of_day + BNO_omicron_t$min_of_day

# Grab today's most recent data only (and throw error if not updated)
BNO_omicron<-BNO_omicron_t%>%filter(timestamp == (max(timestamp)))
today_date<-lubridate::today('EST')
stopifnot("Scraper didnt update properly today" = max(BNO_omicron$date) == today_date)

BNO_omicron<-BNO_omicron%>%
  select(code, BNO_confirmed, BNO_probable)

# GISAID OMICRON data read in
omicron_t<-read.csv(OMICRON_DAILY_CASES)
omicron_seq<-omicron_t%>%group_by(code)%>%
  summarise(cum_omicron_seq = sum(n, na.rm = TRUE))

# GISAID Omicron global summary stats
GISAID_print<-omicron_seq%>%
  mutate(Country = countrycode(code, origin = 'iso3c', destination = 'country.name'))%>%
  rename(Count = cum_omicron_seq)%>%select(Country, Count)


# combine the two tables
omicron_seq<-full_join(omicron_seq, BNO_omicron, by = "code")
omicron_seq<-distinct(omicron_seq) # remove duplicate rows
omicron_seq$max_omicron<- rep(0, nrow(omicron_seq))
# Set max
for (i in 1:nrow(omicron_seq)){
  omicron_seq$max_omicron[i]<-max(c(omicron_seq$cum_omicron_seq[i], omicron_seq$BNO_confirmed[i]), na.rm = TRUE)
}

omicron_seq$max_omicron[omicron_seq$max_omicron== -Inf]<-NA

omicron_seq<- omicron_seq%>%
  mutate(country_name = countrycode(code, origin = 'iso3c', destination = 'country.name'))
# rename columns
omicron_seq_print<-omicron_seq%>%select(
  country_name,BNO_confirmed, BNO_probable, cum_omicron_seq)%>%rename(
  `Country/Region/Territory` = country_name,
  Confirmed = BNO_confirmed,
  Probable = BNO_probable, 
  GISAID = cum_omicron_seq
)

n_GISAID_omicron_seq<-sum(omicron_seq$cum_omicron_seq, na.rm = TRUE)
n_GISAID_omicron_countries<-sum(!is.na(omicron_seq$cum_omicron_seq))
n_total_omicron_cases<-sum(omicron_seq$max_omicron, na.rm = TRUE) # sum of the max of GISAID or newsnodes
n_confirmed_omicron_cases<-sum(omicron_seq$BNO_confirmed, na.rm = TRUE) # sumof only newsnodes confirmed
diff_total_and_confirmed<-n_total_omicron_cases-n_confirmed_omicron_cases
n_countries_w_cases<-sum(!is.na(omicron_seq$max_omicron))
omicron_global_summary<-data.frame(n_total_omicron_cases, n_countries_w_cases, n_confirmed_omicron_cases,
                                   diff_total_and_confirmed, n_GISAID_omicron_seq, n_GISAID_omicron_countries)

# join GISAID data with omicron sequence counts
gisaid_summary_df<-left_join(gisaid_recent_data, omicron_seq, by = c("country_code" = "code"))

# Make a column with omicron sequences where absenses are NAs
gisaid_summary_df$cum_omicron_seq_NA<-gisaid_summary_df$cum_omicron_seq
gisaid_summary_df$max_omicron_seq_NA<-gisaid_summary_df$max_omicron
gisaid_summary_df$max_omicron_seq_NA<-as.integer(gisaid_summary_df$max_omicron_seq_NA)


#Make another column where missing omicron sequences are 0s
gisaid_summary_df$cum_omicron_seq[is.na(gisaid_summary_df$cum_omicron_seq)]<-0
gisaid_summary_df$max_omicron[is.na(gisaid_summary_df$max_omicron)]<-0
gisaid_summary_df$max_omicron<-as.integer(gisaid_summary_df$max_omicron)

# Log transform max_prevalence_variant_pct
gisaid_summary_df$log10_max_prevalence_variant_pct<-log10(gisaid_summary_df$max_prevalence_variant_pct)

# Round percent to 2 decimal points
gisaid_summary_df$max_prevalence_variant_pct[gisaid_summary_df$max_prevalence_variant_pct>0.01]<-round(
      gisaid_summary_df$max_prevalence_variant_pct[gisaid_summary_df$max_prevalence_variant_pct>0.01],2)
# If <0.01, set as 0.01
gisaid_summary_df$max_prevalence_variant_pct[gisaid_summary_df$max_prevalence_variant_pct<=0.01]<-0.01


# Add a column with numbers and flags
gisaid_summary_df$max_prevalence_variant_pct_flags<-gisaid_summary_df$max_prevalence_variant_pct
gisaid_summary_df$max_prevalence_variant_pct_flags[gisaid_summary_df$max_prevalence_variant_pct<=0.01]<-'<0.01'
gisaid_summary_df$max_prevalence_variant_pct_flags[gisaid_summary_df$max_prevalence_variant_pct>=95]<-'not estimated, insufficient recent sequencing'
gisaid_summary_df$max_prevalence_variant_pct_flags[gisaid_summary_df$cum_tpr<0.002 & gisaid_summary_df$max_prevalence_variant_pct>=95]<-'minimal recent COVID cases'

gisaid_summary_df$max_prevalence_variant_pct_w_pct<-gisaid_summary_df$max_prevalence_variant_pct
gisaid_summary_df$max_prevalence_variant_pct_w_pct<-paste0(gisaid_summary_df$max_prevalence_variant_pct, '%')
gisaid_summary_df$max_prevalence_variant_pct_w_pct[gisaid_summary_df$max_prevalence_variant_pct<=0.01]<-'<0.01%'
gisaid_summary_df$max_prevalence_variant_pct_w_pct[gisaid_summary_df$max_prevalence_variant_pct>=95]<-'not estimated, insufficient recent sequencing'
gisaid_summary_df$max_prevalence_variant_pct_w_pct[gisaid_summary_df$cum_tpr<0.002 & gisaid_summary_df$max_prevalence_variant_pct>=95]<-'minimal recent COVID cases'



# Load and join shapefile for flourish
shapefile <- read_delim(SHAPEFILES_FOR_FLOURISH_PATH, delim = "\t") %>%
 rename(country_code = `3-letter ISO code`) %>%
 select(geometry, Name, country_code)

lat_long<-read.csv(LAT_LONG_FOR_FLOURISH_PATH)%>% clean_names()%>%
  rename(country_code= x3_letter_iso_code) %>%
  select(country_code, latitude, longitude)

gisaid_summary_df <-left_join(shapefile, gisaid_summary_df, by = 'country_code')
gisaid_summary_df <- left_join(lat_long, gisaid_summary_df, by = "country_code")

# select cols for flourish and ensure that they're present in the df
stopifnot ("Error: gisaid_summary_df.csv does not contain all necessary columns" = 
             c('geometry', 'latitude', 'longitude', 'Name', 'max_omicron',
               'max_omicron_seq_NA', 'cases_per_100k_last_7_days') %in% colnames(gisaid_summary_df))

# Check to make sure cases are filled in for say USA
stopifnot("Error: case data not in gisaid_summary_df" = 
            !is.na(gisaid_summary_df$cases_per_100k_last_7_days[gisaid_summary_df$country_code=="USA"]))

# only output the necessary columns!
gisaid_summary_df<-gisaid_summary_df%>% select(geometry,latitude, longitude, Name, max_omicron, 
                                               max_omicron_seq_NA, cases_per_100k_last_7_days)
# Ready to for output
gisaid_summary_df<-distinct(gisaid_summary_df)







# -------- Global Combined Omicron from BNO and GISAID, and GISAID+FIND stats over time --------------------------------
# Keeps only the latest timestamp only for that day
BNO_omicron_t<-BNO_omicron_t%>%group_by(date)%>%
  mutate(time_diff= (abs(time_of_day) - 13))%>%
  filter(time_diff == min(time_diff))


# Global Omicron cases by time
BNO_global_t<-BNO_omicron_t%>%group_by(date)%>%
  summarise(n_countries_BNO = n(),
            n_case_BNO = sum(BNO_confirmed, na.rm = TRUE))
#write.csv(BNO_global_t, "../data/processed/BNO_global_t.csv")

# Make omicron sequences from GISAID a global time series
omicron_t<-read.csv(OMICRON_DAILY_CASES)
omicron_t<-omicron_t%>%rename(GISAID_sequences = n)
omicron_t$submission_date<-as.Date(omicron_t$submission_date)
omicron_t$GISAID_sequences<-replace_na(omicron_t$GISAID_sequences, 0)


# Need to make a column for cumulative sequences 
omicron_t<-omicron_t%>%group_by(code)%>%
  mutate(cum_GISAID_seq = cumsum(GISAID_sequences))

# remove rows where cumulative GISAID sequences is 0
omicron_t<-omicron_t%>%filter(cum_GISAID_seq!=0)
  

#Summarise by date (need to make this submission date)
GISAID_omicron_t<-omicron_t%>%group_by(submission_date)%>%
  summarise(n_countries_GISAID = n(),
            n_seq_GISAID = sum(replace_na(cum_GISAID_seq, 0)))

omicron_merge_country_date<-full_join(omicron_t, BNO_omicron_t, by = c("submission_date"= 
"date", "code"= "code"))

omicron_merge_country_date<-distinct(omicron_merge_country_date) # remove duplicate rows
omicron_merge_country_date$max_omicron<- rep(0, nrow(omicron_merge_country_date))
# Set max
for (i in 1:nrow(omicron_merge_country_date)){
  omicron_merge_country_date$max_omicron[i]<-max(c(omicron_merge_country_date$cum_GISAID_seq[i], omicron_merge_country_date$BNO_confirmed[i]), na.rm = TRUE)
}


# On each day, find total countries reporting omicron cases 
# and total cases from max of both sources
omicron_global_t<-omicron_merge_country_date%>%group_by(submission_date)%>%
  summarise(n_countries_all = n(),
            n_cases_all = sum(max_omicron),
            n_cases_BNO = sum(BNO_confirmed, na.rm = TRUE),
            n_seq_GISAID = sum(cum_GISAID_seq, na.rm = TRUE))

# Merge with GISAID metadata global
gisaid_global_t<-gisaid_t%>%group_by(collection_date)%>%
  summarise(
    rolling_cases_last_7_days = sum(rolling_cases_last_7_days),
    rolling_cases_last_30_days = sum(rolling_cases_last_30_days),
    #rolling_seq_last_30_days = sum(rolling_seq_last_30_days),
    n_new_sequences = sum(n_new_sequences),
    n_new_cases = sum(owid_new_cases)
  )%>%
  mutate(
    #percent_of_cases_seq_last_30_days = 100*rolling_seq_last_30_days/rolling_cases_last_30_days,
    new_cases_7d_avg = round(zoo::rollmean(n_new_cases, 7, fill = NA),2),
    new_seq_7d_avg = round(zoo::rollmean(n_new_sequences, 7, fill = NA),2)
  )

gisaid_global_t <- gisaid_global_t %>%
  mutate(rolling_cases_lag_7 = c(rep(NA_integer_, 7), gisaid_global_t$rolling_cases_last_7_days[1:(nrow(gisaid_global_t)-7)]),
         rolling_cases_lag_30 = c(rep(NA_integer_, 30), gisaid_global_t$rolling_cases_last_30_days[1:(nrow(gisaid_global_t)-30)]))

# Merge with the cases_sequenced_by_time_window and lagged version
seq_last_30_days<-read_csv(SEQUENCES_LAST_30_DAYS)%>%
  rename(cases_seq_last_30_days = n,
         cases_seq_last_30_days_lag_30 = n_lag_30)%>%
  select(date, cases_seq_last_30_days, cases_seq_last_30_days_lag_30)%>%
 filter(date<=LAST_DATA_PULL_DATE & date>=as.Date(FIRST_DATE))

toplines_t<-full_join(gisaid_global_t, seq_last_30_days, by = c("collection_date" = "date"))
toplines_t<-toplines_t%>%mutate(
  pct_cases_seq_30_day_window = round(100*cases_seq_last_30_days/rolling_cases_last_30_days,1)
)



global_t<-full_join(omicron_global_t,gisaid_global_t, by = c("submission_date" = "collection_date"))
global_t<-global_t%>%arrange((submission_date))
global_t<-left_join(global_t, seq_last_30_days, by = c("submission_date" = "date"))
global_t<-global_t%>%rename(n_omicron_seq = n_seq_GISAID, n_omicron_cases = n_cases_all)
#write.csv(global_t, '../data/processed/all_metrics_global_t.csv')





# -------- Make the toplines -------------------------------------------------------------
today <- lubridate::today('EST')
global_today<-global_t%>%filter(submission_date==(today-6) | # grab todaya nd a week ago
                                submission_date==today)%>%
              select(submission_date, n_omicron_cases,n_countries_all, n_omicron_seq)%>%
 rename(n_omicroncases = n_omicron_cases,
         n_countries = n_countries_all,
         n_omicronseq = n_omicron_seq)%>%
  mutate(change_omicroncases = diff(n_omicroncases),
         change_countries = diff(n_countries),
         change_omicronseq = diff(n_omicronseq))%>%
  filter(submission_date == today)%>%
  mutate(pctchange_omicroncases = round(100*change_omicroncases/(n_omicroncases - change_omicroncases),1),
         pctchange_countries = round(100*change_countries/(n_countries - change_countries),1),
         pctchange_omicronseq = round(100*change_omicronseq/(n_omicronseq - change_omicronseq),1))

# Pivot the dataframe so sources are rows and the number, increase, and percent change are columns
topline_df<-global_today%>%pivot_longer(
  cols = !submission_date,
  names_to = c("metric", "type"),
               names_sep = "_",
               values_to = "value")%>%
  select(!submission_date)%>%pivot_wider(
  names_from = metric,
  values_from = value
)
# Remove the sequences one since we're  not suing currently
topline_df<-topline_df%>%filter(type!="omicronseq")



# Find the total number, raw change, and % change of cases reported as of yesterday
cases_recent<-global_t%>%filter(submission_date==LAST_DATA_PULL_DATE)%>%
  select(submission_date, rolling_cases_last_7_days, n_new_cases, rolling_cases_lag_7)%>%
  mutate(change_cases_rolling_7_day_sum = (rolling_cases_last_7_days - rolling_cases_lag_7),
         pctchange_cases_rolling_7_day_sum = round(100*change_cases_rolling_7_day_sum/(rolling_cases_lag_7),1))%>%
  select(!submission_date)

# rename so we can bind to toplin df
cases_df<-cases_recent%>%select(!rolling_cases_lag_7)%>%rename(
  n = rolling_cases_last_7_days, change = change_cases_rolling_7_day_sum,
  pctchange = pctchange_cases_rolling_7_day_sum)%>%
  mutate(type = "global_cases")%>%
  select(type, n, change, pctchange)

# Combine with the omicron global summary
topline_df<-rbind(topline_df, cases_df)






# Do the same with sequences over the last 30 days, need to add column for sequences, by collection date
sequences_recent<-global_t%>%filter(submission_date==LAST_DATA_PULL_DATE)%>%
  select(submission_date,  rolling_seq_last_30_days, rolling_cases_last_30_days,
         cases_seq_last_30_days_lag_30,rolling_cases_lag_30)%>%mutate(
           percent_of_cases_seq_last_30_days = round(100*cases_seq_last_30_days/rolling_cases_last_30_days,2),
           previous_percent_of_cases_seq_30_days = round(100*cases_seq_last_30_days_lag_30/rolling_cases_lag_30,2),
           change_pct_cases_seq = (percent_of_cases_seq_last_30_days - previous_percent_of_cases_seq_30_days),
           pctchange_pct_cases_seq = round(100*change_pct_cases_seq/previous_percent_of_cases_seq_30_days,1))
type = "pct_cases_seq"
seq_df<-sequences_recent%>%select(!submission_date & !rolling_cases_last_30_days & !rolling_seq_last_30_days
                                  & !cases_seq_last_30_days_lag_30 & !rolling_cases_lag_30
                                  & !previous_percent_of_cases_seq_30_days)%>%rename(
                                    n = percent_of_cases_seq_last_30_days,
                                    change = change_pct_cases_seq,
                                    pctchange = pctchange_pct_cases_seq)%>%
                                mutate(type = "pct_cases_seq")%>%
                                select(type, n, change, pctchange)

topline_df<-rbind(topline_df, seq_df)




# Make topline df pretty! *** NOTE THIS IS SUPER HARDCODED IN***


# Change all but percent to be formatted with commas!
topline_df$n[1:3]<-comma_format()(topline_df$n[1:3])
topline_df$change[1:3]<-comma_format()(topline_df$change[1:3])
# add percents
topline_df$n[4]<-paste0(topline_df$n[4], ' %')
topline_df$change[4]<-paste0(topline_df$change[4], ' %')
topline_df$pctchange<-paste0(topline_df$pctchange, ' %')

# Add pluses to the changes and percent changes
topline_df$change[topline_df$change>0]<-paste0('+ ', topline_df$change[topline_df$change>0])
topline_df$pctchange[topline_df$pctchange>0]<-paste0('+ ', topline_df$pctchange[topline_df$pctchange>0])

# add column for timeframe
topline_df$change_from = c("Last week", "Last week", "7 days ago", "30 days ago")

# Add descriptors of metrics
topline_df$Metric <-c("Cumulative confirmed Omicron cases",
                      "Countries/territories with confirmed Omicron case(s)",
                      "Total Covid-19 cases reported in the last 7 days",
                      "Percent of Covid-19 cases sequenced in the last 30 days*")

topline_df<-topline_df%>%select(Metric, n, change, pctchange, change_from)%>%
  rename(Value = n, `Change (#)` = change, `Change (%)`= pctchange, `Change from` = change_from)


# ----- Output paths ------------------------------------------------------

# Domino path
if (USE_CASE == 'domino'){
write.csv(omicron_seq_print, "/mnt/data/processed/omicron_seq.csv")
write.csv(omicron_global_summary, '/mnt/data/processed/sitrep_summary.csv')
write.csv(topline_df, '/mnt/data/processed/topline_df_weekly.csv')
write.csv(gisaid_summary_df, "/mnt/data/processed/gisaid_summary_df.csv")
write.csv(global_t, "/mnt/data/processed/all_metrics_global_t.csv")
write.csv(GISAID_omicron_t, "/mnt/data/processed/GISAID_omicron_t.csv")
write_csv(toplines_t, '/mnt/data/processed/processed_toplines_t.csv')
}

if (USE_CASE == 'local'){
write.csv(omicron_seq_print, "../data/processed/omicron_seq.csv") # omicron by country currently
write.csv(topline_df, '../data/processed/topline_df.csv') #toplines for carousel
write.csv(gisaid_summary_df, "../data/processed/gisaid_summary_df.csv") # country-level metrics for FLourish
write.csv(global_t, '../data/processed/all_metrics_global_t.csv') #data for global timecourse
write.csv(GISAID_omicron_t, "../data/processed/GISAID_omicron_t.csv")
}


