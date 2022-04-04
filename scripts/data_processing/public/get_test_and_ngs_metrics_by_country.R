# Original Author: Nathan Poland
# Updated: Briana Thrift & Zach Susswein
# Current Author: Kaitlyn Johnson
#Second Author: Briana Thrift


# This script takes in the GISAID metadata, OWID and FIND testing data and computes a number of metrics that will be used
# to define country archetypes to assess surveillance capacity.

#clears (rm = removes) all objects from the workspace
rm(list = ls())

#Sys.getenv obtains the values of the environment variables
USE_CASE = Sys.getenv("USE_CASE")
#if USE_CASE is empty, then replace space with "local"
if(USE_CASE == ""){
  USE_CASE<-'local'
}
# USE_CASE = domino used for automatic update




#---- Packages----------
#If USE_CASE = 'Domino' (virtual environment for automatic data updates) then install these libraries
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
install.packages("dplyr", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("scales", dependencies=TRUE, repos='http://cran.us.r-project.org')
}

#---- Libraries----------
#install these libraries regardless of "USE_CASE"
library(tidyverse) # data wrangling
library(tibble) # data wrangling
library(janitor) # column naming
library(countrycode) # country codes
library(lubridate) # date times
library(readxl) # excel import
library(zoo) # calculate rolling averages
library(R.utils) # R utilities
library(stringr) # to parse strings in R
library(dplyr) # data wrangling
library(scales) # comma formatting
 
# ------ Name data paths and set parameters -------------------------------------------

#pulls todays date in, using Eastern Standard Time
#substr: Extract or replace substrings in a character vector, starting at 1 character length, and ending at 13
today <- substr(lubridate::now('EST'), 1, 13)
today <- chartr(old = ' ', new = '-', today)
today_date<-lubridate::today('EST')

#pulling in the current month
current_month<-month.name[month(today_date)]
current_year<-year(today_date)
current_folder<-str_c(current_month, current_year, sep = '_')
last_update_date<-today_date - months(1)

#?????: Does this prev_month of November overwrite the other prev_month definition?
prev_month<-month.name[month(last_update_date)]
prev_month<-"November"
prev_year<-year(last_update_date)
prev_year<-"2021"
prev_folder<-str_c(prev_month, prev_year, sep = '_')


## Set filepaths

#FIND Test Tracker data pulled from github
ALL_DATA_PATH<- url("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv")

#Out-dated FIND NGS map methodologies that were lives/updated in November. 
OLD_FIND_MAP_PATH<-url(paste0("https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/", prev_folder, "/PPI/find_map_11.30.2021.csv"))
#Lat/long github repo
LAT_LONG_DATA<-url("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv")

#if running in domino, run the following pathways to ingest this data from Domino folders:
if (USE_CASE == 'domino'){
GISAID_DAILY_PATH<-'/mnt/data/processed/gisaid_owid_merged.csv' # output from gisaid_metadata_processing.R
SHAPEFILES_FOR_FLOURISH_PATH <- '/mnt/data/Geospatial_Data/geometric_polygons_country.txt' #shapefiles for mapping
WHO_REGIONS_PATH<-'/mnt/data/additional_sources/WHO_region_data.csv' # WHO country list
ECONOMY_PATH<-'/mnt/data/additional_sources/WB_class_data.xls' #World Bank socioeconomic class data
FIND_TESTING_SEQ_RAW_PATH<- '/mnt/data/additional_sources/Sequencing_labs_data.xlsx' # WHO NGS facility data
LAT_LONG_DATA <- '/mnt/data/Geospatial_data/country_lat_long_names.csv' #lat/long coordinates data
}

#if running locally, run the following local pathways to ingets data from local folders (pulled from github)
if (USE_CASE == 'local'){
GISAID_DAILY_PATH<-'../../../../data/processed/gisaid_owid_merged.csv' # output from gisaid_metadata_processing.R
SHAPEFILES_FOR_FLOURISH_PATH <- '../../../../data/Geospatial_Data/geometric_polygons_country.txt' # shapefiles for mapping
WHO_REGIONS_PATH<-'../../../../data/additional_sources/WHO_region_data.csv' # WHO country list
ECONOMY_PATH<-'../../../../data/additional_sources/WB_class_data.xls' #World Bank socioeconomic class data
FIND_TESTING_SEQ_RAW_PATH<- '../../../../data/additional_sources/Sequencing_labs_data.xlsx' # WHO NGS facility data
LAT_LONG_DATA <- '../../../../data/Geospatial_data/iso_3_centroids.csv' #lat/long coordinates data
}

#Create last-data pull dates and make sure that the time window for the data pull is within 364 days
LAST_DATA_PULL_DATE<-as.Date(substr(lubridate::now('EST'), 1, 10))-days(1) #Make this based off of yesterday! Because of data lag
LAST_DATA_PULL_DATE<-as.Date("2022-04-01")-days(1) #Change this per month of data update
FIRST_DATE<-"2019-12-01" # First data that we would expect to see SARS-CoV-2 genomes/cases/tests
TIME_WINDOW_YEAR<-364 #Pull within the last 12 months, so 364 days (minus one from 365 days)






# ------ WHO countries, regions, and code ---------------------------------------------

# import csv of WHO region countries 
WHO_regions <- read.csv(WHO_REGIONS_PATH) %>%
  # standardize names with this janitor function
  clean_names() %>%
  # rename column from name to "country_name"
  rename(name = country_name)
# All additional variables will be joined to this dataset




# ------ Categorical WHO NGS facility data -------------------------------------
# Reading in all countries and subsetting to countries with valid names
test_seq_raw <- read_excel(FIND_TESTING_SEQ_RAW_PATH,
                           sheet = "Country classification",
                           skip = 1) %>%
  #clean names are those that do not have missing country codes
  clean_names()%>%
  filter(country_code != "x") %>% 
  filter(country_code != "") %>%
  filter(country_code != " ")

# get only ngs facility data
#???? Explain here, is "code" the column or "country_code"? 
#??? Is this not case sensitive, since the actual dataset is "NGS capacity" column?
ngs_clean <- test_seq_raw %>%
  select(contains("code"), starts_with("ngs_capacity_"))

# define  capacity column for later reference
ngs_capacity_column <- colnames(ngs_clean)[max(ncol(ngs_clean))]


# assign ngs_capacity variable a label
# 0: "0 - 0 NGS facilities" or 0
# 1: "1 - 1-3 NGS facilities or equivalent" or 1
# 2: "2 - 4+ NGS facilities or equivalent" or 2
#variable is ngs_capacity, and writing this function on ngs_capacity_column
ngs_clean$ngs_capacity <- case_when(
  ngs_clean[ , ngs_capacity_column] == 0 ~ 0,
  ngs_clean[ , ngs_capacity_column] == "0 - No NGS facilities" ~ 0,
  ngs_clean[ , ngs_capacity_column] == 1 ~ 1,
  ngs_clean[ , ngs_capacity_column] == "1 - 1-3 NGS facilities or equivalent" ~ 1,
  ngs_clean[ , ngs_capacity_column] == 2 ~ 2,
  ngs_clean[ , ngs_capacity_column] == "2 - >3 NGS facilities or equivalent" ~ 2
)
# Make a binary facility access variable
ngs_clean<-ngs_clean%>%mutate(
  facility_access = case_when(
    ngs_capacity ==0 ~ "Does not have access to sequencing facilities",
    (ngs_capacity ==1 | ngs_capacity == 2) ~ "Has access to sequencing facilities"
  
))


# select only code, capacity, and binary facility variables
ngs_clean <- ngs_clean %>%
  select(contains("code"),  ngs_capacity, facility_access)

# find_clean: merge sequencing capacity data into template
#changed the original "find_clean" to avoid duplicate or overwritten dataframes
find_clean <- left_join(WHO_regions, ngs_clean, by = c("code" = "country_code"))

# Make a cleaned sequencing_capacity variable to give more granular data on number of facilities
# Variable indicating evidence of Sequencing Capacity
# Variable is based on WHO slides on facilities, GISRS data for extraction of capacity & testing data
# Variable is based on confidential manufacturer data on install bases
find_clean <- find_clean %>%
  mutate(
    # just recoding what is in raw data
    sequencing_capacity = case_when(
      ngs_capacity == 0 ~ "0 NGS facilities or equivalent",
      ngs_capacity == 1 ~ "1-3 NGS facilities or equivalent",
      ngs_capacity == 2 ~ "4+ NGS facilities or equivalent",
      is.na(ngs_capacity) == T ~ "0 NGS facilities or equivalent" #??? What is this T??, know this is for missing values
    )
  )

#??? Can I move this ^^^ before the merge?



# ------ FIND testing data to estimate testing metric -------------------------------------
 
# import csv from FIND containing country, date, population size, tests, 
find_raw <- read.csv(ALL_DATA_PATH) %>% clean_names()
 
# select and rename necessary columns
find_testing_t <- find_raw %>%
  # filter for country set
  filter(set == "country") %>%
  # select the smoothed and raw test and case data by day (case data is also from OWID)
  select(name, time, unit, pop_100k, all_new_tests, all_new_cases, 
         pos, new_tests_orig, new_cases_orig) %>%
  # rename columns as date, code, pop_100k, new_tests_smoothed, cum_tests, cum_cases, new_cases_smoothed
  rename(country = name, date = time, code= unit, new_tests_smoothed = all_new_tests, 
           new_cases_smoothed = all_new_cases) %>%
  
  # parse date as date class
  mutate(date = as.Date(date, format = "%Y-%m-%d"), #reformatting date
         pop = pop_100k*100000, # get full pop
         code = countrycode(country, origin = 'country.name', destination = 'iso3c')) # make country code column for joining
      # what does this dooo with "destination 'iso3c'?

# new_tests_smoothed runs only up until the most recent date of test reporting. Therefore, we need cases to also be truncated
# at this point to calculate the test positivity rate properly. We replace all case data after the data of tests last being reported
# as NA (last day of smoothed test data)

# Add a column that has the new cases smoothed with NA entries when new_tests_smoothed is NA
# if find_testing_t a new dataframe?
find_testing_t['new_cases_smoothed_truncated']<-find_testing_t$new_cases_smoothed
find_testing_t$new_cases_smoothed_truncated[is.na(find_testing_t$new_tests_smoothed)]<-NA


# inserts missing country codes manually 
find_testing_t$code[find_testing_t$country == "Kosovo"] <- "XKX"
find_testing_t$code[find_testing_t$country == "Namibia"] <- "NAM"

# FILL IN MISSING DATES (just in case are ahy)
# set start date
first_date<-min(find_testing_t$date, na.rm = TRUE)
date <- seq.Date(first_date, LAST_DATA_PULL_DATE, by = "day")
code <-unique(find_testing_t$code)
date_country<-expand_grid(date, code)
#join the find_test_t dataframe with filled dates df
find_testing_t<-left_join(date_country,find_testing_t, by = c("code", "date"))



# For each country, find the date they last reported raw new tests
find_test_update_date<-find_testing_t%>%
  group_by(code) %>% select(code, date, new_tests_orig)%>% #Is new_test_orig an existing column name? lost here
  filter(!is.na(new_tests_orig) & new_tests_orig != 0)%>%
  filter(date == max(as.Date(date)))%>%
  mutate(date_tests_last_reported = date)%>%
  select(code, date_tests_last_reported) %>%
  mutate(
    rept_tests_within_last_6_months = as.Date(date_tests_last_reported)> (LAST_DATA_PULL_DATE-180), #why 180?
    days_since_tests_reported = LAST_DATA_PULL_DATE - as.Date(date_tests_last_reported))


# Unit test for report date
stopifnot('last reported date is more recent than data pull date' = sum(as.Date(find_test_update_date$date_tests_last_reported) > 
                                                                          as.Date(LAST_DATA_PULL_DATE), na.rm = TRUE)== 0)


# Compute the testing metrics over the past year
find_testing_last_year<- find_testing_t %>% filter(date>=(LAST_DATA_PULL_DATE -TIME_WINDOW_YEAR) & 
                                                     date<= LAST_DATA_PULL_DATE)%>%
  group_by(code) %>%
  summarise(tests_in_last_year_raw = sum(new_tests_orig, na.rm = TRUE),
            cases_in_last_year_raw = sum(new_cases_orig, na.rm = TRUE),
            tests_in_last_year_smoothed = sum(new_tests_smoothed, na.rm = TRUE),
            cases_in_last_year_smoothed = sum(new_cases_smoothed, na.rm = TRUE),
            cases_in_last_year_smoothed_truncated = sum(new_cases_smoothed_truncated, na.rm = TRUE),
            avg_tpr_find = mean(pos, na.rm = TRUE), # The way that FIND calculates TPR (which they named 'pos') (average over a daily value)
            # We calculate TPR as a cumulative of cases over tests in the past year 
            tpr_year_raw = cases_in_last_year_raw/tests_in_last_year_raw, # uses only raw data, overestimates because of delayed test reporting
            tpr_year_smoothed = cases_in_last_year_smoothed/tests_in_last_year_smoothed, # overestimates because doesn't account for truncation 
            tpr_year_smoothed_truncated = cases_in_last_year_smoothed_truncated/tests_in_last_year_smoothed, # used for archetype definition
            #convert TPR to a percentage:
            tpr = 100*tpr_year_smoothed_truncated,
            # ??? why max(pop) instead of just population?
            avg_daily_test_per_1000_last_year_raw = 1000*mean(new_tests_orig/max(pop), na.rm = TRUE),
            avg_daily_tests_per_1000_last_year_smoothed = 1000*mean(new_tests_smoothed/max(pop), na.rm = TRUE), # used for archetype definition
            population_size = max(pop)# pops should all be the same
            )%>% 
  rename(avg_daily_tests_per_1000_last_year_smoothed = avg_daily_tests) # rename long columnn name to average daily test
  filter(!is.na(code))


# Find the countries where they haven't reported tests in the past year
no_avg_tpr<-find_testing_last_year%>%filter(is.na(avg_tpr_find))
no_avg_daily_tests<-find_testing_last_year%>%filter(is.na(avg_daily_tests_per_1000_last_year_smoothed))

# Join last year with data on test updating
find_testing_clean<-left_join(find_test_update_date, find_testing_last_year, by = "code")
# Number of countries that haven't reported tests in past 6 months but have in past year 
n_not_rept_6_mos= nrow(find_testing_clean%>%filter(!is.na(avg_tpr_find) & rept_tests_within_last_6_months == FALSE))
find_not_reported<-find_testing_clean%>%filter(rept_tests_within_last_6_months==FALSE & !is.na(avg_tpr_find))

# Add some unit tests
stopifnot('Countries reporting greater than a test per person per day' = sum(find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed>1000,na.rm = T)== 0)
stopifnot('More than 25 countries with data havent reported tests in 6 months' = n_not_rept_6_mos<=25)
stopifnot('More than 35 countries are missing average daily TPR from FIND' = nrow(no_avg_tpr)<=35)

# remove any negative daily tests or impossible TPRs
find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed<- ifelse(find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed < 0, NA, 
                                                                   find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed)
find_testing_clean$tpr_year_smoothed_truncated <- ifelse(find_testing_clean$tpr_year_smoothed_truncated < 0 | find_testing_clean$tpr_year_smoothed_truncated > 1, NA, 
                                               find_testing_clean$tpr_year_smoothed_truncated)


# Join to find_clean
#would like to rename these dataframes so then they are not always overwriting one another
find_clean<-left_join(find_clean, find_testing_clean, by = "code")


#------Dx Testing Capacity Classifier------------------

# Create Dx Testing Capacity Classifier variable, labeled as binary of Does or Does NOT meet testing target
# based on the truncated TPR and average daily tests per 1000 last year using smoothed cases and test data
# If countries have not reported in 6 months or are missing TPR reported to FIND, get labeled as having insufficient data to 
#assess test capacity
daily_tests_thres<- 0.5 #0.5 average daily tests target
TPR_thres<- 20 #20% TPR target
ACT_A_target<-1 #ACT-A-Accelerator targets by WHO
find_clean <- find_clean %>%
  # create new variable for Dx testing capacity based on:
  # TPR over the past year > 20%
  # Unless average daily tests per 1000 > 0.15 (PPI/FIND targets)
  mutate(
    # dx_testing_capacity = case_when(
    #   (is.na(avg_tpr_find)  | rept_tests_within_last_6_months ==FALSE | is.infinite(avg_tpr_find)) ~ "Insufficient testing data",
    #   avg_daily_tests_per_1000_last_year_smoothed >= daily_tests_thres ~ "Meets testing target", # upper left
    #   avg_daily_tests_per_1000_last_year_smoothed < daily_tests_thres ~ "Does not meet testing target", #bottom right
    # ),
    
    # dx_testing_capacity = case_when(
    #   (is.na(avg_tpr_find)  | rept_tests_within_last_6_months ==FALSE | is.infinite(avg_tpr_find)) ~ "Insufficient testing data",
    #   avg_daily_tests_per_1000_last_year_smoothed >= daily_tests_thres ~ "Meets testing target", # upper left
    #    avg_daily_tests_per_1000_last_year_smoothed <= daily_tests_thres ~ "Does not meet testing target", #bottom right
    #   tpr_year_smoothed_truncated >=TPR_thres & avg_daily_tests_per_1000_last_year_smoothed > daily_tests_thres ~ "Does not meet testing target", # upper right
    #   tpr_year_smoothed_truncated < TPR_thres & avg_daily_tests_per_1000_last_year_smoothed <= daily_tests_thres ~ "Does not meet testing target" # bottom left
    # ),
    
    dx_testing_capacity = case_when(
      (is.na(avg_tpr_find)  | rept_tests_within_last_6_months ==FALSE | is.infinite(avg_tpr_find)) ~ "Insufficient testing data",
      # define the 4 quadrants of daily tests vs TPR, only the upper left is NOT in test (i.e. must have <15% TPR & >0.15 test per 1000)
      tpr >= TPR_thres & avg_daily_tests <= daily_tests_thres ~ "Does not meet testing target", #bottom right
      tpr >=TPR_thres & avg_daily_tests > daily_tests_thres ~ "Does not meet testing target", # upper right
      tpr < TPR_thres & avg_daily_tests <= daily_tests_thres ~ "Does not meet testing target", # bottom left
      tpr < TPR_thres & avg_daily_tests > daily_tests_thres ~ "Meets testing target" # upper left
    ),
    
    dx_testing_rec = case_when(
       dx_testing_capacity == "Insufficient testing data" ~ "Insufficient testing data",
       dx_testing_capacity == "Does not meet testing target" ~ "Test - Increase diagnostic testing capacity",
       dx_testing_capacity == "Meets testing target" ~ "Sustain - Sustain diagnostic testing capacity"
    ),
    
    dx_archetype = case_when(
      dx_testing_capacity == "Insufficient testing data" ~ "Insufficient testing data",
      dx_testing_capacity == "Does not meet testing target" ~ "Test",
      dx_testing_capacity == "Meets testing target" ~ "Sustain"
    )
  )

# has_dx_cap<-find_clean%>%filter(dx_testing_capacity == "Meets testing target")%>%
#     select(name, 
#            tpr_year_smoothed_truncated,
#            avg_daily_tests_per_1000_last_year_smoothed)
# write.csv(has_dx_cap, '../../../data/processed/has_dx_cap_daily_tests_and_TPR.csv')

 

# -------- GISAID data on SARS-CoV-2 sequences by country and time --------------------------------

gisaid_raw <- read.csv(GISAID_DAILY_PATH) %>% # raw refers to all variants, by country, by day
  clean_names()

# Replaces with collection date and parse as date
gisaid_raw$collection_date <- as.Date(as.character(gisaid_raw$gisaid_collect_date), format = "%Y-%m-%d")

#select important columns: collection date, country name, number of new sequences, OWID
# covid-19 case data, OWID population data, country code, OWID location (whyy?)
gisaid_t <- gisaid_raw%>%
  select(collection_date, gisaid_country, n_new_sequences,
         owid_new_cases, owid_population, country_code, owid_location)

# Make sure that the most recent date is yesterday (only relevant if we are updating)
if ((max(gisaid_t$collection_date) != LAST_DATA_PULL_DATE)){
  warning("GISAID metadata not updated")
}

# Subset GISAID data to the last  year
gisaid_last_year<-gisaid_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE -TIME_WINDOW_YEAR) & 
                                      collection_date<= LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(cases_in_last_year = sum(owid_new_cases), # total cases in past year from OWID
            cases_per_100k_last_year = round(100000 * owid_new_cases/max(owid_population),3), # total cases per capita
            sequences_in_last_year= sum(n_new_sequences), # total sequences collected and submitted to GISAID in past year 
            pct_cases_sequenced_in_last_year = round(100*(sequences_in_last_year/cases_in_last_year),2), # pct cases sequenced
            sequences_per_100k_last_year = round(100000*sequences_in_last_year/max(owid_population),3) # total sequences per capita
            )

# Add a metric of cumulative sequences (to the end)
#does this add into the gisaid_last_year df? Or is this added later in the script?
gisaid_cumulative<-gisaid_t%>%group_by(country_code)%>%
  summarise(cum_seq = sum(n_new_sequences), #cumulative number of sequences collected and submitted over the entire pandemic
            cum_cases = sum(owid_new_cases), #cumulative number of cases over the entire pandemic
            cum_cases_per_capita = round(100000 *cum_cases/max(owid_population), 3)
            )

gisaid_last_year$pct_cases_sequenced_in_last_year[is.infinite(gisaid_last_year$pct_cases_sequenced_in_last_year)]<-NA
#is this just taking out any answers that are INF, and replacing with NaNs?



# Join both sets of metrics 
find_clean <- left_join(find_clean, gisaid_last_year, by = c("code" = "country_code")) 
#does this mean one df has "code" and another has "country_code"?



#------- Add in old archetype ------------------------------------------------------
if (prev_month == "November" & prev_year == "2021")
  {
  old_find<-read.csv(url('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/November_2021/PPI/find_map_11.30.2021.csv'))%>%
  select(code,country,dx_testing_capacity_clean, sars_cov_2_sequencing, archetype)%>%

#when renaming, does order of variables matter?
  rename(prev_test_rec = dx_testing_capacity_clean, # dx_testing_capacity
         old_archetype = archetype, # archetype_orig_w_HICs
         old_sequencing_archetype = sars_cov_2_sequencing)%>%filter(!is.na(code))%>%filter(country != "West Bank and Gaza") # 237 countries, filter out Palastine 2 country  names
  }

# for following months, after first new methodologies iteration
if (prev_month!= "November" & prev_year != "2021")
  { 
  old_find<-read.csv(OLD_FIND_MAP_PATH)%>%
    select(code,country,dx_testing_rec, sars_cov_2_sequencing, archetype_orig_w_HICs)%>%
    rename(prev_test_rec = dx_testing_rec, 
           old_archetype = archetype_orig_w_HICs)
  }
#subset all unique code values from this old FIND NGS data into "old_codes" dataframe
old_codes<-unique(old_find$code) # 237 of them

# filter to only codes in old data
find_clean<-find_clean%>%
  filter(code %in% old_codes)

find_clean<-left_join(find_clean, old_find, by = "code")
find_clean<-find_clean%>%filter(name != "West Bank and Gaza")%>% #filer out Palastine double country  name
  filter(code != "UMI") # 237 countries #take out UMI country
n_codes <- length(unique(find_clean$code)) #assess number of unique codes in dataframe



# ------ World Bank Economy classifier ------------------------------------------------
world_bank_background_raw <- read_excel(ECONOMY_PATH,
                                        sheet = "List of economies",
                                        skip = 5) %>%
  # standardize names with this janitor function
  clean_names()

# remove buffer rows by iso3 code and select code and testing capacity columns
world_bank_background_clean <- world_bank_background_raw %>%
  # drop all columns except iso3 code and world_bank_economies
  select(4,7)

# rename columns code and world_bank_economies
colnames(world_bank_background_clean) <- c("code", "world_bank_economies")

# find_clean: merge WHO testing data into template
find_clean <- left_join(find_clean, world_bank_background_clean, by = c("code" = "code"))

find_clean<-find_clean%>%
  mutate( LMIC_status = case_when(
  world_bank_economies == 'High income' ~ 'High Income',
  world_bank_economies != 'High income' ~ 'LMIC'))

# Unit tests
stopifnot('Incorrect number of countries'= n_codes<=237 | n_codes>=236)
find_clean_LMICs<-find_clean%>%
  filter(LMIC_status != 'High Income')
#Why 90? Don't we expect this number to change as more LMICs report their data?
stopifnot('Less than 90 LMICs with avg daily TPR not NA & with tests reported in last 6 months' = 
            nrow(find_clean_LMICs%>%
                   filter(!is.na(avg_tpr_find))%>%filter(rept_tests_within_last_6_months == TRUE)) >= 90)





# -------- SARS-CoV-2 sequencing capacity classifier original -----------------------
# Variable assessing if country has met sequencing surveillance target via GISAID sequences
pct_seq_thres<-0.5 #0.5% of cases sequenced in the last year
seq_per_cap_thres<- 10 #>=10 sequences per 100K persons collected/submitted in the last year

find_clean <- find_clean %>%
  # create new variable for SARS-CoV-2 Sequencing based on:
  # % of cases sequenced in the past year > 0.5%
  # cases sequenced per 100k in the past year >= 10
  mutate(
    sars_cov_2_sequencing = case_when(
      ((cases_in_last_year == 0 | is.na(cases_in_last_year)) & (sequences_per_100k_last_year< seq_per_cap_thres | is.na(sequences_per_100k_last_year))) ~ "Insufficient data",
      ((cases_in_last_year == 0 | is.na(cases_in_last_year)) & sequences_per_100k_last_year> seq_per_cap_thres) ~ "Insufficient data", 
      is.na(sequences_per_100k_last_year) ~ "Does not meet sequencing target",
      pct_cases_sequenced_in_last_year >= pct_seq_thres & sequences_per_100k_last_year >= seq_per_cap_thres  ~ "Meets sequencing target", # upper right
      pct_cases_sequenced_in_last_year < pct_seq_thres & sequences_per_100k_last_year >= seq_per_cap_thres ~ "Does not meet sequencing target", #upper left
      pct_cases_sequenced_in_last_year < pct_seq_thres & sequences_per_100k_last_year < seq_per_cap_thres ~ "Does not meet sequencing target", # bottom left
      pct_cases_sequenced_in_last_year >= pct_seq_thres & sequences_per_100k_last_year < seq_per_cap_thres ~ "Does not meet sequencing target", # bottom right

    )
  )

# has_seq_cap<-find_clean%>%filter(sars_cov_2_sequencing == "Meets sequencing target")%>%
#     select(name,
#            pct_cases_sequenced_in_last_year,
#            sequences_in_last_year)
# write.csv(has_seq_cap, '../../../data/processed/has_seq_cap_10.csv')



# -------- Create Archetype classifier variable ----------------------------------------
# Archetype_orig indicates the original archetype names (Strengthen, Connect, Leverage, Test but with the new definitions)
# New archetype names: Sustain, Leverage/Strengthen, Connect/Build
find_clean<-find_clean%>%
  mutate(
  archetype_orig = case_when(
    old_archetype == "High Income*" ~ "High Income*", #excludes HICs
   (sars_cov_2_sequencing == "Insufficient data") ~ "Insufficient data",
    sars_cov_2_sequencing == "Meets sequencing target" ~ "Strengthen",
    (sars_cov_2_sequencing == "Does not meet sequencing target" &
    (ngs_capacity == 2 | ngs_capacity == 1)) ~ "Leverage",
     sars_cov_2_sequencing == "Does not meet sequencing target" & (ngs_capacity == 0 |is.na(ngs_capacity))~ "Connect"),
  
  archetype_orig_w_HICs = case_when( # classifies HICs as well 
    (sars_cov_2_sequencing == "Insufficient data") ~ "Insufficient data",
   sars_cov_2_sequencing == "Meets sequencing target" ~ "Sustain",
    (sars_cov_2_sequencing == "Does not meet sequencing target" &
       (ngs_capacity == 2 | ngs_capacity == 1)) ~ "Strengthen/Leverage",
    sars_cov_2_sequencing == "Does not meet sequencing target" & (ngs_capacity == 0 |is.na(ngs_capacity)) ~ "Connect/Build"),
  
  archetype_draft = case_when( # Lumps together Leverage & Connect if we don't want to use ngs facility access data
    old_archetype == "High Income*" ~ "High Income*",
    sars_cov_2_sequencing == "Insufficient data" ~ "Insufficient data",
    sars_cov_2_sequencing == "Meets sequencing target" ~ "Sustain",
    sars_cov_2_sequencing == "Does not meet sequencing target" ~ "Sequence")
)

#______________________________________________________________________________

# Edit the date variable so that it is in a universally readable format
date_df<-find_clean%>%
  separate(date_tests_last_reported,
                            into = c("year", "month", "day"),
                            sep = "-")
date_df$month<-month.name[as.numeric(date_df$month)]
date_df$date_tests_last_reported<-paste0(date_df$month, ' ', date_df$day, ', ', date_df$year)
#explain
date_df$date_tests_last_reported[date_df$date_tests_last_reported == "NA NA, NA"]<-"No tests reported"
find_clean$date_tests_last_reported<-date_df$date_tests_last_reported


# Internal troubleshooting, generates dataset that groups by archetype for easy validation
find_clean_LMICs<-find_clean%>%filter(LMIC_status != 'High Income')
n_insufficient_data<- sum(find_clean_LMICs$archetype_orig == "Insufficient data")
n_not_tests<-sum(find_clean_LMICs$dx_testing_capacity == "Meets testing target")
n_Test<- sum(find_clean_LMICs$dx_testing_capacity == "Does not meet testing target")
n_Strengthen <-sum(find_clean_LMICs$archetype_orig == "Strengthen")
n_Leverage <- sum(find_clean_LMICs$archetype_orig == "Leverage")
n_Connect <- sum(find_clean_LMICs$archetype_orig == "Connect")
n_Sequence<- sum(find_clean_LMICs$archetype_new == "Sequence")

n_given_archetypes =  n_Strengthen + n_Sequence

# unit test
stopifnot("Number given archetypes other than insufficient data is less than 90 (should be around 95)"= n_given_archetypes>=90)

# Select necessary variables for Flourish map only
find_map<- find_clean %>%select(name, code, population_size, sequencing_capacity, tpr_year_smoothed_truncated, avg_daily_tests_per_1000_last_year_smoothed,
                                dx_testing_capacity, date_tests_last_reported, days_since_tests_reported, pct_cases_sequenced_in_last_year,
                                sequences_per_100k_last_year, sars_cov_2_sequencing, ngs_capacity, facility_access,
                                old_archetype, archetype_orig,
                                archetype_orig_w_HICs, archetype_new, world_bank_economies, prev_test_rec, dx_archetype, dx_testing_rec)
# Find the countries with new archetypes
if (prev_month != "November" & prev_year!= "2021"){
find_changed_archetypes <-find_map%>%filter(old_archetype != archetype_orig_w_HICs, prev_test_rec != dx_testing_rec)%>%
  select(!archetype_orig, !archetype_new)
}

# Make internal validation data sets 
if (USE_CASE == 'local') {
  find_clean%>%filter(archetype_orig == "Insufficient data")%>%write.csv(paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/countries_in_insufficient_data.csv'))
  find_clean%>%filter(dx_testing_rec == "Test - Increase diagnostic testing capacity")%>%write.csv(paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/countries_in_test.csv'))
  find_clean%>%filter(archetype_orig == "Strengthen")%>%write.csv(paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/countries_in_strengthen.csv'))
  find_clean%>%filter(archetype_orig == "Leverage" | archetype_orig == "Connect")%>%
    write.csv(paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/countries_in_insufficient_data.csv'))
  if (prev_month != "November" & prev_year!= "2021"){
    write.csv(find_changed_archetypes, paste0('../../../data/NGS_Data_Tables/PPI/find_changed_archetypes', prev_month, '_to_', current_month, '.csv'))
  }
}


if (USE_CASE == 'domino') {
  find_clean%>%filter(archetype_orig == "Insufficient data")%>%write.csv('/mnt/data/processed/countries_in_insufficient_data.csv')
  find_clean%>%filter(dx_testing_rec == "Test - Increase diagnostic testing capacity")%>%write.csv('/mnt/data/processed/countries_in_test.csv')
  find_clean%>%filter(archetype_orig == "Strengthen")%>%write.csv('/mnt/data/processed/countries_in_strengthen.csv')
  find_clean%>%filter(archetype_orig == "Leverage" | archetype_orig == "Connect")%>%write.csv('/mnt/data/processed/countries_in_Lev_or_Connect.csv')
  if (prev_month != "November" & prev_year!= "2021"){
    write.csv(find_changed_archetypes, paste0('../../../data/NGS_Data_Tables/PPI/find_changed_archetypes', prev_month, '_to_', current_month, '.csv'))
  }
}

  


# Make pretty with rounded numbers, and add in the potential new names
find_map<-find_map%>% mutate(
  archetype_full_orig = case_when(
    archetype_orig == "Insufficient data" ~ "Insufficient data - Missing key diagnostic or case metrics",
    archetype_orig == "High Income*" ~ "High Income*",
    archetype_orig == "Strengthen" ~ "Strengthen - Build additional NGS capacity for scale-up",
    archetype_orig == "Leverage" ~ "Leverage - Leverage existing NGS capacity",
    archetype_orig == "Connect" ~ "Connect - Connect to countries with NGS capacity or build NGS capacity from scratch"),
  archetype_full_new = case_when(
    archetype_new == "Insufficient data" ~ "Insufficient data - Missing key diagnostic or case metrics",
    archetype_new == "High Income*" ~ "High Income*",
    archetype_new == "Sustain" ~ "Sustain - Sustain diagnostic & sequencing levels",
    archetype_new == "Sequence" ~ "Sequence - Improve sequencing levels"),
  archetype_full_orig_w_HICs = case_when(
    archetype_orig_w_HICs == "Insufficient data" ~ "Insufficient data - Missing key diagnostic or case metrics",
    archetype_orig_w_HICs == "Sustain" ~ "Sustain - Sustain current sequencing levels",
    archetype_orig_w_HICs == "Strengthen/Leverage" ~ "Strengthen/Leverage - Strengthen sequencing levels by leveraging existing NGS capacity",
    archetype_orig_w_HICs == "Connect/Build" ~ "Connect/Build - Connect to countries with NGS capacity or build NGS capacity from scratch"),
  TPR_pct = paste0(round(100*tpr_year_smoothed_truncated, 1), ' %'),
  daily_tests_per_1000 = paste0(round(avg_daily_tests_per_1000_last_year_smoothed,2), ' per 1,000 persons'),
  pct_seq = paste0(round(pct_cases_sequenced_in_last_year,2), ' %'),
  seq_per_100k = paste0(round(sequences_per_100k_last_year,1), ' per 100k persons')
  )


find_map$TPR_pct[find_map$TPR_pct == "NA %"]<- 'Insufficient data'
find_map$TPR_pct[find_map$TPR_pct == "0 %"]<- 'No cases reported'
find_map$daily_tests_per_1000[find_map$daily_tests_per_1000 == "NA per 1,000 persons"]<- 'Insufficient data'
find_map$pct_seq[find_map$pct_seq == "NA %" | find_map$pct_seq == "NaN %"]<-'Insufficient data'
find_map$seq_per_100k[find_map$seq_per_100k == "NA per 100k persons"]<- 'Insufficient data'
find_map$facility_access[is.na(find_map$facility_access)]<-"Insufficient data"

# Add in the cumulative number of sequences
find_map<-left_join(find_map, gisaid_cumulative, by= c("code" = "country_code"))

# Make column headers look nice and add commas
#find_map$cum_seq<-comma_format()(round(find_map$cum_seq, 0))
#find_map$seq_per_100k<-comma_format()find_map$seq_per_100k
find_map<-find_map%>%
    rename(
  Archetype = archetype_full_orig,
  `Archetype*` = archetype_full_orig_w_HICs,
  `Test recommendation` = dx_testing_rec,
  `Test positivity rate (%) in past year` = TPR_pct,
  `Average daily tests in past year` = daily_tests_per_1000,
  `Date tests last reported` = date_tests_last_reported,
  `Days since tests were reported` = days_since_tests_reported,
  `% of cases sequenced in past year` = pct_seq,
  `Number of sequences in past year` = seq_per_100k,
  `Cumulative number of sequences entire pandemic` = cum_seq
)

stopifnot('More than 3 countries missing archetype at final step' = sum(find_map$Archetype == "NaN" |is.na(find_map$Archetype)) <=3)

# Generates a dataset with all the countries that need to improve testing
find_rec_test<-find_map%>%
  filter(dx_testing_capacity == "Does not meet testing target")
  

find_map<-find_map%>%mutate(
  seq_but_no_test_flag = ifelse(
    (dx_testing_capacity == "Does not meet testing target" & 
       sars_cov_2_sequencing == "Meets sequencing target"), 'yes', 'no'
  )
)


# Join shapefiles! 
shapefile <- read_delim(SHAPEFILES_FOR_FLOURISH_PATH, delim = "\t") %>%
  select(geometry, code, country)
lat_long<-read.csv(LAT_LONG_DATA)%>%clean_names()%>%
  select(alpha_3_code, latitude, longitude)%>%
  rename(code = alpha_3_code)%>%
    mutate(code = trim_ws(as.character(code)))



find_map_small<-find_map%>%select(-name) # remove name and replace with country from shapefile
find_clean_flourish<-left_join(shapefile,find_map_small, by = "code")
find_clean_flourish<-left_join(find_clean_flourish, lat_long, by = "code")

find_clean_flourish$`Test positivity rate (%) in past year`[is.na(find_clean_flourish$`Test positivity rate (%) in past year`)]<- 'Insufficient data'
find_clean_flourish$`Average daily tests in past year`[is.na(find_clean_flourish$`Average daily tests in past year`)]<- 'Insufficient data'
find_clean_flourish$`% of cases sequenced in past year`[is.na(find_clean_flourish$`% of cases sequenced in past year`)]<-'Insufficient data'
find_clean_flourish$`Number of sequences in past year`[is.na(find_clean_flourish$`Number of sequences in past year`)]<- 'Insufficient data'
find_clean_flourish$facility_access[is.na(find_clean_flourish$facility_access)]<-"Insufficient data"
find_clean_flourish$sequencing_capacity[is.na(find_clean_flourish$facility_access)]<-"Insufficient data"
find_clean_flourish$dx_testing_capacity[is.na(find_clean_flourish$dx_testing_capacity)]<-"Insufficient testing data"
find_clean_flourish$sars_cov_2_sequencing[is.na(find_clean_flourish$sars_cov_2_sequencing)]<-"Insufficient data"
find_clean_flourish$sequencing_capacity[is.na(find_clean_flourish$sequencing_capacity)]<-"Insufficient data"
find_clean_flourish$archetype_orig_w_HICs[is.na(find_clean_flourish$archetype_orig_w_HICs)]<-"Insufficient data"
find_clean_flourish$`Archetype*`[is.na(find_clean_flourish$`Archetype*`)]<-"Insufficient data - Missing key diagnostic or case metrics"
find_clean_flourish$world_bank_economies[is.na(find_clean_flourish$world_bank_economies)]<- "Insufficient data"
find_clean_flourish$sequencing_capacity[is.na(find_clean_flourish$sequencing_capacity)]<- "Insufficient data"


find_clean_flourish<-find_clean_flourish%>%mutate(
  ngs_capacity_binary = ifelse(
    (ngs_capacity == 0 | is.na(ngs_capacity)), "No", "Yes"),
  test_binary = ifelse(
    `Test recommendation` == "Test - Increase diagnostic testing capacity", 1, NA
  ))

find_rec_test<-left_join(find_rec_test, shapefile, by = "code")

# Remove the extraneous columns from the full_dataset
full_dataset<-find_clean%>%select(name, code, population_size, date_tests_last_reported,
                                  rept_tests_within_last_6_months, days_since_tests_reported,
                                  cases_in_last_year_smoothed_truncated, tests_in_last_year_smoothed,
                                  tpr_year_smoothed_truncated, avg_daily_tests_per_1000_last_year_smoothed,
                                  cases_in_last_year, sequences_in_last_year,
                                  pct_cases_sequenced_in_last_year, sequences_per_100k_last_year,
                                  dx_testing_capacity,ngs_capacity, sequencing_capacity,
                                  sars_cov_2_sequencing, world_bank_economies, archetype_orig_w_HICs, prev_test_rec,
                                  dx_testing_rec, old_archetype)%>%
  rename(archetype = archetype_orig_w_HICs)%>%left_join(gisaid_cumulative, by = c("code" = "country_code"))


# Remove extraneous columns from the map dataset 
find_clean_flourish<-find_clean_flourish%>%select(-old_archetype, -archetype_orig, -`Archetype`, 
                                                  -archetype_full_new, -archetype_new)
# Add a column for TPR that is in % but without the tacked on percent!
find_clean_flourish<-find_clean_flourish%>%mutate(tpr_pct = 100*tpr_year_smoothed_truncated)

# Make clean dataset
clean_dataset<-find_map%>%select(name, `Date tests last reported`, `Test positivity rate (%) in past year`,
                                 `Average daily tests in past year`, `% of cases sequenced in past year`,
                                 `Number of sequences in past year`, `Cumulative number of sequences entire pandemic`,
                                 world_bank_economies,dx_testing_capacity, 
                                 `Test recommendation`,
                                 sars_cov_2_sequencing,facility_access, archetype_orig_w_HICs)%>%
  rename(
    `Country` = name,
    `World Bank economic status` = world_bank_economies,
    `COVID-19 diagnostic testing capacity` = dx_testing_capacity,
    `SARS-CoV-2 sequencing capacity` = sars_cov_2_sequencing,
    `NGS facility access` = facility_access,
    `Archetype` = archetype_orig_w_HICs)




if (USE_CASE == 'local'){
  if(prev_month!= 'November' & prev_year != '2021'){
    write.csv(find_changed_archetypes, paste0('../../../data/NGS_Data_Tables/', current_folder, '/PPI/find_changed_archetypes.csv'), row.names = F)
  }
  write.csv(find_not_reported, paste0('../../../data/NGS_Data_Tables/', current_folder, '/PPI/find_delayed_test_reporting.csv'), row.names = F)
  write.csv(full_dataset, paste0('../../../data/NGS_Data_Tables/', current_folder, '/public/full_dataset.csv'), na = "NaN", row.names = FALSE)
  write.csv(find_clean_flourish, paste0('../../../data/NGS_Data_Tables/', current_folder, '/PPI/find_map.csv'), na = "NaN", row.names = FALSE)
  write.csv(clean_dataset, paste0('../../../data/NGS_Data_Tables/', current_folder, '/public/clean_dataset.csv'), na = "NaN", row.names = FALSE)
  write.csv(find_rec_test, paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/countries_in_test.csv'), na = "NaN", row.names = FALSE )
}

if (USE_CASE == 'domino'){
  write.csv(find_changed_archetypes, '/mnt/data/processed/find_changed_archetypes.csv')
  write.csv(find_not_reported, '/mnt/data/processed/find_delayed_test_reporting.csv')
  write.csv(find_not_reported, '/mnt/data/processed/find_delayed_test_reporting.csv')
  write.csv(full_dataset, "/mnt/data/processed/full_dataset.csv", na = "NaN", row.names = FALSE)
  write.csv(find_clean_flourish, "/mnt/data/processed/find_map.csv", na = "NaN", row.names = FALSE)
  write.csv(clean_dataset, "/mnt/data/processed/clean_dataset.csv", na = "NaN", row.names = FALSE)
  write.csv(find_insufficient_test_but_have_seq, "/mnt/data/processed/test_but_suff_seq.csv", na = "NaN", row.names = FALSE )
}






