# Original Author: Nathan Poland
# Updated: Briana Thrift & Zach Susswein
# Current Author: Kaitlyn Johnson
# Second Author: Briana Thrift


# This script takes in the GISAID metadata, OWID and FIND testing data and computes a number of metrics that will be used
# to define country archetypes to assess surveillance capacity.

# this is a test

# Clears (rm = removes) all objects from the workspace
rm(list = ls())

# Sys.getenv obtains the values of the environment variables
USE_CASE = Sys.getenv("USE_CASE")

# If USE_CASE is empty, then replace space with "local"
if(USE_CASE == ""){
  USE_CASE<-'local'
}



# ---------------------------------------------------------
# -------------------- Install Packages
# ---------------------------------------------------------
# USE_CASE = domino used for automatic update
# Package installations are only needed the first time you use it
# K: True, but in Domino, since it spins up a new "machine" each time, we have to install at the top of every script"
# If USE_CASE = 'Domino' (virtual environment for automatic data updates) then install these libraries
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
install.packages("magrittr", dependencies=TRUE, repos='http://cran.us.r-project.org') 
install.packages("dplyr", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("scales", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("bpa", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("reshape2", dependencies=TRUE, repos='http://cran.us.r-project.org')
}

# ---------------------------------------------------------
# --------------  Load Libraries 
# ---------------------------------------------------------

# Install these libraries regardless of "USE_CASE"
# K: This is not installing the libraries, it's just loading them! 
library(tidyverse) # Data wrangling
library(tibble) # Data wrangling
library(janitor) # Column naming
library(countrycode) # Country codes
library(lubridate) # Date times
library(readxl) # Excel import
library(zoo) # Calculate rolling averages
library(R.utils) # R utilities
library(stringr) # To parse strings in R
library(magrittr) # Needs to be run every time you start R and want to use %>%
library(dplyr) # Data wrangling
library(scales) # Comma formatting
library(bpa) # To get the trim_ws working, which will allow you to join the lat and long files
library(reshape2) # melt function for wrangling


 
# ----------------------------------------------------------
# ---------- Set data files pathnames and set parameters 
# ----------------------------------------------------------

# Pull today's date in, using Eastern Standard Time
# Substr: Extract or replace substrings in a character vector, 
# Starting at 1 character length, and ending at 13 length
today_date<-lubridate::today('EST')

# Pulling in the current month and year to name the folder 
current_month<-month.name[month(today_date)]
current_year<-year(today_date)
current_folder<-str_c(current_month, current_year, sep = '_')


# Creating last date pulled (first day of current month) and previous month variables
LAST_DATA_PULL_DATE<-ymd(str_c(current_year, current_month, "01", sep = '-'))
# Compares to last published archetype definitions
prev_month<-month.name[month(ymd(LAST_DATA_PULL_DATE) - months(1))]
# prev_month of this dataframe <-"March"
prev_year<-year(ymd(LAST_DATA_PULL_DATE) - months(1))
# prev_year of this dataframe <-"2022"
prev_folder<-str_c(prev_month, prev_year, sep = '_')
# First data that we would expect to see SARS-CoV-2 genomes/cases/tests
FIRST_DATE<-"2019-12-01" 
# Timeframe for analysis
TIME_WINDOW_YEAR<-364
# Include for weekly aggregation of cases
TIME_WINDOW_WEEK<- 6


# ------------------------------------
# ------------  Set file-paths 
# Note that sometimes, URLs need to be re-read, if there is back to back running
# -------------------------------------

# Load in all the datasets available on public githubs (FINDS and ours)
ALL_DATA_PATH<- url("https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv")
SHAPEFILES_FOR_FLOURISH_PATH <- url('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/Geospatial_Data/geometric_polygons_country.txt') # shapefiles for mapping+
WHO_REGIONS_PATH<- url('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/WHO_region_data.csv') # WHO country list
ECONOMY_PATH<- url('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/WB_class_data.csv')
FIND_TESTING_SEQ_RAW_PATH<- url('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/Sequencing_labs_data.csv')
LAT_LONG_DATA <- url ('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/Geospatial_Data/geography.csv')
INVESTMENT_DATA <- url('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/FIND_partner_support_maps/FIND_partner_maps_May2022.csv')


# Out-dated FIND NGS map methodologies that were lives/updated in November.
# K: I delted the lines that duplicate this later in the code
if (prev_folder == "November_2021"){
  OLD_FIND_MAP_PATH<-url(paste0 ("https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/", prev_folder, "/PPI/find_map_11.30.2021.csv"))
}
if(prev_folder != "November_2021")
  {
  OLD_FIND_MAP_PATH<-url(paste0("https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/", prev_folder, "/PPI/find_map.csv"))
}


# When running in domino, run the following pathways to ingest this data from Domino folders:
if (USE_CASE == 'domino'){
  GISAID_DAILY_PATH<-'/mnt/data/processed/gisaid_owid_merged.csv' # output from gisaid_metadata_processing.R
}

# When running locally, run the following local pathways to ingest data from local folders:
if (USE_CASE == 'local'){
  GISAID_DAILY_PATH<-'../../../data/processed/gisaid_owid_merged.csv' # output from gisaid_metadata_processing.R
}

# -----------------------------------------------------------------------
# --------------------------  WHO countries, regions, and code 
# -----------------------------------------------------------------------

# Import csv of WHO region countries 
WHO_regions <- read.csv(WHO_REGIONS_PATH) %>%
  # Standardize names with this janitor function
  clean_names() %>%
  # Rename column from name to "country_name"
  rename(name = country_name)


# -------------------------------------------------------------------------------------------
# ------------------------------ WHO NGS facility data to construct Access metric 
# -------------------------------------------------------------------------------------------

# Reading in all countries and sub-setting to countries with valid names
test_seq_raw <- read.csv (FIND_TESTING_SEQ_RAW_PATH) %>%
  #clean names are those that do not have missing country codes
  clean_names() 

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
    ngs_capacity ==0 ~ "Does not have access to sequencing facilities",
    (ngs_capacity ==1 | ngs_capacity == 2) ~ "Has access to sequencing facilities"
  
))


# Select only necessary variables, and clean metric string values
ngs_clean <- ngs_clean %>%
  select(contains("code"),  ngs_capacity, facility_access)%>%
    mutate(
        # Just recoding what is in raw data values, where 0 = 0,
        # 1 = 1-3 labs, and 2 = 4+ labs
        ngs_facility = case_when(
            ngs_capacity == 0 ~ "0 NGS facilities or equivalent",
            ngs_capacity == 1 ~ "1-3 NGS facilities or equivalent",
            ngs_capacity == 2 ~ "4+ NGS facilities or equivalent",
            is.na(ngs_capacity) == T ~ "0 NGS facilities or equivalent"
        ))
    

# find_clean: merge sequencing capacity data into template
# All additional variables will be joined to this find_clean template
find_clean <- left_join(WHO_regions, ngs_clean, by = c("code" = "country_code"))


# -------------------------------------------------------------------------------------
# ------------------------------ FIND Test Tracker data: TESTING metric
# ------------------------------------------------------------------------------------
 
# Import csv from FIND containing country, date, population size, tests, 
find_raw <- read.csv(ALL_DATA_PATH) %>% clean_names()
 
# Select and rename necessary columns
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
date <- seq.Date(first_date, LAST_DATA_PULL_DATE, by = "day")
code <-unique(find_testing_t$code)
date_country<-expand_grid(date, code)

# Join the find_test_t dataframe with filled dates df
find_testing_t<-left_join(date_country,find_testing_t, by = c("code", "date"))


# ----------- Validation Testing ---------------------------------

# For each country, find the date they last reported raw new tests
find_test_update_date<-find_testing_t%>%
  group_by(code) %>% select(code, date, new_tests_orig)%>% #Is new_test_orig an existing column name? lost here
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
# ----------- Data Processing ------------------------------

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
            tpr_year_smoothed_truncated = round(cases_in_last_year_smoothed_truncated/tests_in_last_year_smoothed,4), # used for archetype definition
            #convert TPR to a percentage:
            tpr = round(100*tpr_year_smoothed_truncated,2),
            # Max(pop) used for deduplication
            avg_daily_test_per_1000_last_year_raw = 1000*mean(new_tests_orig/max(pop, na.rm = T), na.rm = TRUE),
            avg_daily_tests_per_1000_last_year_smoothed = round(1000*mean(new_tests_smoothed/max(pop, na.rm = T), na.rm = TRUE),2), # used for archetype definition
            population_size = max(pop)# pops should all be the same
            )%>%  # rename long columnn name to average daily test
  filter(!is.na(code))


# ----------- Validation Testing ---------------------------------

# Find the countries where they haven't reported tests in the past year
no_avg_tpr<-find_testing_last_year%>%filter(is.na(avg_tpr_find))
no_avg_daily_tests<-find_testing_last_year%>%filter(is.na(avg_daily_tests_per_1000_last_year_smoothed))

# Join last year with data on test updating
find_testing_clean<-left_join(find_test_update_date, find_testing_last_year, by = "code")
# Number of countries that haven't reported tests in past 6 months but have in past year 
n_not_rept_6_mos= nrow(find_testing_clean%>%filter(!is.na(avg_tpr_find) & rept_tests_within_last_6_months == FALSE))
find_not_reported<-find_testing_clean%>%filter(rept_tests_within_last_6_months==FALSE & !is.na(avg_tpr_find))

# Unit tests
stopifnot('Countries reporting greater than a test per person per day' = sum(find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed>1000,na.rm = T)== 0)
stopifnot('More than 25 countries with data havent reported tests in 6 months' = n_not_rept_6_mos<=25)
stopifnot('More than 35 countries are missing average daily TPR from FIND' = nrow(no_avg_tpr)<=35)

# Remove any negative daily tests or impossible TPRs
find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed<- ifelse(find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed < 0, NA, 
                                                                   find_testing_clean$avg_daily_tests_per_1000_last_year_smoothed)
find_testing_clean$tpr_year_smoothed_truncated <- ifelse(find_testing_clean$tpr_year_smoothed_truncated < 0 | find_testing_clean$tpr_year_smoothed_truncated > 1, NA, 
                                               find_testing_clean$tpr_year_smoothed_truncated)


# ----------- Data Processing ------------------------------

# Join to find_clean (template)
find_clean<-left_join(find_clean, find_testing_clean, by = "code")


#-------------- Construct Dx Testing Targets ------------------------------

# Create Dx Testing Capacity Classifier variable, labeled as binary of Does or Does NOT meet testing target
# based on the truncated TPR and average daily tests per 1000 last year using smoothed cases and test data
# If countries have not reported in 6 months or are missing TPR reported to FIND, get labeled as having insufficient data to 

#0.5 average daily tests target
daily_tests_thres <- 0.5

#20% TPR target
TPR_thres<- 20 

#ACT-A-Accelerator targets by WHO
ACT_A_target<-1 

# Create new variable for Dx testing capacity based on:
# TPR over the past year > 20%
# AND average daily tests per 1000 > 0.15 (PPI/FIND targets)
find_clean <- find_clean %>%
  mutate(
    dx_testing_binary = case_when(
      (is.na(avg_tpr_find)  | rept_tests_within_last_6_months ==FALSE | is.infinite(avg_tpr_find)) ~ "Insufficient testing data",
      tpr >= TPR_thres & avg_daily_tests_per_1000_last_year_smoothed <= daily_tests_thres ~ "Does not meet testing target", #bottom right
      tpr >= TPR_thres & avg_daily_tests_per_1000_last_year_smoothed > daily_tests_thres ~ "Does not meet testing target", # upper right
      tpr < TPR_thres & avg_daily_tests_per_1000_last_year_smoothed <= daily_tests_thres ~ "Does not meet testing target", # bottom left
      tpr < TPR_thres & avg_daily_tests_per_1000_last_year_smoothed > daily_tests_thres ~ "Meets testing target" # upper left
    ),
    
    dx_testing_rec = case_when(
      dx_testing_binary == "Insufficient testing data" ~ "Insufficient testing data",
      dx_testing_binary == "Does not meet testing target" ~ "Test - Increase diagnostic testing capacity",
      dx_testing_binary == "Meets testing target" ~ "Sustain - Sustain diagnostic testing capacity"
    ),
    
    dx_archetype = case_when(
      dx_testing_binary == "Insufficient testing data" ~ "Insufficient data",
      dx_testing_binary == "Does not meet testing target" ~ "Test",
      dx_testing_binary == "Meets testing target" ~ "Sustain"
    )
  )
 

# ------------------------------------------------------------------------
# --------------- GISAID SARS-CoV-2 data: SEQUENCING metric 
# ------------------------------------------------------------------------

# This is where we read in the gisaid_metadata_processing script, pulled in another R script
# to process (filter & clean) raw GISAID data, downloaded from GISAID site
# raw refers to all variants, by country, by day
gisaid_raw <- read.csv(GISAID_DAILY_PATH) %>% 
  clean_names()

# Replaces with collection date and parse as date
gisaid_raw$collection_date <- as.Date(as.character(gisaid_raw$gisaid_collect_date), format = "%Y-%m-%d")

# Select important columns: collection date, country name, number of new sequences, OWID
# covid-19 case data, OWID population data, country code, OWID location
gisaid_t <- gisaid_raw%>%
  select(collection_date, gisaid_country, n_new_sequences,
         owid_new_cases, owid_population, country_code, owid_location)

# ----------- Validation Testing ---------------------------------

# Unit test: Make sure that the most recent date is yesterday (only relevant if we are updating)
if (ymd(max(gisaid_t$collection_date)) != ymd(LAST_DATA_PULL_DATE)-days(1)){
  warning("GISAID metadata not updated")
}

# ----------- Data Processing  ---------------------------------

# Subset GISAID data to the last  year, and aggregate by country
gisaid_last_year<-gisaid_t%>%filter(collection_date>=(LAST_DATA_PULL_DATE - TIME_WINDOW_YEAR) & 
                                      collection_date<= LAST_DATA_PULL_DATE)%>%
  group_by(country_code)%>%
  summarise(cases_in_last_year = sum(owid_new_cases), # total cases in past year from OWID
            cases_per_100k_last_year = round(100000 *cases_in_last_year/max(owid_population),3), # total cases per capita
            sequences_in_last_year= sum(n_new_sequences), # total sequences collected and submitted to GISAID in past year 
            pct_cases_sequenced_in_last_year = round(100*(sequences_in_last_year/cases_in_last_year),2), # pct cases sequenced
            sequences_per_100k_last_year = round(100000*sequences_in_last_year/max(owid_population),3) # total sequences per capita
            )

gisaid_last_year$pct_cases_sequenced_in_last_year[is.infinite(gisaid_last_year$pct_cases_sequenced_in_last_year)]<-NA




# Add a metric of cumulative sequences (to the end)
#does this add into the gisaid_last_year df? Or is this added later in the script?
gisaid_cumulative<-gisaid_t%>%group_by(country_code)%>%
  summarise(cum_seq = sum(n_new_sequences), #cumulative number of sequences collected and submitted over the entire pandemic
            cum_cases = sum(owid_new_cases), #cumulative number of cases over the entire pandemic
            cum_cases_per_100k = round(100000 *cum_cases/max(owid_population),3) 
            )



# Join both sets of metrics 
find_clean <- left_join(find_clean, gisaid_last_year, by = c("code" = "country_code")) 
find_clean <- left_join(find_clean, gisaid_cumulative, by = c("code" = "country_code"))

# -------- SARS-CoV-2 sequencing Targets -----------------------

# Variable assessing if country has met sequencing surveillance target via GISAID sequences

# 0.5% of cases sequenced in the last year
pct_seq_thres<-0.5

# >=10 sequences per 100K persons collected/submitted in the last year
seq_per_cap_thres<- 10 

# Create new variable for SARS-CoV-2 Sequencing based on:
# % of cases sequenced in the past year > 0.5% 
# AND cases sequenced per 100k in the past year >= 10
find_clean <- find_clean %>%
  mutate(
    sars_cov_2_binary = case_when(
      ((cases_in_last_year == 0 | is.na(cases_in_last_year)) & (sequences_per_100k_last_year< seq_per_cap_thres | is.na(sequences_per_100k_last_year))) ~ "Insufficient data",
      ((cases_in_last_year == 0 | is.na(cases_in_last_year)) & sequences_per_100k_last_year> seq_per_cap_thres) ~ "Insufficient data", 
      is.na(sequences_per_100k_last_year) ~ "Does not meet sequencing target",
      pct_cases_sequenced_in_last_year >= pct_seq_thres & sequences_per_100k_last_year >= seq_per_cap_thres  ~ "Meets sequencing target", # upper right
      pct_cases_sequenced_in_last_year < pct_seq_thres & sequences_per_100k_last_year >= seq_per_cap_thres ~ "Does not meet sequencing target", #upper left
      pct_cases_sequenced_in_last_year < pct_seq_thres & sequences_per_100k_last_year < seq_per_cap_thres ~ "Does not meet sequencing target", # bottom left
      pct_cases_sequenced_in_last_year >= pct_seq_thres & sequences_per_100k_last_year < seq_per_cap_thres ~ "Does not meet sequencing target", # bottom right
      
    )
  )


# ---------------------------------------------------------------
# ------------------------------ Add in old archetypes 
# ---------------------------------------------------------------


# Special case if old dataset is Novemver 2021
if (prev_folder == "November_2021")
{
  old_find <-read.csv(OLD_FIND_MAP_PATH)%>%
    select(code, country, dx_testing_capacity_clean, sars_cov_2_capacity, archetype)%>%
    rename(prev_dx = dx_testing_capacity_clean, # dx_testing_capacity
           prev_sequ = archetype, # sx_archetype including HICs
           old_sequencing_archetype = sars_cov_2_capacity)%>%
           filter(!is.na(code))%>%
           filter(country != "West Bank and Gaza") # 237 countries, filter out Palastine 2 country  names
}

# Minor changes from March, so account for them
if (prev_folder == "March_2022"){
  old_find <-read.csv(OLD_FIND_MAP_PATH) %>% clean_names() %>%
    select(code, country, ngs_capacity, dx_testing_capacity, sars_cov_2_sequencing)%>%
      mutate(dx_archetype = case_when(
          dx_testing_capacity == "Meets testing target" ~ "Sustain",
          dx_testing_capacity == "Does not meet testing target" ~ "Test",
          dx_testing_capacity == "Insufficient testing data" ~ "Insufficient data"
      ),
      sx_archetype = case_when(  
          (sars_cov_2_sequencing == "Insufficient data") ~ "Insufficient data",
          sars_cov_2_sequencing == "Meets sequencing target" ~ "Sustain",
          (sars_cov_2_sequencing == "Does not meet sequencing target" &
               (ngs_capacity == 2 | ngs_capacity == 1)) ~ "Strengthen/Leverage",
          sars_cov_2_sequencing == "Does not meet sequencing target" & (ngs_capacity == 0 |is.na(ngs_capacity)) ~ "Connect/Build")
      )%>%
      rename(sars_cov_2_binary = sars_cov_2_sequencing)
}

if (prev_folder != "March_2022" & prev_folder != "November_2021"){
    old_find <-read.csv(OLD_FIND_MAP_PATH) %>% clean_names() %>%
        select(code, country, ngs_capacity, dx_archetype, sx_archetype, sars_cov_2_binary)
}

old_find_df <- old_find %>% 
    # When renaming, first variable is the NEW name, second is the OLD name (opposite of python rename)
    rename(prev_dx = dx_archetype,
           prev_sequ = sx_archetype, # sx_archetype including HICs
           old_sequencing_archetype = sars_cov_2_binary)

old_find_df <- old_find_df %>%
  select (code, old_sequencing_archetype, prev_sequ, prev_dx)

# Subset all unique code values from this old FIND NGS data into "old_codes" dataframe
old_codes<-unique(old_find_df$code) # 237 or 238 of them

# Filter to only codes in old data
find_clean<-find_clean%>%
  filter(code %in% old_codes)

# Filer out Palestine double country  name
find_clean<-left_join(find_clean, old_find_df, by = "code")
find_clean<-find_clean%>%filter(name != "West Bank and Gaza")%>% 
  filter(code != "UMI") # 237 countries #take out UMI country

# Assess number of unique codes in dataframe
n_codes <- length(unique(find_clean$code)) 



# ----------------------------------------------------------------------
# ------------------------------- World Bank Economy SES groups 
# ----------------------------------------------------------------------

world_bank_background_raw <- read_csv(ECONOMY_PATH) %>%
  # Standardize names with this janitor function
  clean_names()

# Remove buffer rows by iso3 code and select code and testing capacity columns
world_bank_background_clean <- world_bank_background_raw %>%
  # Drop all columns except iso3 code and world_bank_economies
  select('code','income_group')%>%
  filter(code != 'x') 

# Rename columns code and world_bank_economies
colnames(world_bank_background_clean) <- c("code", "world_bank_economies")

#replace missing values with 'No income data' in world_bank_economies column
world_bank_background_clean$world_bank_economies <- world_bank_background_clean$world_bank_economies %>% replace_na('No income data')

# find_clean: merge WHO testing data into template
find_clean <- left_join(find_clean, world_bank_background_clean, by = c("code" = "code"))

find_clean<-find_clean%>%
  mutate( LMIC_status = case_when(
  world_bank_economies == 'High income' ~ 'High Income',
  world_bank_economies != 'High income' ~ 'LMIC',
  is.na(world_bank_economies) == T ~ "No income data")
  )

#replace missing values with 'No income data' in world_bank_economies column
find_clean$world_bank_economies <- find_clean$world_bank_economies %>% replace_na('No income data')


# -------------------- Validation test---------------------------------
# Unit tests
stopifnot('Incorrect number of countries'= n_codes<=237 | n_codes>=236)
find_clean_LMICs<-find_clean%>%
  filter(LMIC_status != 'High Income')

stopifnot('Less than 90 LMICs with avg daily TPR not NA & with tests reported in last 6 months' = 
            nrow(find_clean_LMICs%>%
                   filter(!is.na(avg_tpr_find))%>%filter(rept_tests_within_last_6_months == TRUE)) >= 90)




# ----------------------------------------------------------------------
# -------------------- Create Archetypes and Classify
# ----------------------------------------------------------------------

# Archetype_orig indicates the original archetype names (Strengthen, Connect, Leverage, Test but with the new definitions)
# New archetype names: Sustain, Leverage/Strengthen, Connect/Build
find_clean<-find_clean%>%
  mutate(
  # archetype_orig excludes HICs and classifies based on Original Methodologies language
  archetype_orig = case_when(
    prev_sequ == "High Income*" ~ "High Income*", 
   (sars_cov_2_binary == "Insufficient data") ~ "Insufficient data",
    sars_cov_2_binary == "Meets sequencing target" ~ "Strengthen",
    (sars_cov_2_binary == "Does not meet sequencing target" &
    (ngs_capacity == 2 | ngs_capacity == 1)) ~ "Leverage",
     sars_cov_2_binary == "Does not meet sequencing target" & (ngs_capacity == 0 |is.na(ngs_capacity))~ "Connect"),
  
  # Classifies HICs and classifies based on New Methodologies language
  sx_archetype = case_when(  
    (sars_cov_2_binary == "Insufficient data") ~ "Insufficient data",
    sars_cov_2_binary == "Meets sequencing target" ~ "Sustain",
    (sars_cov_2_binary == "Does not meet sequencing target" &
       (ngs_capacity == 2 | ngs_capacity == 1)) ~ "Strengthen/Leverage",
    sars_cov_2_binary == "Does not meet sequencing target" & (ngs_capacity == 0 |is.na(ngs_capacity)) ~ "Connect/Build")
  )

# ---------------------------------------------------------------------
# ----------------- Data edits
# ---------------------------------------------------------------------

# Edit the date variable so that it is in a universally readable format
date_df<-find_clean%>%
  separate(date_tests_last_reported,
                            into = c("year", "month", "day"),
                            sep = "-")
date_df$month<-month.name[as.numeric(date_df$month)]
date_df$date_tests_last_reported<-paste0(date_df$month, ' ', date_df$day, ', ', date_df$year)

# Replacing NaNs with string "No tests reported"
date_df$date_tests_last_reported[date_df$date_tests_last_reported == "NA NA, NA"]<-"No tests reported"
find_clean$date_tests_last_reported<-date_df$date_tests_last_reported

# Make pretty with rounded numbers, and add in the potential new names
find_clean<-find_clean%>% mutate(
  
  sx_sequencing_rec = case_when(
    sx_archetype == "Insufficient data" ~ "Insufficient data - Missing key diagnostic or case metrics",
    sx_archetype == "Sustain" ~ "Sustain - Sustain current sequencing levels",
    sx_archetype == "Strengthen/Leverage" ~ "Strengthen / Leverage - Strengthen sequencing levels by leveraging existing NGS capacity",
    sx_archetype == "Connect/Build" ~ "Connect / Build - Connect to countries with NGS capacity or build NGS capacity from scratch"),
  
  TPR_pct = paste0(round(100*tpr_year_smoothed_truncated, 1), ' %'),
  daily_tests_per_1000 = paste0(round(avg_daily_tests_per_1000_last_year_smoothed,2), ' per 1,000 persons'),
  pct_seq = paste0(round(pct_cases_sequenced_in_last_year,2), ' %'),
  seq_per_100k = paste0(round(sequences_per_100k_last_year,1), ' per 100k persons')
  )


find_clean$TPR_pct[find_clean$TPR_pct == "NA %"]<- 'Insufficient data'
find_clean$TPR_pct[find_clean$TPR_pct == "0 %"]<- 'No cases reported'
find_clean$daily_tests_per_1000[find_clean$daily_tests_per_1000 == "NA per 1,000 persons"]<- 'Insufficient data'
find_clean$pct_seq[find_clean$pct_seq == "NA %" | find_clean$pct_seq == "NaN %"]<-'Insufficient data'
find_clean$seq_per_100k[find_clean$seq_per_100k == "NA per 100k persons"]<- 'Insufficient data'
find_clean$facility_access[is.na(find_clean$facility_access)]<-"Insufficient data"

# Select necessary variables for Flourish map only (find_clean -> find_map df)
find_map<- find_clean %>%select(name, code, population_size, world_bank_economies,
                                
                                #Cases & Reporting 
                                cum_seq,
                                date_tests_last_reported, days_since_tests_reported, 
                                #Testing
                                tpr_year_smoothed_truncated, TPR_pct,
                                avg_daily_tests_per_1000_last_year_smoothed, daily_tests_per_1000, 
                                #Sequencing
                                cum_seq, pct_cases_sequenced_in_last_year, pct_seq, 
                                sequences_per_100k_last_year, seq_per_100k,
                                #Targets
                                dx_testing_binary, sars_cov_2_binary, 
                                ngs_capacity, facility_access, ngs_facility,
                                archetype_orig, dx_archetype, sx_archetype,
                                sx_sequencing_rec, dx_testing_rec,
                                prev_dx, prev_sequ)


# Make column headers look nice for a separate dataset
find_map_clean_titles <-find_map%>%
    rename(
  `Sequencing Archetype` = sx_sequencing_rec,
  `Test Archetype` = dx_testing_rec,
  `Test positivity rate (%)` = TPR_pct,
  `Average daily tests` = daily_tests_per_1000,
  `Date tests last reported` = date_tests_last_reported,
  `Days since tests were reported` = days_since_tests_reported,
  `% of cases sequenced` = pct_seq,
  `Number of sequences` = seq_per_100k,
  `Cumulative number of sequences entire pandemic` = cum_seq)

# -------------------- Validation test
# Unit test
stopifnot('More than 3 countries missing archetype at final step' = sum(find_map$Archetype == "NaN" |is.na(find_map$Archetype)) <=3)



# ---------------------------------------------------------------------------
# ------------  Assessing Difference in Archetypes post changes 
# ---------------------------------------------------------------------------


# Find the countries with new archetypes
# !!! Need to run after we have prev_month post "March"!!!
# if (prev_month != "November" & prev_year != "2021" || prev_month != "March" & prev_year != "2022")
#  {
# find_changed_archetypes <-find_clean%>%filter
# (prev_sequ != sx_archetype, prev_dx != dx_archetype)%>%
#  select(!archetype_orig)
# }
  
# Find the countries with new archetypes
 if (prev_month != "November" & prev_year != "2021")
  {
 find_changed_archetypes <-find_map%>%
   filter (prev_sequ != sx_archetype, prev_dx != dx_archetype)
}

 find_changed_archetypes<-find_changed_archetypes%>%
   select(name, world_bank_economies,
          prev_sequ, sx_archetype, prev_dx, dx_archetype,
          tpr_year_smoothed_truncated, avg_daily_tests_per_1000_last_year_smoothed,
          pct_cases_sequenced_in_last_year,
          sequences_per_100k_last_year,
          ngs_capacity, ngs_facility)


# ---------------------------------------------------------------------------
# ------------  Internal Troubleshooting/QA datasets 
# ---------------------------------------------------------------------------


# Internal troubleshooting, generates dataset that groups by archetype for easy validation
find_clean_LMICs<-find_clean%>%filter(LMIC_status != 'High Income')
n_insufficient_data<- sum(find_map$sx_archetype == "Insufficient data")

n_not_tests<-sum(find_clean_LMICs$dx_testing_binary == "Meets testing target")
n_Test<- sum(find_clean_LMICs$dx_testing_binary == "Does not meet testing target")
n_Sustain <-sum(find_clean_LMICs$sx_archetype == "Sustain")
n_Leverage <- sum(find_clean_LMICs$sx_archetype == "Strengthen/Leverage")
n_Connect <- sum(find_clean_LMICs$sx_archetype == "Connect/Build")

#Number of LMICs with given archetypes, per group
LMICs_n_given_archetypes =  n_Sustain + n_Leverage + n_Connect

# unit test
stopifnot("Number given archetypes other than insufficient data is less than 90 (should be around 95)"= LMICs_n_given_archetypes>=90)



# ------------------------------------------------------------------------------
# ----------------------- Join shapefiles & lat/long coordinates
# ------------------------------------------------------------------------------

shapefile <- read_delim(SHAPEFILES_FOR_FLOURISH_PATH, delim = "\t", show_col_types = FALSE) %>%
  select(geometry, code, country)

# lat_long<-read.csv(LAT_LONG_DATA)%>%clean_names()%>%
#   select(alpha_3_code, latitude, longitude)%>%
#   rename(code = alpha_3_code)%>%
#   mutate(code = trim_ws(as.character(code)))

# new centroids from Google
centroids<-read.csv(LAT_LONG_DATA) %>% 
  filter(str_length(location_key)==2) %>% 
  mutate(code = countrycode(location_key, origin = 'iso2c', destination = 'iso3c')) %>% 
  select(code, latitude, longitude)

# Join dataset with lat/long coordinates to visualize dots on countries
# that do not meet testing targets
find_rec_test<-find_map%>%
  filter(dx_testing_binary == "Does not meet testing target")

# Join for lat/long coordinates
find_TEST_countries <-left_join(find_rec_test, centroids, by = "code") %>% 
  mutate(col_for_points =1)

# Join Shapefiles dataframe to find_map template
find_map<-left_join (shapefile, find_map, by = "code") 
# need to join to the shape file so that every geometry gets assigned its country's data


#-------------------------------------------------------------------------
#------------- Creating new dfs for visuals and public
#-------------------------------------------------------------------------


# Create new variable in find_map that has a flag of not meeting testing
find_map<-find_map%>%mutate(
  seq_but_no_test_flag = ifelse(
    (dx_testing_binary == "Does not meet testing target" & 
       sars_cov_2_binary == "Meets sequencing target"), 'yes', 'no'
  )
)

SEQ_but_TEST <-find_map%>%
  filter(seq_but_no_test_flag == "yes")

#Create a dataset for Sequencing scatterplot visuals
seq_scatterplot<-find_map%>% select(
  country, code, population_size,world_bank_economies,
  pct_cases_sequenced_in_last_year,
  sequences_per_100k_last_year, sars_cov_2_binary)%>%
  filter(sars_cov_2_binary != "Insufficient data", !is.na(sars_cov_2_binary))
# Create a dataset with those without income data removed
seq_scatterplot_income_rmvd<- seq_scatterplot %>%
    filter(world_bank_economies != "No income data")

#Create a dataset for Testing scatterplot visuals
test_scatterplot<-find_map%>%mutate(
  TPR_pct = 100*tpr_year_smoothed_truncated)%>% select(
    country, code, population_size,world_bank_economies,
    date_tests_last_reported, TPR_pct,
    avg_daily_tests_per_1000_last_year_smoothed, dx_testing_binary)%>%
  filter(dx_testing_binary != "Insufficient testing data", !is.na(dx_testing_binary))
# Create a dataset with those without income data removed
test_scatterplot_income_rmvd<-test_scatterplot %>%
    filter(world_bank_economies != "No income data")

# 




#-------------------------------------------------------------------------
#------------- Cleaning dfs to be public
#-------------------------------------------------------------------------

#Clean up dataframe and order variables
find_map <- find_map %>%select(-name)

# Remove extraneous columns from find_map.csv to create a "full_dataset" for public consumption
full_dataset<-find_map%>%select(
                        #Descriptives
                         country, code, population_size, world_bank_economies,
                        #Cases & Reporting 
                          date_tests_last_reported, days_since_tests_reported, 
                        #Testing
                          tpr_year_smoothed_truncated, avg_daily_tests_per_1000_last_year_smoothed,
                        #Sequencing
                          cum_seq, pct_cases_sequenced_in_last_year, 
                          sequences_per_100k_last_year, 
                        #Targets
                          dx_testing_binary, sars_cov_2_binary, 
                          ngs_capacity, facility_access, ngs_facility,
                          archetype_orig, dx_archetype, sx_archetype,
                          dx_testing_rec, sx_sequencing_rec,
                          prev_dx, prev_sequ,
                        #Flags
                          seq_but_no_test_flag, 
                        #Metrics explained
                          TPR_pct, daily_tests_per_1000, pct_seq,seq_per_100k) %>%
    filter(code %in% old_codes) %>%
    unique()# Need to deduplicate because geometries add duplicates


# Create a dataset with clean titles
clean_dataset<-find_map_clean_titles%>%filter(code %in% old_codes) %>%
    select(name, world_bank_economies,
           `Date tests last reported`, 
           `Test positivity rate (%)`,
           `Average daily tests`, 
           `% of cases sequenced`,
           `Number of sequences`, 
           `Cumulative number of sequences entire pandemic`,
           `Test Archetype`,`Sequencing Archetype`)%>%
rename(
  Country = name,
    `Socioeconomic status` = world_bank_economies) %>% 
    unique()
# if a country has NA for sequences, assume 0 sequences submitted in GISAID (and weren't in GISAID dataset)
clean_dataset$`Cumulative number of sequences entire pandemic`[is.na(clean_dataset$`Cumulative number of sequences entire pandemic`)]<-0




# K:We still need these all I think because of the geometries (for anything that is in the map)
find_map$TPR_pct[is.na(find_map$TPR_pct)]<- 'Insufficient data'
find_map$date_tests_last_reported[is.na(find_map$date_tests_last_reported)]<-'Insufficient data'
find_map$daily_tests_per_1000[is.na(find_map$daily_tests_per_1000)]<- 'Insufficient data'
find_map$pct_cases_sequenced_in_last_year[is.na(find_map$pct_cases_sequenced_in_last_year)]<-'Insufficient data'
find_map$sequences_per_100k_last_year[is.na(find_map$sequences_per_100k_last_year)]<- 'Insufficient data'
find_map$facility_access[is.na(find_map$facility_access)]<-"Insufficient data"
find_map$ngs_facility[is.na(find_map$ngs_facility)]<-"Insufficient data"
find_map$dx_testing_binary[is.na(find_map$dx_testing_binary)]<-"Insufficient testing data"
find_map$dx_archetype[is.na(find_map$dx_archetype)]<-"Insufficient testing data"
find_map$dx_testing_rec[is.na(find_map$dx_testing_rec)]<-"Insufficient testing data"
find_map$sars_cov_2_binary[is.na(find_map$sars_cov_2_binary)]<-"Insufficient data"
find_map$sx_archetype[is.na(find_map$sx_archetype)]<-"Insufficient data"
find_map$sx_sequencing_rec[is.na(find_map$sx_sequencing_rec)]<-"Insufficient data"
find_map$world_bank_economies[is.na(find_map$world_bank_economies)]<- "Insufficient data"
find_map$pct_seq[is.na(find_map$pct_seq)]<- "Insufficient data"
find_map$seq_per_100k[is.na(find_map$seq_per_100k)]<-"Insufficient data"
find_map$cum_seq[is.na(find_map$cum_seq)]<-"Insufficient data"


# Add a column for binary Yes or No of whether the country has ngs facilities
find_map<-find_map%>%mutate(
  ngs_capacity_binary = ifelse(
    (ngs_capacity == 0 | is.na(ngs_capacity)), "No", "Yes"))


# Combine with investment data
investment_df_raw <-read.csv(INVESTMENT_DATA) 
investment_df_full<- investment_df_raw%>% 
  select( -X, -sx_archetype,-dx_archetype, -pct_seq, -sequences_per_100k_last_year,
          -TPR_pct, - avg_daily_tests_per_1000_last_year_smoothed, - code_y) %>% 
  rename(code = code_x) %>% 
  left_join(full_dataset %>% 
              select(code, sx_archetype), by = "code") %>% 
  distinct() 
investment_df <-investment_df %>% filter(N_projects >0)



# -----------------------------------------------------------------
# -----------------------    NGS NARRATIVE: SES Summary Stats 
# -----------------------------------------------------------------

# Running descriptive stats for narrative
# Percent (%) of global population, cases, testing by SES, in the past 12 months

# Join together these data-frames, and filer for this past 12 months
globe_aggregated <- left_join (gisaid_last_year, find_testing_clean, by = c ("country_code" = "code"))%>%
    select(-population_size)
# Need the population for all countries, not just those in the find data
all_pops<-full_dataset%>%select(code, population_size)
globe_aggregated<- left_join(all_pops,globe_aggregated, by = c("code" = "country_code"))
# want all 238 countries that we have population size data for 
global_pop_size = sum(globe_aggregated$population_size, na.rm = T)
print(paste0('Global population size in our dataset ', global_pop_size/1e9, ' billion, Google says 7.9 billion'))

# find_clean: merge WB economy data into template
globe_aggregated <- left_join(globe_aggregated, world_bank_background_clean, by = c("code"))
#replace missing values with 'No income data' in world_bank_economies column
globe_aggregated$world_bank_economies <- globe_aggregated$world_bank_economies %>% replace_na('No income data')

# Compute global stats
Global_stats <-globe_aggregated%>%
  summarise(global_population = sum(population_size, na.rm = TRUE),
            global_cases = sum(cases_in_last_year_smoothed, na.rm = TRUE),
            global_tests = sum(tests_in_last_year_smoothed, na.rm = TRUE),
            global_sequences = sum(sequences_in_last_year, na.rm = TRUE)
  )

# Compute SES stats
SES_stats <-globe_aggregated %>%
  group_by(world_bank_economies)%>%
  summarise(SES_population = sum(population_size, na.rm = TRUE),
            SES_cases = sum(cases_in_last_year_smoothed, na.rm = TRUE),
            SES_tests = sum(tests_in_last_year_smoothed, na.rm = TRUE),
            SES_sequences = sum(sequences_in_last_year, na.rm = TRUE)
  )

# Join global and SES dataframes (global stats will duplicate per SES group)
global_SES<-cbind(SES_stats, Global_stats)

# Compute SES %
SES_breakdown <- global_SES %>%
  group_by (world_bank_economies)%>%
  summarize ( SES_pop_rate = round(100* (SES_population/global_population),1),
              SES_cases_rate = round(100* (SES_cases/global_cases),1),
              SES_test_rate = round(100* (SES_tests/global_tests),1),
              SES_sequ_rate = round(100*(SES_sequences/global_sequences),1))

# check that everything adds to 100!
SES_check<-SES_breakdown%>%summarise(
  pop_sum = sum(SES_pop_rate),
  case_sum = sum(SES_cases_rate),
  test_sum = sum(SES_test_rate),
  sequ_sum = sum(SES_sequ_rate)
)
# Unit test to make sure that all of the percents add to 100!
stopifnot('Percents dont add to 100' = SES_check[1,] == c(100,100,100,100))
              
              
SES_breakdown<- SES_breakdown%>%
    rename(
        `% of global population` = SES_pop_rate,
        `% of global cases in the past year` = SES_cases_rate,
        `% of global tests in the past year` = SES_test_rate,
        `% of sequences collected & submitted in the past year` = SES_sequ_rate
    )

print (SES_breakdown)
# Format in the weird way FLourish needs it
SES_flipped<-cbind(ID = rownames(SES_breakdown), SES_breakdown)
SES_flourish<-SES_flipped %>% melt()%>%group_by(world_bank_economies)%>%
    pivot_wider(id_cols = variable, names_from = world_bank_economies, values_from = value)%>%
    select(-`No income data`)%>%
    relocate(variable, `High income`, `Upper middle income`, `Lower middle income`, `Low income`)




# Compute LMIC vs. HICs
LMIC_binary <- globe_aggregated %>%
  mutate(
    LMIC_group = case_when(
      world_bank_economies != "High income" ~ "LMICs",
      world_bank_economies == "High income" ~ "HICs")
  )

# Compute LMIC aggregated stats for narrative
LMIC_stats <-LMIC_binary %>%
  group_by(LMIC_group)%>%
  summarise(LMIC_population = sum(population_size, na.rm = TRUE),
            LMIC_cases = sum(cases_in_last_year_smoothed, na.rm = TRUE),
            LMIC_tests = sum(tests_in_last_year_smoothed, na.rm = TRUE),
            LMIC_sequences = sum(sequences_in_last_year, na.rm = TRUE)
  )

# Join global and LMICs data-frames (global stats will duplicate per LMIC group)
global_LMIC<-cbind (LMIC_stats, Global_stats)


LMIC_breakdown <- global_LMIC %>%
    group_by (LMIC_group)%>%
  summarize ( LMIC_pop_rate = 100* (LMIC_population/global_population),
              LMIC_cases_rate = 100* (LMIC_cases/global_cases),
              LMIC_test_rate = 100* (LMIC_tests/global_tests),
              LMIC_sequ_rate = 100* (LMIC_sequences/global_sequences)
  )

print (LMIC_breakdown)

LMIC_check<-LMIC_breakdown%>%summarise(
    pop_sum = sum(LMIC_pop_rate),
    case_sum = sum(LMIC_cases_rate),
    test_sum = sum(LMIC_test_rate),
    sequ_sum = sum(LMIC_sequ_rate)
)
# Unit test to make sure that all of the percents add to 100!
stopifnot('Percents dont add to 100' = LMIC_check[1,] == c(100,100,100,100))


# -----------------------------------------------------------------------------------
# -----------------------    Exporting to .csv files for local and remote repos
# -----------------------------------------------------------------------------------
 if (USE_CASE == 'local'){
   if (prev_month != "November" & prev_year!= "2021"){
     write.csv(find_changed_archetypes, paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/find_changed_archetypes', prev_month, '_to_', current_month, '.csv'))
   }
   write.csv(find_not_reported, paste0('../../../data/NGS_Data_Tables/', current_folder, '/PPI/find_delayed_test_reporting.csv'), row.names = F)
   write.csv(full_dataset, paste0('../../../data/NGS_Data_Tables/', current_folder, '/public/full_dataset.csv'), na = "NaN", row.names = FALSE)
   write.csv(find_map, paste0('../../../data/NGS_Data_Tables/', current_folder, '/PPI/find_map.csv'), na = "NaN", row.names = FALSE)
   write.csv(investment_df, paste0('../../../data/NGS_Data_Tables/', current_folder, '/PPI/find_projects.csv'), na = "NaN", row.names = FALSE)
   write.csv(investment_df_full, paste0('../../../data/NGS_Data_Tables/', current_folder, '/PPI/find_projects_full.csv'), na = "NaN", row.names = FALSE)
   write.csv(clean_dataset, paste0('../../../data/NGS_Data_Tables/', current_folder, '/public/clean_dataset.csv'), na = "NaN", row.names = FALSE)
   write.csv(find_TEST_countries, paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/find_TEST_countries.csv'), na = "NaN", row.names = FALSE )
   write.csv(seq_scatterplot, paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/seq_data.csv'), na = "NaN", row.names = FALSE )
   write.csv(seq_scatterplot_income_rmvd, paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/seq_data_SES.csv'), na = "NaN", row.names = FALSE )
   write.csv(test_scatterplot, paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/test_data.csv'), na = "NaN", row.names = FALSE )
   write.csv(test_scatterplot_income_rmvd, paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/test_data_SES.csv'), na = "NaN", row.names = FALSE )
   write.csv(SES_flourish, paste0('../../../data/NGS_Data_Tables/', current_folder,'/PPI/SES_breakdown.csv'), na = "NaN", row.names = FALSE)
   write.csv(LMIC_breakdown, paste0('../../../data/NGS_Data_Tables/',current_folder, '/PPI/LMIC_breakdown.csv'), na = "NaN", row.names = FALSE)
 }
# Briana's local paths 
#if (USE_CASE == 'local'){
  #if(prev_month!= 'November' & prev_year != '2021')
  #{
    #write.csv(find_map, paste0('/Users/bthrift/Documents/ppi-output/ngs_find_map/find_map.csv'), row.names = FALSE)
    
   # write.csv(full_dataset, paste0('/Users/bthrift/Documents/NGS-Capacity-map/data/NGS_Data_Tables/April_2022/public/full_dataset.csv'), na = "NaN", row.names = FALSE)
   # write.csv(clean_dataset, paste0('/Users/bthrift/Documents/NGS-Capacity-map/data/NGS_Data_Tables/April_2022/public/clean_dataset.csv'), na = "NaN", row.names = FALSE)
   # write.csv(find_rec_test, paste0('/Users/bthrift/Documents/NGS-Capacity-map/data/NGS_Data_Tables/April_2022/PPI/find_rec_test.csv'), na = "NaN", row.names = FALSE)
   # write.csv(seq_scatterplot, paste0('/Users/bthrift/Documents/NGS-Capacity-map/data/NGS_Data_Tables/April_2022/PPI/seq_scatterplot.csv'), na = "NaN", row.names = FALSE)
   # write.csv(test_scatterplot, paste0('/Users/bthrift/Documents/NGS-Capacity-map/data/NGS_Data_Tables/April_2022/PPI/test_scatterplot.csv'), na = "NaN", row.names = FALSE)
   # write.csv(find_changed_archetypes, paste0('/Users/bthrift/Documents/NGS-Capacity-map/data/NGS_Data_Tables/April_2022/PPI/find_changed_archetypes.csv'), na = "NaN", row.names = FALSE)
   # write.csv(SES_breakdown, paste0('/Users/bthrift/Documents/NGS-Capacity-map/data/NGS_Data_Tables/April_2022/PPI/SES_breakdown.csv'), na = "NaN", row.names = FALSE)
   # write.csv(LMIC_breakdown, paste0('/Users/bthrift/Documents/NGS-Capacity-map/data/NGS_Data_Tables/April_2022/PPI/LMIC_breakdown.csv'), na = "NaN", row.names = FALSE)
   # }
#}


if (USE_CASE == 'domino'){
  if (prev_month != "November" & prev_year!= "2021"){
    write.csv(find_changed_archetypes, paste0('/mnt/data/processed/find_changed_archetypes', prev_month, '_to_', current_month, '.csv'))
  }
  write.csv(find_not_reported, '/mnt/data/processed/find_delayed_test_reporting.csv', row.names = F)
  write.csv(full_dataset, '/mnt/data/processed/full_dataset.csv', na = "NaN", row.names = FALSE)
  write.csv(find_map, '/mnt/data/processed/find_map.csv', na = "NaN", row.names = FALSE)
  write.csv(investment_df, paste0('/mnt/data/NGS_Data_Tables/', current_folder, '/PPI/find_projects.csv'), na = "NaN", row.names = FALSE)
  write.csv(investment_df_full, paste0('/mnt/data/NGS_Data_Tables/', current_folder, '/PPI/find_projects_full.csv'), na = "NaN", row.names = FALSE)
  write.csv(clean_dataset, '/mnt/data/processed/clean_dataset.csv', na = "NaN", row.names = FALSE)
  write.csv(find_TEST_countries, '/mnt/data/processed/find_TEST_countries.csv', na = "NaN", row.names = FALSE )
  write.csv(seq_scatterplot, '/mnt/data/processed/seq_data.csv', na = "NaN", row.names = FALSE )
  write.csv(seq_scatterplot_income_rmvd, '/mnt/data/processed/seq_data_SES.csv', na = "NaN", row.names = FALSE )
  write.csv(test_scatterplot, '/mnt/data/processed/test_data.csv', na = "NaN", row.names = FALSE )
  write.csv(test_scatterplot_income_rmvd, '/mnt/data/processed/test_data_SES.csv', na = "NaN", row.names = FALSE )
  write.csv(LMIC_breakdown, '/mnt/data/processed/LMIC_breakdown.csv', na = "NaN", row.names = FALSE )
  write.csv(SES_flourish, '/mnt/data/processed/SES_breakdown.csv', na = "NaN", row.names = FALSE)
}
