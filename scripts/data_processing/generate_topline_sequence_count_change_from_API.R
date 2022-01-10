# This file takes the gisaid metadata returns a tibble with an index of days,
# a count of the number of sequences that were collected in the prior 30 days
# and were submitted to GISAID prior to the index date, the same metric but 
# lagged the days, and the ratio of these two 

# This file generates the masterfile, calculating this from the beginning of
# 2020 onward. 

#getwd()

USE_CASE = 'domino' # 'domino or local'
# Libraries ---------------------------------------------------------------
if (USE_CASE == 'domino'){
install.packages("tidyverse", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("lubridate", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("zoo", dependencies=TRUE, repos='http://cran.us.r-project.org')
}
library(tidyverse)
library(lubridate)
library(zoo)

# Load data ---------------------------------------------------------------

#local path
if (USE_CASE == 'local'){
GISAID_METADATA_PATH<-'../data/raw/metadata.csv'
}
#Domino paths
if (USE_CASE == 'domino'){
GISAID_METADATA_PATH<-'/mnt/data/raw/metadata.csv' # API path
#GISAID_METADATA_PATH<-'/mnt/data/processed/inital_clean_metadata.csv'
}

#df <- read_csv(GISAID_METADATA_PATH,
               #col_types = 'iTTlcc') %>% 
  #filter(collect_date > ymd('2020-1-1'))

GISAID_METADATA_PATH<-'/mnt/data/raw/metadata.csv' # API path
df<-read_csv(GISAID_METADATA_PATH) %>%
  filter(collection_date > ymd('2019-12-01'))





# Set up arrays -----------------------------------------------------------

date_seq = as.character.Date(seq(ymd("2019-12-01"), today()-1, by = 'day'))

results = vector(mode = 'integer',
                 length = length(date_seq))


# Loop through calculation ------------------------------------------------

for (i in 1:length(date_seq)){
  
  day_iter = ymd(date_seq[i])
  
  results[i] <-  df %>% 
    filter(collection_date >= day_iter - days(29),
           collection_date <= day_iter,
           submission_date <= day_iter) %>% 
    nrow()
  
  
}

# Combine date and result arrays into a tibble ----------------------------

combined_df <- tibble(date = ymd(date_seq),
                      n = results)


combined_df <- combined_df %>% 
  mutate(n_lag_30 = c(rep(NA_integer_, 30), combined_df$n[1:(nrow(combined_df)-30)]),
         r = n / n_lag_30)


# Save time series --------------------------------------------------------

# Domino path
if (USE_CASE == 'domino'){
write_csv(combined_df, '/mnt/data/processed/sequences_last_30_days.csv')
}
# local path
if (USE_CASE == 'local'){
write_csv(combined_df, '../data/processed/sequences_last_30_days.csv')
}
