#### Process Gridded Population of the World Data ####
#### Jordan Klein

#### 1) Setup ####
rm(list = ls())
#### Load packages
library(httr)
library(tidyverse)
library(janitor)
library(countrycode)
library(lubridate)

## GPW data access credentials
gpw_credentials <- read_csv("../../../data/gpw_credentials.csv", col_names = F) 
user <- as.character(gpw_credentials[1,1])
pw <- as.character(gpw_credentials[2,1])

#### 2) Import GPW data ####
# Get URL of data
GPW_url <- "https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-admin-unit-center-points-population-estimates-rev11/gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv.zip"
# Extract raw data from url
GPW_extract <- GET(GPW_url, authenticate(user = user, password = pw, type = "basic"))
# Get content
GPW_content <- content(GPW_extract, type = "application/zip", as = "raw")
### Create temporary file & import raw data
zipfile <- tempfile()
writeBin(GPW_content, zipfile)
# Global
globalfile <- tempfile()
GPW_global_raw <- unzip(zipfile, "gpw_v4_admin_unit_center_points_population_estimates_rev11_global.csv", exdir = globalfile) %>% 
    read_csv()
unlink(globalfile, force = T, recursive = T)
gc()
# US West
uswfile <- tempfile()
GPW_usw_raw <- unzip(zipfile, "gpw_v4_admin_unit_center_points_population_estimates_rev11_usa_west.csv", exdir = uswfile) %>% 
    read_csv()
unlink(uswfile, force = T, recursive = T)
gc()
# US South
ussfile <- tempfile()
GPW_uss_raw <- unzip(zipfile, "gpw_v4_admin_unit_center_points_population_estimates_rev11_usa_south.csv", exdir = ussfile) %>% 
    read_csv()
unlink(ussfile, force = T, recursive = T)
gc()
# US Northeast
usnefile <- tempfile()
GPW_usne_raw <- unzip(zipfile, "gpw_v4_admin_unit_center_points_population_estimates_rev11_usa_northeast.csv", exdir = usnefile) %>% 
    read_csv()
unlink(usnefile, force = T, recursive = T)
gc()
# US Midwest
usmwfile <- tempfile()
GPW_usmw_raw <- unzip(zipfile, "gpw_v4_admin_unit_center_points_population_estimates_rev11_usa_midwest.csv", exdir = usmwfile) %>% 
    read_csv()
unlink(usmwfile, force = T, recursive = T)
unlink(zipfile, force = T, recursive = T)
gc()

#### 3) Clean & export data ####
#### Clean
GPW_full <- bind_rows(GPW_global_raw, GPW_usne_raw, GPW_usmw_raw, GPW_uss_raw, GPW_usw_raw) %>% 
    select(GUBID:CONTEXT_NM, UN_2020_E)

#### Export
write_csv(GPW_full, file.path("../../../data/raw", "gpw.csv.gz"))
