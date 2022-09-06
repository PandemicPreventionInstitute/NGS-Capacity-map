#### Creates data files on sequences collected in the past 12 months for 26 countries at the amd1 level ####
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
library(data.table)
library(fuzzyjoin)
library(stringdist)
library(sf)
library(choroplethrAdmin1)

#### Clear environment & load data
rm(list = ls())
gc()
#### Line list of locations collected in previous 12 months
Countries_seq <- read_csv("../../../data/raw/gisaid_26countries_locations_lastyear.csv")

#### Basemap data linking file
keys <- read_csv("../../../data/raw/basemap/keys.csv")

#### Adm1 geojson
adm1 <- st_read("../../../data/raw/basemap/adm1.geojson")

#### Locations in GISAID data that have been IDed
Geokeys_IDed <- read_csv('../../../data/raw/geo_keys_ids.csv')

#### Admin 1 regions library
data("admin1.regions")

#### 2) Clean & simplify line list location data ####
#### Reduce size -> tabulate frequencies by unique location
Countries_locs <- group_by(Countries_seq, iso3, location) %>% 
    summarise(seqs = n()) %>% 
    ungroup()

#### Split location string into geographies at each level (up to 7)
Countries_locs <-  str_split(Countries_locs$location, "  |/", simplify = T) %>% 
    as_tibble() %>%
    mutate(across(everything(), ~str_trim(.))) %>%
    mutate(across(everything(), ~ifelse(.=="", NA, as.character(.)))) %>% 
    select(V2:V7) %>% 
    bind_cols(select(Countries_locs, iso3), ., select(Countries_locs, seqs))
colnames(Countries_locs) <- c("iso3", paste0("adm", 0:5), "seqs")

### Add country names
Countries_locs <- add_column(Countries_locs, cntry_n = countrycode(Countries_locs$iso3, "iso3c", "country.name"), 
                             .after = "iso3")

### Check country names = adm0
Countries_locs <- mutate(Countries_locs, cntry_n_ch = countrycode(adm0, "country.name", "country.name"))
## For cases where country name != adm0, shift all geographies down 1 level
Countries_locs[Countries_locs$cntry_n != Countries_locs$cntry_n_ch | is.na(Countries_locs$cntry_n_ch), paste0("adm", 1:5)] <- 
    Countries_locs[Countries_locs$cntry_n != Countries_locs$cntry_n_ch | is.na(Countries_locs$cntry_n_ch), paste0("adm", 0:4)]
# Drop adm0 & country name check variable
Countries_locs <- select(Countries_locs, -adm0, -cntry_n_ch)

### Drop obs with no subnational division info
Countries_locs <- filter(Countries_locs, !is.na(adm1))

#### 3) Process & match subnational division fields ####
#### Try to match w/ already IDed geokeys
Countries_locs <- select(Geokeys_IDed, cntry_n = country, adm1 = geo_l3, adm2 = geo_l4, adm3 = geo_l5, adm4 = geo_l6, adm5 = geo_l7, geoid) %>% 
    mutate(cntry_n = countrycode(cntry_n, "country.name", "country.name")) %>% 
    left_join(Countries_locs, .)

#### Join with keys to assign adm1_id's
Countries_locs <- select(keys, -adm2_id) %>% 
    unique() %>% 
    left_join(Countries_locs, .)
    
#### Try to direct name match with adm1 file
Countries_locs <- st_drop_geometry(adm1) %>% 
    select(iso3, adm1 = adm1_name, adm1_id) %>% 
    left_join(select(filter(Countries_locs, is.na(adm1_id)), -adm1_id), .) %>% 
    bind_rows(filter(Countries_locs, !is.na(adm1_id)))

#### Name match with all caps & cleaned names
## Create all-caps & clean version of name
Countries_locs <- add_column(Countries_locs, adm1_name = str_to_upper(Countries_locs$adm1) %>% 
                             str_replace_all("&", "AND") %>% 
                             str_replace_all("-|–|_", " ") %>% 
                             str_remove_all("[[:punct:]]") %>% 
                             str_remove_all("[[:symbol:]]") %>% 
                             stri_trans_general(id = "Latin-ASCII") %>%
                             str_squish(), .after = "adm1")
## Match w/ adm1
Countries_locs <- st_drop_geometry(adm1) %>% 
    select(iso3, adm1_name, adm1_id) %>% 
    mutate(adm1_name = str_to_upper(adm1_name) %>% 
               str_replace_all("&", "AND") %>% 
               str_replace_all("-|–|_", " ") %>% 
               str_remove_all("[[:punct:]]") %>% 
               str_remove_all("[[:symbol:]]") %>% 
               stri_trans_general(id = "Latin-ASCII") %>%
               str_squish()) %>% 
    left_join(select(filter(Countries_locs, is.na(adm1_id)), -adm1_id), .) %>% 
    bind_rows(filter(Countries_locs, !is.na(adm1_id)))

#### Name match with adm level 2 ids
Countries_locs <- select(keys, iso3, adm1 = adm2_id, adm1_id) %>% 
    left_join(select(filter(Countries_locs, is.na(adm1_id)), -adm1_id), .) %>%
    bind_rows(filter(Countries_locs, !is.na(adm1_id)))

#### 4) Fix belgium ####
##### BEL- have 3rd level, aggregate up to 2nd & 1st
### Run script to extract raw data for belgium
source("belgium_rawdata.R")
### Get keys linking Arrondissements (districts, 3rd level) to provinces (2nd level) & regions (1st level)
BEL_provs <- select(latest_data, TX_ADM_DSTR_DESCR_NL, PROVINCE, REGION) %>% 
    filter(!is.na(PROVINCE)) %>% 
    unique() %>% 
    mutate(iso3 = "BEL", micr_nm = str_remove_all(TX_ADM_DSTR_DESCR_NL, "Arrondissement ") %>% 
               str_to_upper() %>% 
               stri_trans_general(id = "Latin-ASCII")) %>% 
    select(-TX_ADM_DSTR_DESCR_NL)

#### Read in Pop-Geo file
Geo.Pop <- st_read("../../../data/raw/GeoPop.shp")

#### Combine belgium province & region data with arrondissements- get belgium level 1/2 divisions
Countries_locs <- inner_join(Geo.Pop, BEL_provs) %>% 
    mutate(macr_nm = case_when(iso3 == "BEL" ~ REGION, 
                               iso3 != "BEL" ~ macr_nm)) %>% 
    mutate(micr_nm = case_when(iso3 == "BEL" ~ PROVINCE, 
                               iso3 != "BEL" ~ micr_nm)) %>%
    st_drop_geometry() %>% 
    select(geoid, adm1_id = macr_nm) %>% 
    left_join(select(filter(Countries_locs, is.na(adm1_id) & !is.na(geoid)), -adm1_id), .) %>% 
    bind_rows(filter(Countries_locs, !is.na(adm1_id) | is.na(geoid)))

### Free memory
rm(list = c("aa", "flag", "STRING", "latest_data", "getDate", "BEL_provs", "Geo.Pop"))
gc()

#### 5) Geocode remaining unmatched ####
#### Concatenate locations of remaining umatched for geocoding
To.geocode <- filter(Countries_locs, is.na(adm1_id)) %>% 
    select(cntry_n, adm1, adm2:adm5) %>% 
    unite(location_to_geocode, adm5, adm4, adm3, adm2, adm1,  
          cntry_n, sep = ", ", na.rm = T)

#### Run script to geocode using google maps api
source("gmaps_api_geocode.R")

### Get centroid coordinates of geocoded locations
Geocoded.coords <- mp_get_points(Geocoded, all_results = T) %>% 
    group_by(id) %>%
    mutate(n_results = n())

### Get number of results with location type strings
Geocoded.coords <- sapply(Geocoded, as.character) %>% 
    str_extract_all("<result>\\s*(.*?)\\s*</type>") %>% 
    sapply(length) %>% 
    tibble(id = seq(1:length(Geocoded)), type_results = .) %>% 
    left_join(Geocoded.coords, .)
    
### Extract location types                        
Location_types <- sapply(Geocoded, as.character) %>% 
    str_extract_all("<result>\\s*(.*?)\\s*</type>")
names(Location_types) <- seq(1:length(Geocoded))
Location_types <- sapply(Location_types, length) %>% 
    rep(names(Location_types), .) %>% 
    tibble(id = ., location_text = unlist(Location_types))
## Clean location strings
Location_types <- mutate(Location_types, admin_lvl = str_remove_all(location_text, "<result>\\\n") %>% 
                             str_remove_all("<type>") %>% str_remove_all("</type>") %>% 
                             str_squish())

#### Add back to geocoded data
Geocoded.coords <- filter(Geocoded.coords, n_results == type_results) %>% 
    bind_cols(., select(Location_types, admin_lvl)) %>% 
    bind_rows(mutate(filter(Geocoded.coords, n_results != type_results), admin_lvl = NA))

## Get entered country & google returned country
Geocoded.coords <- mutate(Geocoded.coords, entered_country = 
                              str_replace_all(address, "^.*(?=, )", "") %>% 
                              str_replace_all(", ", "") %>% 
                              countrycode("country.name", "country.name")) %>% 
    mutate(google_country = 
               str_replace_all(address_google, "^.*(?=, )", "") %>% 
               str_replace_all(", ", "") %>% 
               countrycode("country.name", "country.name")) %>% 
    ## Mark invalid returns (country level or different country)
    mutate(valid_return = if_else(admin_lvl == "country" | is.na(admin_lvl) | 
                                      entered_country != google_country, F, T))

### Add country codes 
Geocoded.coords <- mutate(Geocoded.coords, iso3 = countryname(entered_country, "iso3c"))

#### 6) Search for polygons that contain geocoded points ####
#### Obtain matches- polygon contains geocoded centroid coordinates
Geo.matches <- mapply(function(points, polygons) {
    rownames(polygons) <- seq(1:nrow(polygons))
    
    # Find matches
    matches <- st_contains(polygons, points)

    # Join matching locations to adm1_id's
    locations_matched <- sapply(t(matches), function(x) {
        st_drop_geometry(polygons) %>%
            select(adm1_id) %>%
            filter(rownames(.) %in% x)
    }) %>%
        sapply(function(x) if(identical(x, character(0))) NA_character_ else x) %>%
        unlist() %>%
        tibble(points, adm1_id = .)
    
    return(locations_matched)
}, points = filter(Geocoded.coords, valid_return == T) %>%
    split(.$iso3), 
polygons = split(adm1, adm1$iso3)[grepl(paste(unique(filter(Geocoded.coords, valid_return == T)$iso3), 
                                              collapse = "|"), 
                                        names(split(adm1, adm1$iso3)))], SIMPLIFY = F) %>% 
    bind_rows()

#### Get matches w/in a certain distance
Geo.windist <- mapply(function(points, polygons) {
    rownames(polygons) <- seq(1:nrow(polygons))
    
    # Find matches
    matches <-  st_is_within_distance(st_convex_hull(polygons), points, dist = 1000)
    
    # Join matching locations to adm1_id's
    locations_matched <- sapply(t(matches), function(x) {
        st_drop_geometry(polygons) %>%
            select(adm1_id) %>%
            filter(rownames(.) %in% x)
    }) %>%
        sapply(function(x) if(identical(x, character(0))) NA_character_ else x) %>%
        unlist() %>%
        tibble(points, adm1_id = .)
    
    return(locations_matched)
}, points = filter(Geocoded.coords, id %in% filter(group_by(Geo.matches, id), all(is.na(adm1_id)))$id) %>%
    split(.$iso3), 
polygons = split(adm1, adm1$iso3)[grepl(paste(unique(filter(Geocoded.coords, id %in% filter(group_by(Geo.matches, id), all(is.na(adm1_id)))$id)$iso3), 
                                              collapse = "|"), 
                                        names(split(adm1, adm1$iso3)))], SIMPLIFY = F) %>% 
    bind_rows()

#### Extract id's & adm1_id's
Geo.matches_ids <- filter(Geo.matches, !is.na(adm1_id)) %>%
    select(id, iso3, adm1_id) %>% unique %>% 
    mutate(duplicated = duplicated(id)) %>% 
    group_by(id) %>% 
    filter(all(duplicated == F))

#### Add adm1_id's back to full set of geocoded locations
Geocoded.coords <- select(Geo.matches_ids, id, iso3, adm1_id) %>% 
    bind_rows(select(Geo.windist, id, iso3, adm1_id)) %>% 
    left_join(Geocoded.coords, .)

#### Mark duplicate results in geocoded locations- locations that turned out > 1 adm1_id
Geocoded.coords <- filter(Geo.matches, !is.na(adm1_id)) %>%
    select(id, iso3, adm1_id) %>% unique %>% 
    mutate(duplicated = duplicated(id)) %>% 
    filter(duplicated == T) %>% select(id, iso3, duplicated) %>% 
    left_join(Geocoded.coords, .)

#### Check to make sure all locations missing an adm1_id either not a valid result or duplicated/not unambiguous result
filter(Geocoded.coords, is.na(adm1_id) & valid_return == T & is.na(duplicated)) %>% nrow() == 0

#### Put results back into main data
Countries_locs <- select(filter(Countries_locs, is.na(adm1_id)), -adm1_id) %>% 
    bind_cols(select(unique(st_drop_geometry(select(Geocoded.coords, id, adm1_id))), adm1_id)) %>% 
    bind_rows(filter(Countries_locs, !is.na(adm1_id)))

#### 7) Clean & tabulate results ####
#### Drop cases with adm1_id missing
seqs_adm1 <- filter(Countries_locs, !is.na(adm1_id)) %>% 
    ### Group by unique country & adm1 identifiers & sum sequences
    group_by(iso3, adm1_id) %>% 
    summarise(seqs = sum(seqs))

#### 8) Export data ####
write_csv(seqs_adm1, "../../../data/raw/basemap/seqs_adm1.csv")
