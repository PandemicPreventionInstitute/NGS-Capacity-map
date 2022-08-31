#### Base map of population of countries in sjbeckett/subregionalcovid19 at multiple administrative levels ####
#### Jordan Klein

##### 1) Setup ####
rm(list = ls())

### Load packages & set authorization for google drive
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
library("devtools")
library("subregionalcovid19")
googledrive::drive_auth(email = T)
library(httr)
library(vietnameseConverter)
library(sf)

#### Read in Pop-Geo file
Geo.Pop <- st_read("../../../data/raw/GeoPop.shp")
unique(Geo.Pop$iso3) %>% sort()

#### 2) Get Table of countries' population ####
##### Geometries
#### Impot shapefile of country geometries & filter out the 26 used here (-china & taiwan)
shapefile_raw <- read_delim('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/Geospatial_Data/geometric_polygons_country.txt', 
                        delim = "\t") %>% 
    filter((code %in% unique(Geo.Pop$iso3)) & code != "CHN" & code != "TWN")
#### Convert into a table of country isocodes & geometries
shapefile <- select(shapefile_raw, iso3 = code) %>% 
    st_set_geometry(st_as_sfc(shapefile_raw$geometry, GeoJSON = T))

#### Get geometries for china & taiwan
shapes_chn_twn <- filter(Geo.Pop, iso3 == "CHN" | iso3 == "TWN") %>%
    group_by(iso3) %>% 
    summarise()

### Combine geometries for all 26 countries
Country_geoms <- bind_rows(shapefile, shapes_chn_twn)

##### Population
Countries_pop <- st_drop_geometry(Geo.Pop) %>% 
    group_by(iso3, cntry_n) %>% 
    summarise(pop = sum(pop, na.rm = T))

##### Combine countries geoms & pop
Countries <- merge(Countries_pop, Country_geoms, by = "iso3") %>% 
    st_set_geometry(.$geometry)

### Clear memory
rm(shapefile, shapefile_raw, shapes_chn_twn)
gc()

#### 3) Get get level 1 & 2 subnational divisions for each country ####
##### AUS- only 1st level

##### BEL- have 3rd level, aggregate up to 2nd (treat as 1st)
### Run script to extract raw data for belgium
source("belgium_rawdata.R")
### Get keys linking Arrondissements (districts, 3rd level) to provinces (2nd level)
BEL_provs <- select(latest_data, TX_ADM_DSTR_DESCR_NL, PROVINCE) %>% 
    filter(!is.na(PROVINCE)) %>% 
    unique() %>% 
    mutate(iso3 = "BEL", micr_nm = str_remove_all(TX_ADM_DSTR_DESCR_NL, "Arrondissement ") %>% 
               str_to_upper() %>% 
               stri_trans_general(id = "Latin-ASCII")) %>% 
    select(-TX_ADM_DSTR_DESCR_NL)

#### Combine belgium province data with arrondissements- get belgium level 1 divisions
Geo.Pop_subdivs <- left_join(Geo.Pop, BEL_provs) %>% 
    mutate(macr_nm = case_when(iso3 == "BEL" ~ PROVINCE, 
                               iso3 != "BEL" ~ macr_nm)) %>% 
    select(-PROVINCE)
### Free memory
rm(list = c("aa", "flag", "STRING", "latest_data", "getDate", "BEL_provs"))
gc()

##### BRA- pop at level 2, shapefiles at level 1
#### Get level 2 shapefiles from IBGE
tmpdir <- tempdir()
unzip("../../../data/raw/shapefiles/BR_Municipios_2021.zip", exdir = tmpdir)
IBGE <- paste0(tmpdir, "/BR_Municipios_2021.shp") %>% 
    st_read()

### Put Get Brazil level 2 subdivisions
BRA2 <- mutate(pop_brazil, ibge = as.character(ibge)) %>%
    merge(IBGE, by.x= "ibge", by.y = "CD_MUN", all.x = T) %>% 
    st_set_geometry(.$geometry) %>% 
    mutate(iso3 = "BRA", cntry_n = "Brazil") %>% 
    select(iso3, cntry_n, macr_cd = state, micr_cd = ibge, micr_nm = NM_MUN, pop = pop2021) %>% 
    st_transform("WGS84")
### Combine Brazil level 2 divisions back in the data
Geo.Pop_subdivs <- filter(Geo.Pop_subdivs, iso3 == "BRA") %>% 
    st_drop_geometry %>% 
    mutate(macr_cd = micr_cd, macr_nm = micr_nm) %>% 
    select(-c(micr_cd:pop)) %>% 
    left_join(BRA2) %>% 
    st_set_geometry(.$geometry) %>% 
    bind_rows(filter(Geo.Pop_subdivs, iso3 != "BRA"))
### Free up memory
rm(tmpdir, IBGE, BRA2)
gc()

##### CAN- pop at level 2, level 1 in the data

##### CHN- only 1st level

##### CZE- pop at level 2, level 1 in the data

##### DEU- at level 2, level 1 not in the data
### Get level 1-level 2 keys
DEU_land <- st_read('https://public.opendatasoft.com/explore/dataset/covid-19-germany-landkreise/download/?format=geojson&timezone=Europe/Berlin&lang=en') %>% 
    st_drop_geometry %>%
    select(bl_id, bl, micr_nm = county) %>% 
    mutate(iso3 = "DEU", micr_nm = str_to_upper(micr_nm) %>% 
               stri_trans_general(id = "Latin-ASCII"))

### Combine level 1 divisions back in the data
Geo.Pop_subdivs <- left_join(Geo.Pop_subdivs, DEU_land) %>% 
    mutate(macr_cd = case_when(iso3 == "DEU" ~ bl_id, iso3 != "DEU" ~ macr_cd), 
           macr_nm = case_when(iso3 == "DEU" ~ bl, iso3 != "DEU" ~ macr_nm)) %>% 
    select(-bl_id, -bl)

### Free up memory
rm(DEU_land)
gc()

##### DNK- at level 2, level 1 not in the data
### Get level 1-level 2 keys
DNK_reg <- st_read('https://raw.githubusercontent.com/magnuslarsen/geoJSON-Danish-municipalities/master/municipalities/municipalities.geojson') %>% 
    st_drop_geometry %>%
    group_by(label_en, iso_3166_2) %>% 
    summarise %>% 
    select(iso_3166_2, micr_nm = label_en) %>% 
    mutate(iso3 = "DNK", micr_nm = str_to_upper(micr_nm) %>% 
               stri_trans_general(id = "Latin-ASCII")) 
# edit some of the names so they match those in the main data
DNK_reg$micr_nm[DNK_reg$micr_nm == "BRONDERSLEV-DRONNINGLUND"] <- "BRONDERSLEV"
DNK_reg$micr_nm[DNK_reg$micr_nm == "ARHUS"] <- "AARHUS"
DNK_reg$micr_nm[DNK_reg$micr_nm == "NORDFYNS"] <- "NORDFYN"

### Combine level 1 divisions back in the data
Geo.Pop_subdivs <- left_join(Geo.Pop_subdivs, DNK_reg) %>% 
    mutate(macr_cd = case_when(iso3 == "DNK" ~ iso_3166_2, iso3 != "DNK" ~ macr_cd)) %>% 
    select(-iso_3166_2)

### Free up memory
rm(DNK_reg)
gc()

##### DZA- only 1st level

##### ESP- at level 2, level 1 in data (macr_cd right, macr_nm wrong)
### Fix macr_nm
Geo.Pop_subdivs <- Geo.Pop_subdivs %>% 
    mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "03", "ILLES BALEARS", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "04", "CANARIAS", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "05", "CANTABRIA", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "06", "CASTILLA - LA MANCHA", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "08", "CATALUNA", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "09", "CEUTA", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "10", "EXTREMADURA", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "11", "GALICIA", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "12", "LA RIOJA", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "14", "MELILLA", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "15", "MURCIA", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "16", "NAVARRA", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "17", "PAIS VASCO", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "18", "ASTURIAS", macr_nm)) %>% 
               mutate(macr_nm = if_else(iso3 == "ESP" & macr_cd == "19", "COMUNITAT VALENCIANA", macr_nm))

##### GBR- at level 3 (but treat as level 2), level 1 in data (first letter of micr_cd)
#### Assign level 1 names
Geo.Pop_subdivs <- mutate(Geo.Pop_subdivs, macr_nm = case_when(iso3 == "GBR" & grepl("E", micr_cd) ~ "England", 
                                                               iso3 == "GBR" & grepl("N", micr_cd) ~ "Northern Ireland", 
                                                               iso3 == "GBR" & grepl("S", micr_cd) ~ "Scotland", 
                                                               iso3 == "GBR" & grepl("W", micr_cd) ~ "Wales", 
                                                               iso3 != "GBR" ~ macr_nm))

##### IND- only 1st level

##### ITA- pop at level 2, level 1 in the data

##### JPN- at level 2, level 1 not in the data
### Run script to extract raw data for japan- level 1-level 2 keys
source("japan_rawdata.R")
### Get get level 1 names in the data
Geo.Pop_subdivs <- JPN_reg_pref %>% 
    mutate(iso3 = "JPN", micr_nm = str_to_upper(prefecture)) %>% 
    select(-prefecture) %>%
    left_join(Geo.Pop_subdivs, .) %>% 
    mutate(macr_nm = case_when(iso3 == "JPN" ~ region, 
                               iso3 != "JPN" ~ macr_nm)) %>% 
    select(-region)

## Free up memory
rm(Japanregions, JPNprefectures, dataJapan, JPN_reg_pref)
gc()

##### MYS- only 1st level

##### NLD- pop at level 2, level 1 in the data

##### PHL- pop at level 2, level 1 in the data

##### SAU- only 1st level

##### SWE- only 1st level

##### THA- pop at level 2, level 1 in the data

##### TWN- only 1st level

##### USA- pop at level 2, level 1 in the data

##### VEN at level 2, level 1 not in the data
### Get get level 1 names in the data
Geo.Pop_subdivs <- pop_venezuela %>% 
    mutate(iso3 = "VEN", micr_nm = str_to_upper(State) %>% 
               stri_trans_general(id = "Latin-ASCII") %>% 
               str_replace_all("[:punct:]", " ")) %>% 
    select(-State, -Population_2011) %>% 
    left_join(Geo.Pop_subdivs, .) %>% 
    mutate(macr_nm = case_when(iso3 == "VEN" ~ Region, 
                               iso3 != "VEN" ~ macr_nm)) %>% 
    select(-Region)

##### VNM- only 1st level

##### ZAF- only 1st level

##### ZWE- only 1st level

#### 4) Clean data ####
### Identify countries that only have 1st level -> move micr_cd & nm to macr
Geo.Pop_subdivs <- Geo.Pop_subdivs %>% 
    mutate(max_lvl = if_else(macr_cd == "00" & is.na(macr_nm), 1, 2)) %>% 
    mutate(macr_cd = case_when(max_lvl == 1 ~ micr_cd, 
                               max_lvl == 2 ~ macr_cd), 
           macr_nm = case_when(max_lvl == 1 ~ micr_nm, 
                               max_lvl == 2 ~ macr_nm)) %>% 
    mutate(micr_cd = ifelse(max_lvl == 1, NA, micr_cd), 
           micr_nm = ifelse(max_lvl == 1, NA, micr_nm))

### Get subdivision level 1 & 2 identifiers w/in each country
Geo.Pop_subdivs <- Geo.Pop_subdivs %>% 
    mutate(adm1_id = case_when(!is.na(macr_nm) ~ macr_nm, is.na(macr_nm) ~ macr_cd) %>% 
               str_replace_all("[:punct:]| ", "_")) %>%
    mutate(adm2_id = micr_cd)

#### 5) Get ADM1 table ####
ADM1 <- filter(Geo.Pop_subdivs, iso3 != "BRA") %>%
    group_by(iso3, cntry_n, adm1_id) %>% 
    summarise(pop = sum(pop, na.rm = T))
# Add back in Brazil data later to improve performance
ADM1 <- filter(Geo.Pop, iso3 == "BRA") %>% 
    select(iso3, cntry_n, adm1_id = micr_nm, pop) %>% 
    bind_rows(ADM1, .) %>% 
    add_column(adm1_name = str_replace_all(.$adm1_id, "_", " "), .after = "adm1_id") %>% 
    mutate(adm1_name = str_squish(adm1_name)) %>%
    mutate(adm1_name = case_when(str_count(adm1_name, "[:alpha:]") <= 2 ~ adm1_name, 
                         str_count(adm1_name, "[:alpha:]") > 2 ~ str_to_title(adm1_name)))

# Check to make sure nrows of adm1 keys in data = rows in adm1 table
select(Geo.Pop_subdivs, iso3, adm1_id) %>% st_drop_geometry %>% 
    unique %>% nrow == nrow(ADM1)

#### 6) Get ADM2 table ####
ADM2 <- filter(Geo.Pop_subdivs, max_lvl == 2) %>% 
    mutate(adm1_name = str_replace_all(adm1_id, "_", " ") %>% 
               str_squish) %>%
    mutate(adm1_name = case_when(str_count(adm1_name, "[:alpha:]") <= 2 ~ adm1_name, 
                                 str_count(adm1_name, "[:alpha:]") > 2 ~ str_to_title(adm1_name))) %>% 
    select(iso3, cntry_n, adm1_id, adm1_name, adm2_id, adm2_name = micr_nm, pop) %>% 
    mutate(adm2_name = case_when(str_detect(adm2_name, "[:lower:]", negate = T) ~ str_to_title(adm2_name), 
                                 str_detect(adm2_name, "[:lower:]") ~ adm2_name) %>% 
               str_squish())

# Check to make sure nrows of adm2 keys in data = rows in adm2 table
filter(Geo.Pop_subdivs, max_lvl == 2) %>% 
    select(iso3, adm1_id, adm2_id) %>% st_drop_geometry %>% 
    unique %>% nrow == nrow(ADM2)

#### 7) Create file w/ keys for each geography ####
Keys <- st_drop_geometry(Geo.Pop_subdivs) %>% 
    select(geoid, iso3, adm1_id, adm2_id)

#### 9) Save files ####
#### Keys
write_csv(Keys, "../../../data/raw/basemap/keys.csv")

#### Countries
st_write(Countries, "../../../data/raw/basemap/countries.geojson", append = F)

#### Level 1 administrative divisions
st_write(ADM1, "../../../data/raw/basemap/adm1.geojson", append = F)

#### Level 2 administrative divisions
st_write(ADM2, "../../../data/raw/basemap/adm2.geojson", append = F)
