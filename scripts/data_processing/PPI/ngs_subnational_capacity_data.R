#### Create NGS subnational capacity data file ####
#### Jordan Klein

#### 1) Setup ####
#### Run gisaid metadata extraction & processing script
source("gisaid_metadata_geokeys_processing.R")
## Update file linking gisaid locations to shapefiles & population w/ new gisaid metadata
source("geokey_id_update.R")

#### Clear environment & load metadata
rm(list = ls())
Metadata_raw <- read_csv("../../../data/raw/metadata.csv") # from extracted datastream

##### Get set of unique geolocations present in the metadata
#### Split location string into geographies at each level (up to 7)
Locations_split <-  str_split(Metadata_raw$location, "  |/", simplify = T) %>% 
    as_tibble() %>%
    mutate(across(everything(), ~str_trim(.))) %>%
    mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
colnames(Locations_split) <- paste0("geo_l", 1:7)

### Combine back with metadata
Metadata <- bind_cols(Metadata_raw, Locations_split)
# give each location a unique id
Metadata <-tibble(location = unique(Metadata$location), 
                  loc_id = seq(1, nrow(unique(select(Metadata, location))))) %>% 
    full_join(Metadata)

### Clear memory & unneeded objects
rm(Metadata_raw)
rm(Locations_split)
gc()

#### Load packages
library("devtools")
library("subregionalcovid19")
library(geojsonR)
library(geojsonsf)
library(geojsonio)

#### Read in joined Pop-Geo file & IDed unique geokeys in metadata
Geo.Pop <- st_read("../../../data/raw/GeoPop.shp")
Geokeys_IDed <- read_csv('../../../data/raw/geo_keys_ids.csv', col_types = "dcccccccc")

#### 2) Clean data ####
### Houston city -> Harris county's geoid 
Geokeys_IDed <- mutate(Geokeys_IDed, geoid = ifelse(geo_l3 == "Texas" & geo_l4 == "Houston", "USA840_TX_48201", geoid)) %>% 
    ## Brandenburg Germany -> no geoid (not fine-grained enough)
    mutate(geoid = ifelse(geo_l3 == "Brandenburg", NA, geoid))

# ### Combine all of Berlin (gisaid doesn't specify districts w/in the city)
# Get Berlin polygon
Berlin.geo <- filter(Geo.Pop, grepl("BERLIN", micr_nm)) %>%
    st_union()
# Get berlin population-geospatial data & replace individual districts (use geoid DEU276_00_71- central district)
Berlin <- filter(Geo.Pop, grepl("BERLIN", micr_nm)) %>%
    select(pop) %>%
    st_drop_geometry() %>%
    sum() %>%
    tibble(geoid = "DEU276_00_71", iso3 = "DEU", cntry_n = "Germany", macr_cd = "00", macr_nm = NA, micr_cd = "71", micr_nm = "Berlin", pop = .) %>%
    st_set_geometry(Berlin.geo)
# Combine back into original data
Geo.Pop <- bind_rows(Geo.Pop, Berlin) %>%
    filter(!grepl("BERLIN", micr_nm))

#### Get sequences submitted per 100k in last 12 mos in each geoid we have pop-shapefile data for in 26 countries of interest
NGS.map <- filter(Metadata, submission_date >= today()-365) %>%
    select(loc_id) %>% 
    group_by(loc_id) %>% 
    summarise(seqs = n()) %>% 
    left_join(Geokeys_IDed) %>% 
    filter(!is.na(geoid)) %>% 
    group_by(geoid) %>% 
    summarise(seqs = sum(seqs)) %>% 
    left_join(Geo.Pop, .) %>% 
    mutate(seq_pc = 100000*seqs/pop) %>% 
    # log transform since this is very skewed
    mutate(log_seqpc = log(seq_pc)) %>% 
    # change subnational location names to title case
    mutate(macr_nm = str_to_title(macr_nm), micr_nm = str_to_title(micr_nm)) %>% 
    # rounding
    mutate(seq_pc = round(seq_pc, digits = 1), log_seqpc = round(log_seqpc, digits = 2))

#### 3) Export data ####
#### Get shapefiles for all countries
## Import shapefile for country borders from repo & save as a csv
SHAPEFILES_FOR_FLOURISH_PATH <- url('https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/Geospatial_Data/geometric_polygons_country.txt') # shapefiles for mapping
shapefile <- read_delim(SHAPEFILES_FOR_FLOURISH_PATH, delim = "\t", show_col_types = FALSE) 

## Before exporting filter out countries in the NGS map data
filter(shapefile, !c(code %in% unique(NGS.map$iso3))) %>% 
    write_csv("../../../data/raw/shapefile.csv")

#### As csv by saving the geometry as wellknown text
st_write(NGS.map, "../../../data/raw/NGS_subnational_capacity.csv", layer_options = "GEOMETRY=AS_WKT", append = F)

#### Geojson w/ geometries & geoids
st_write(select(NGS.map, geoid), "../../../data/raw/NGS_subnational_capacity.geojson", append = F)
