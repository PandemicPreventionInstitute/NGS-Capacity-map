#### Script to join new locations from gisaid with population-geometry data ####
#### Jordan Klein

#### 1) Setup ####
#### Run geokeys cleaning scripts
source("geokeys_cleaning.R")

#### Load packages
library("devtools")
library("subregionalcovid19")

#### Read in joined Pop-Geo file & IDed unique geokeys in metadata
Geo.Pop <- st_read("../../../data/raw/GeoPop.shp")
Geokeys_IDed <- read_csv('../../../data/raw/geo_keys_ids.csv')

#### Find out which locations from the gisaid pull have already been IDed
Geokeys_IDed <- mutate(Geokeys_IDed, already.ID = T)

Geokeys_matched <- select(Geokeys_IDed, -loc_id) %>% 
    left_join(Geo_keys, .) %>% 
    unique()

### ID locations that haven't been IDed
Geokeys.noID <- filter(Geokeys_matched, is.na(already.ID))

#### 2) Subset countries present in Geo.Pop data from geokeys (also drop cases where all subnational na) ####
Geokeys_tojoin <- filter(Geo_keys_clean, c(loc_id %in% Geokeys.noID$loc_id)) %>% 
    mutate(country.isocode = countrycode(country.isocode, "iso2c", "iso3c")) %>% 
    select(-country) %>%
    filter(country.isocode %in% unique(Geo.Pop$iso3)) %>% 
    filter(!is.na(geo_l3)) %>% 
    # Remove Canada if only province
    filter(country.isocode != "CAN" | (country.isocode == "CAN" & !is.na(geo_l4))) %>% 
    # Remove US if only state
    filter(country.isocode != "USA" | (country.isocode == "USA" & (geo_l3 == "DISTRICT OF COLUMBIA" | !is.na(geo_l4))))

#### Get long version of this data for easy matching
Geokeys.tojoin_long <- select(Geokeys_tojoin, -c(geol3_id:geol7_id)) %>% 
    pivot_longer(geo_l3:geo_l7, names_to = "sd.level", values_to = "sd.name")

#### Get frequency table of total locations to map
Geokeys_freqtab <- table(Geokeys_tojoin$country.isocode)

#### 3) ID exact matches ####
### Get just IDs from Geo Pop data
GeoPop_ID <- st_drop_geometry(Geo.Pop) %>% 
    filter(c(iso3 %in% names(Geokeys_freqtab))) %>%
    select(-pop) %>% 
    ## Remove punctuation from subdivision names
    mutate(across(c(macr_nm, micr_nm), ~str_replace_all(., "&", "AND") %>% 
                      str_replace_all("-|–|_", " ") %>% 
                      str_remove_all("[[:punct:]]") %>% 
                      str_remove_all("[[:symbol:]]") %>% 
                      stri_trans_general(id = "Latin-ASCII") %>%
                      str_squish()))

#### Tabulate which countries have 2 levels in the Geo Pop data & which only 1
## >1 unique value in macr_nm or macr_cd !numeric
GeoPopID_list <- GeoPop_ID %>% 
    # The level-1 divisions for spain are wrong -> drop them
    mutate(macr_nm = ifelse(iso3 == "ESP", NA, macr_nm)) %>%
    split(., .$iso3) %>% 
    lapply(function(x) {
        n.macr_nm <- length(unique(x$macr_nm))
        
        mutate(x, geo_lvls = ifelse(n.macr_nm  > 1 | str_detect(macr_cd, "[:alpha:]"), 2, 1))
    })
# Get table
Geolvl_table <- lapply(GeoPopID_list, function(x) {
    unique(x$geo_lvls)
}) %>% bind_rows(.id = "iso3") %>% t() %>% as.data.frame() %>% rownames_to_column("iso3")
names(Geolvl_table)[2] <- "geo_lvls"

##### Find exact matches- Match loc_ids in the Geokeys to geoid from the Geo-Pop data
## Only 1 level of geographic granularity
Matched.keys_l1 <- mapply(function(Geokey, GeoPop) {
    # Filter out what I don't need from GeoPop
    GeoPop_clean <- select(GeoPop, c(geoid:iso3, micr_nm))
    
    # Join gisaid geokeys w/ GeoPop
    joined.data <- left_join(Geokey, GeoPop_clean, by = c("country.isocode" = "iso3", "sd.name" = "micr_nm")) %>% 
        filter(!is.na(geoid)) %>%
        select(loc_id, sd.level, geoid) %>% 
        group_by(loc_id) %>% 
        slice(1) %>% 
        ungroup() %>% 
        select(-sd.level)
    
    return(joined.data)
}, Geokey = split(Geokeys.tojoin_long, Geokeys.tojoin_long$country.isocode)[Geolvl_table$geo_lvls == 1 | grepl("CZE|PHL|THA", Geolvl_table$iso3)], 
GeoPop = GeoPopID_list[Geolvl_table$geo_lvls == 1 | grepl("CZE|PHL|THA", Geolvl_table$iso3)], SIMPLIFY = F) %>% 
    bind_rows()

#### Match countries w/ 2 levels of geographic granularity (ITA, NLD, USA)
Matched.keys_l2 <- mapply(function(Geokey, GeoPop) {
    # Filter out what I don't need from GeoPop
    GeoPop_clean <- select(GeoPop, c(geoid:iso3, macr_nm, micr_nm))
    
    ## For cases where 2 subdivisions are present
    # - Pivot geokeys to separate out subdivision level 1 & clean
    Geokey_clean_l2 <- select(Geokey, -c(geol3_id:geol7_id)) %>% 
        filter(!is.na(geo_l4)) %>%
        pivot_longer(geo_l4:geo_l7, names_to = "sd.level", values_to = "sd.name") %>% 
        mutate(sd.name = str_remove_all(sd.name, "COUNTY") %>% 
                   str_squish())
    # Join gisaid geokeys w/ GeoPop
    joined.data_l2 <- left_join(Geokey_clean_l2, GeoPop_clean, 
                                by = c("country.isocode" = "iso3", "geo_l3" = "macr_nm", "sd.name" = "micr_nm")) %>% 
        filter(!is.na(geoid)) %>%
        select(loc_id, sd.level, geoid) %>% 
        group_by(loc_id) %>% 
        slice(1) %>% 
        ungroup() %>% 
        select(-sd.level)
    
    ## For cases where only 1 level subdivision present
    # Identify & clean cases
    Geokey_clean_l1 <- select(Geokey, -c(geol3_id:geol7_id)) %>% 
        filter(is.na(geo_l4)) %>%
        pivot_longer(geo_l3:geo_l7, names_to = "sd.level", values_to = "sd.name") %>% 
        mutate(sd.name = str_remove_all(sd.name, "COUNTY") %>% 
                   str_squish())
    # Join gisaid geokeys w/ GeoPop (by micro only)
    joined.data_l1 <- left_join(Geokey_clean_l1, GeoPop_clean, 
                                by = c("country.isocode" = "iso3", "sd.name" = "micr_nm")) %>% 
        filter(!is.na(geoid)) %>%
        select(loc_id, sd.level, geoid) %>% 
        group_by(loc_id) %>% 
        slice(1) %>% 
        ungroup() %>% 
        select(-sd.level)
    
    ## Return results
    return(bind_rows(joined.data_l1, joined.data_l2))
},  Geokey = split(Geokeys_tojoin, Geokeys_tojoin$country.isocode)[grepl("ITA|NLD|USA", Geolvl_table$iso3)], 
GeoPop = GeoPopID_list[grepl("ITA|NLD|USA", Geolvl_table$iso3)], SIMPLIFY = F) %>% 
    bind_rows()
### Add back to Geokeys
Geokeys_tojoin <- bind_rows(Matched.keys_l1, Matched.keys_l2) %>% 
    left_join(Geokeys_tojoin, .)

#### 4) Geocode remaining unmatched ####
#### Concatenate locations of remaining umatched for geocoding
To.geocode <- filter(Geokeys_tojoin, is.na(geoid)) %>% 
    select(loc_id, country.isocode:geo_l7) %>% 
    mutate(country = countrycode(country.isocode, "iso3c", "country.name")) %>% 
    unite(location_to_geocode, geo_l7, geo_l6, geo_l5, geo_l4, geo_l3, 
          country, sep = ", ", na.rm = T)

#### Run script to geocode using google maps api
source("gmaps_api_geocode.R")

### Get centroid coordinates of geocoded locations
Geocoded.coords <- mp_get_points(Geocoded)

### Get administrative division level of geocoded locations
To.geocode <- mutate(To.geocode, admin_lvl = sapply(Geocoded, as.character) %>% 
                         str_extract("<type>\\s*(.*?)\\s*</type>") %>% 
                         str_remove_all("<type>") %>% str_remove_all("</type>"))

### Get the admin level that each country uses in the geo data
To.geocode <- c("AUS" = 1, "BEL" = 3, "BRA" = 1, "CAN" = 2, "CHN" = 1, "CZE" = 2, "DEU" = 3, "DNK" = 4, "DZA" = 1, "ESP" = 2, "GBR" = 3, "IND" = 1, "ITA" = 2, 
                "JPN" = 1, "MYS" = 1, "NLD" = 2, "PHL" = 2, "SAU" = 1, "SWE" = 1, "THA" = 1, "TWN" = 2, "USA" = 2, "VEN" = 1, "VNM" = 1, "ZAF" = 1, "ZWE" = 1) %>% 
    tibble(country.isocode = names(.), geo_lvl = .) %>% 
    left_join(To.geocode, .)

### Get numeric value of admin division level
To.geocode <- mutate(To.geocode, 
                     lvl_num = case_when(grepl(1, admin_lvl) ~ 1, 
                                         grepl(2, admin_lvl) ~ 2, 
                                         grepl(3, admin_lvl) ~ 3, 
                                         grepl(4, admin_lvl) ~ 4, 
                                         grepl(c("airport|archipelago|bar|cafe|city_hall|doctor|establishment|locality|neighborhood|political|postal|route|street"), admin_lvl) ~ 99, 
                                         grepl("country", admin_lvl) ~ 0, 
                                         grepl("colloquial", admin_lvl) & (grepl("BEL|ITA|TWN|USA", country.isocode) | grepl("HOKURIKUSHINSYU", location_to_geocode)) ~ geo_lvl, 
                                         grepl("colloquial", admin_lvl) & grepl("CZE|JPN", country.isocode) & !grepl("HOKURIKUSHINSYU", location_to_geocode) ~ 0))

### Get location IDs at too high a spatial resolution to map/no subdivision match found
Geocode_nomatch <- filter(To.geocode, lvl_num < geo_lvl | is.na(lvl_num)) %>% 
    select(loc_id)

##### Add coordinates to geocoded locations
Locations.geocoded <- st_set_geometry(To.geocode, Geocoded.coords$pnt) %>% 
    filter(lvl_num >= geo_lvl)

#### 5) Search for polygons that contain geocoded points ####
#### Obtain matches- polygon contains geocoded centroid coordinates
Geo.matches <- mapply(function(locations, geo) {
    # Get location & geo ids
    locations_clean <- select(locations, loc_id, country.isocode, location_to_geocode)
    geo_clean <- select(geo, geoid, iso3, micr_nm)
    rownames(geo_clean) <- seq(1:nrow(geo_clean))
    
    # Find matches
    matches <- st_contains(geo_clean, locations_clean)
    
    # Join matching geoid's to location id's
    locations_matched <- sapply(t(matches), function(x) {
        st_drop_geometry(geo_clean) %>% 
            select(geoid) %>%
            filter(rownames(.) %in% x)
    }) %>% 
        sapply(function(x) if(identical(x, character(0))) NA_character_ else x) %>% 
        unlist() %>% 
        tibble(locations_clean, geoid = .)
    
    return(locations_matched)
}, locations = split(Locations.geocoded, Locations.geocoded$country.isocode), 
geo = split(Geo.Pop, Geo.Pop$iso3)[grepl(paste(unique(Locations.geocoded$country.isocode), collapse = "|"), 
                                          names(split(Geo.Pop, Geo.Pop$iso3)))], SIMPLIFY = F) %>% 
    bind_rows()
Geo.matches <- select(Geo.matches, loc_id, geoid)

#### Join back in with main geokeys data
Geokeys.tojoin_matchespossible <- filter(Geokeys_tojoin, !c(loc_id %in% Geocode_nomatch$loc_id))

Geokeys.tojoin_matchespossible <- left_join(Geokeys.tojoin_matchespossible, Geo.matches, by = c("loc_id")) %>% 
    unite(geoid, geoid.x, geoid.y, na.rm = T)
Geokeys.tojoin_matchespossible <- mutate(Geokeys.tojoin_matchespossible, geoid = ifelse(geoid == "", NA, geoid))

##### Check if non-matches are w/in 100 meters of a polygon
### Get locations that didn't match
Locations_nomatch <- Locations.geocoded %>% 
    filter(loc_id %in% filter(Geokeys.tojoin_matchespossible, is.na(geoid))$loc_id)

#### Get matches w/in a certain distance
Geo.windist <- mapply(function(locations, geo) {
    # Get location & geo ids
    locations_clean <- select(locations, loc_id, country.isocode, location_to_geocode)
    geo_clean <- select(geo, geoid, iso3, micr_nm)
    rownames(geo_clean) <- seq(1:nrow(geo_clean))
    
    # Find matches
    matches <- st_is_within_distance(geo_clean, locations_clean, dist = 1000)
    
    # Join matching geoid's to location id's
    locations_matched <- sapply(t(matches), function(x) {
        st_drop_geometry(geo_clean) %>% 
            select(geoid) %>%
            filter(rownames(.) %in% x)
    }) %>% 
        sapply(function(x) if(identical(x, character(0))) NA_character_ else x) %>% 
        unlist() %>% 
        tibble(locations_clean, geoid = .)
    
    return(locations_matched)
}, locations = split(Locations_nomatch, Locations_nomatch$country.isocode), 
geo = split(Geo.Pop, Geo.Pop$iso3)[grepl(paste(unique(Locations_nomatch$country.isocode), collapse = "|"), names(split(Geo.Pop, Geo.Pop$iso3)))], 
SIMPLIFY = F) %>% 
    do.call(rbind, .)
Geo.windist <- select(Geo.windist, loc_id, geoid)

#### Join back in with main geokeys data
Geokeys.tojoin_matchespossible <- left_join(Geokeys.tojoin_matchespossible, Geo.windist, by = c("loc_id")) %>% 
    unite(geoid, geoid.x, geoid.y, na.rm = T)
Geokeys.tojoin_matchespossible <- mutate(Geokeys.tojoin_matchespossible, geoid = ifelse(geoid == "", NA, geoid))

#### 6) Export ####
### Assign geoid's to geokeys
Geokeys.ID_updated <- select(Geokeys_matched, -already.ID) %>% 
    left_join(., select(Geokeys.tojoin_matchespossible, loc_id, geoid), by = c("loc_id")) %>% 
    unite(geoid, geoid.x, geoid.y, na.rm = T)
Geokeys.ID_updated <- mutate(Geokeys.ID_updated, geoid = ifelse(geoid == "", NA, geoid))

#### Export
write_csv(Geokeys.ID_updated, '../../../data/raw/geo_keys_ids.csv')
