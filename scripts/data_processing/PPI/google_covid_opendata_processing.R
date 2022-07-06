#### 3) Prepare google data ####
#### Import google index data (names of subnational divisions in google data)
G.index <- read_csv("../../../data/Google_covid_data/index.csv", na = c(""))

### Get google index data w/ all division names in same variable
G.index.clean <- G.index %>% 
    filter(aggregation_level > 0) %>%
    mutate(geo_name = case_when(aggregation_level == 1 ~ subregion1_name, 
                                aggregation_level == 2 ~ subregion2_name, 
                                aggregation_level == 3 ~ locality_name) %>% 
               str_to_upper() %>% 
               str_replace_all("&", "AND") %>% 
               str_replace_all("-|–|_", " ") %>% 
               str_remove_all("[[:punct:]]") %>% 
               str_remove_all("[[:symbol:]]") %>% 
               stri_trans_general(id = "Latin-ASCII") %>%
               str_squish()) %>%
    select(country_code, geo_name, aggregation_level, location_key) 

## Get countries google doesn't have subnational data for
Google.no_subnat <- group_by(G.index, country_code) %>% 
    summarise(max.lvl = max(aggregation_level)) %>% 
    filter(max.lvl == 0) %>% 
    select(country_code)

#### **Geo keys w/ no subnational
table(is.na(Geo_keys_clean$geo_l3))


#### 4) Look for matches at geo level 3 ####
#### Get list of unique geo level 3 's by country (where google has subnational data)
GeoL3 <- select(Geo_keys_clean, country.isocode, geo_l3, geol3_id) %>% 
    filter(!is.na(geol3_id)) %>% 
    filter(!(country.isocode %in% Google.no_subnat$country_code)) %>%
    unique()

#### *** deal w/ locations inside other locations
## ** get centroids from google maps -> see if inside polygon of google subnational division?


##### Try matching w/ geo level 3's by country
Geol3_match <- left_join(GeoL3, G.index.clean, by = c("country.isocode" = "country_code")) %>% 
    mutate(exact_match = ifelse(str_detect(geo_name, geo_l3) | str_detect(geo_l3, geo_name), 
                                T, F)) %>% 
    mutate(dist = stringdist(geo_l3, geo_name, method = "jw")) %>% 
    group_by(geo_l3) %>% 
    mutate(min.dist = min(dist)) %>% 
    ungroup() %>% 
    filter(exact_match == T | dist == min.dist)



##### Check that no matches are ones google doesn't disaggregate
GeoL3 %>% 
    filter(!(geol3_id %in% unique(Geol3_match$geol3_id))) %>% 
    View()


group_by(G.index, country_code, country_name) %>% 
    summarise(max.lvl = max(aggregation_level)) %>% 
    filter(max.lvl > 0) %>% 
    View()


#### What are the locations where min distance == 0 but dont have distance == 0
filter(Geol3_match, dist == 0) %>% 
    select(geol3_id) %>% 
    unique() %>% 
    dim()

filter(Geol3_match, min.dist == 0) %>% 
    select(geol3_id) %>% 
    unique() %>% 
    dim()

#### More than 1 match = use highest level


