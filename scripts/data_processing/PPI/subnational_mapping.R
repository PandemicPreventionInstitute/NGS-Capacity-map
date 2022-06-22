#### Produce MVP of subnational NGS capacity mapping ####
#### Jordan Klein

#### 1) Setup ####
#### Run gisaid metadata extraction & processing script
source("gisaid_metadata_geokeys_processing.R")

### Clear memory & unneeded objects
rm(Metadata_raw)
rm(Locations_split)
gc()

#### Load packages
library("devtools")
library("subregionalcovid19")

#### Read in joined Pop-Geo file & IDed unique geokeys in metadata
Geo.Pop <- st_read("../../../data/raw/GeoPop.shp")
Geokeys_IDed <- read_csv('../../../data/raw/geo_keys_ids.csv')

##### **Here I need to match the geokeys I've already IDed with those from the latest gisaid pull**
# This is to avoid running & re-geocoding all the geokeys so that the IDed geokeys = the ones from the latest pull
# Future workaround = only geocode new geokeys that I can't match to the geo-pop data
Geokeys_matched <- select(Geokeys_IDed, -loc_id) %>% 
    left_join(Geo_keys, .)
   
#### 2) Clean data ####
### Houston city -> Harris county's geoid 
Geokeys_matched <- mutate(Geokeys_matched, geoid = ifelse(geo_l3 == "Texas" & geo_l4 == "Houston", "USA840_TX_48201", geoid)) %>% 
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
    left_join(Geokeys_matched) %>% 
    filter(!is.na(geoid)) %>% 
    group_by(geoid) %>% 
    summarise(seqs = sum(seqs)) %>% 
    left_join(Geo.Pop, .) %>% 
    mutate(seq_pc = 100000*seqs/pop)

Seq_tab <- filter(Metadata, submission_date >= today()-365) %>%
    select(loc_id) %>% 
    group_by(loc_id) %>% 
    summarise(seqs = n()) %>% 
    left_join(Geokeys_matched) %>% 
    filter(!is.na(geoid)) %>% 
    group_by(geoid) %>% 
    summarise(seqs = sum(seqs)) %>% 
    left_join(Geo.Pop, .) %>% 
    mutate(seq_pc = 100000*seqs/pop)

#### 3) Print & check all the data for each country to see if anything looks weird ####
filter(NGS.map, iso3 == "USA") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "CAN") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "BRA") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "VEN") %>% 
    .[order(.$seqs, decreasing = T), ]

filter(NGS.map, iso3 == "ESP") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "GBR") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "BEL") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "NLD") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "DEU") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "ITA") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "DNK") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "SWE") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "CZE") %>% 
    .[order(.$seqs, decreasing = T), ]

filter(NGS.map, iso3 == "DZA") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "ZAF") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "ZWE") %>% 
    .[order(.$seqs, decreasing = T), ]

filter(NGS.map, iso3 == "SAU") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "IND") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "CHN") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "THA") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "VNM") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "MYS") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "PHL") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "TWN") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "JPN") %>% 
    .[order(.$seqs, decreasing = T), ]
filter(NGS.map, iso3 == "AUS") %>% 
    .[order(.$seqs, decreasing = T), ]

#### 4) Produce plots ####
filter(NGS.map, iso3 == "USA" & !grepl("MARIANA|ISLAND|HAWAII|ALASKA|SAMOA", macr_nm)) %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + coord_sf(xlim = c(-130, -70), ylim = c(15, 50)) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000, 10000))
filter(NGS.map, iso3 == "CAN") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000))
filter(NGS.map, iso3 == "BRA") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000))
filter(NGS.map, iso3 == "VEN") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000), limits = c(.1, 20))

filter(NGS.map, iso3 == "ESP") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000))
filter(NGS.map, iso3 == "GBR") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000))
filter(NGS.map, iso3 == "BEL") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000, 10000))
filter(NGS.map, iso3 == "NLD") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000, 10000))
filter(NGS.map, iso3 == "DEU") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000, 10000))
filter(NGS.map, iso3 == "ITA") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000))
filter(NGS.map, iso3 == "DNK") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000))
filter(NGS.map, iso3 == "SWE") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000, 10000), limits = c(80, 3000))
filter(NGS.map, iso3 == "CZE") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000))

filter(NGS.map, iso3 == "DZA") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000), limits = c(.01, 12))
filter(NGS.map, iso3 == "ZAF") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000, 10000), limits = c(5, 150))
filter(NGS.map, iso3 == "ZWE") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000), limits = c(.01, 12))

filter(NGS.map, iso3 == "SAU") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000), limits = c(.1, 12))
filter(NGS.map, iso3 == "IND") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000))
filter(NGS.map, iso3 == "CHN") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(.1, 1, 10, 100, 1000))
filter(NGS.map, iso3 == "THA") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000))
filter(NGS.map, iso3 == "VNM") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000))
filter(NGS.map, iso3 == "MYS") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000), limits = c(5, 200))
filter(NGS.map, iso3 == "PHL") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000), limits = c(8, 120))
filter(NGS.map, iso3 == "TWN") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(.1, 1, 10, 100, 1000))
filter(NGS.map, iso3 == "JPN") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000), limits = c(8, 1200))
filter(NGS.map, iso3 == "AUS") %>% 
    ggplot() + geom_sf(aes(fill = seq_pc), color = "white", size = .01) + 
    scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12"), trans = "log", 
                        name = "Sequences submitted per 100,000 in the past 12 months", breaks = c(1, 10, 100, 1000), limits = c(80, 1500))
