#### Processing subregionalcovid19 repo country data ####
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
install_github("sjbeckett/subregionalcovid19")
library("subregionalcovid19")
googledrive::drive_auth(email = T)
library(httr)
library(vietnameseConverter)
devtools::install_github("moodymudskipper/safejoin")
library(safejoin)

#### Load Global regions shapefile
Global_sf <- geomGlobal_simplified

## Run script to load raw data from Germany
source("germany_rawdata.R")
rm(germanyData)
gc()

#### 2) Prepare population data ####
##### Get list of subnational population datasets
## Names of files in repo
Repo_files <- GET("https://api.github.com/repos/sjbeckett/subregionalcovid19/contents/data") %>% 
    content() %>% 
    sapply(function(x) x$name)
## Extract names of countries w/ subnational population data
Popdata.names <- Repo_files[str_detect(Repo_files, "pop|misc_spain|misc_vietnam") & 
                           !str_detect(Repo_files, "pop_smallcountries")] %>% 
    str_remove_all(".rda") %>% 
    str_remove_all("misc_|pop_") %>% 
    str_replace("southafrica", "south africa") %>%
    countrycode(origin = "country.name", destination = "iso3c")

#### Get list of subnational population dataframes
Popdata.list <- list(rename(misc_spain, pop = population2019), 
                     select(misc_vietnam, Name, pop = Population) %>% 
                         mutate(Name = iconv(Name, "latin1", "utf-8") %>% 
                                    str_remove_all(">") %>% 
                                    str_replace_all("<U\\+","\\\\u") %>% 
                                    stri_unescape_unicode() %>% 
                                    stri_trans_general(id = "Latin-ASCII")), 
                     pop_algeria, 
                     rename(pop_australia, name = state), 
                     pop_belgium, 
                     group_by(pop_brazil, state) %>% 
                         summarise(pop = sum(pop2021)) %>% 
                         rename(code = state),
                     filter(pop_canada, !is.na(pop)) %>% 
                         select(code = HR_UID, pop) %>% 
                         mutate(code = as.character(code)), 
                     rename(pop_china, pop = Population_2020), 
                     rename(pop_czechia, name = District), 
                     rename(pop_denmark, name = `...1`, pop = `2020`) %>%
                     mutate(pop = str_remove_all(pop, " ") %>% 
                                as.numeric()) %>% 
                         mutate(name = case_when(name == "Nordfyns" ~ "Nordfyn", 
                                                 name == "Vesthimmerlands" ~ "Vesthimmerland", 
                                                 name != "Nordfyns" & name != "Vesthimmerlands" ~ name)), 
                     select(germanydf, name = Region, pop = Population), 
                     rename(pop_india, name = State) %>% 
                         filter(!is.na(Population) & !c(name == "India")), 
                     pop_italy, 
                     rename(pop_japan, name = Prefecture), 
                     filter(pop_malaysia, idxs != 0) %>% 
                         select(code = idxs, name = state, pop), 
                     select(pop_netherlands, name = Gemeente, pop = Population) %>% 
                         filter(!is.na(pop)), 
                     rename(pop_philippines, name = Location, pop = Pop2015) %>% 
                         filter(c(name %in% Global_sf$micro_name[Global_sf$iso3 == "PHL"])), 
                     select(pop_saudiarabia, name = Emirate, pop = Population), 
                     rename(pop_southafrica, name = X1, pop = X2) %>% 
                         mutate(name = case_when(name == "Northwest" ~ "North West", 
                                                 name != "Northwest" ~ name)), 
                     rename(pop_sweden, name = County), 
                     select(pop_taiwan, name = Chinese.name, English.name, pop = Population.2020), 
                     filter(pop_thailand, !is.na(pop)), 
                     filter(pop_uk, !is.na(pop) & c(code %in% Global_sf$micro_code[Global_sf$iso3 == "GBR"])), 
                     rename(pop_usa, code = fips) %>% 
                         filter(c(code %in% Global_sf$micro_code[Global_sf$iso3 == "USA"])) %>% 
                         mutate(code = as.character(code)),
                     select(pop_venezuela, name = State, pop = Population_2011) %>% 
                         mutate(name = str_replace_all(name, "\\.", " ")), 
                     select(pop_zimbabwe, code = provincepc, name = province, pop = pop_2012))
## Add names
names(Popdata.list) <- Popdata.names

#### Make sure list is cleaned & standardized
Popdata.list <- lapply(Popdata.list, function(x) {
    # colnames lowercase
    names(x) <- str_to_lower(names(x))
    # Population = pop
    names(x)[names(x) == "population"] <- "pop"

    return(x)
})
## Make sure subdivision names upper case & no diacritics
Popdata.list <- map_if(Popdata.list, ~all(c("name") %in% colnames(.)), 
                       ~ mutate(., name = str_to_upper(name) %>% 
                                    stri_trans_general(id = "Latin-ASCII")))

### Add countrycode variable to list of dataframes & order by codes
Popdata.list <- mapply(function(datalist, isocode) {
        tibble(iso3 = isocode, datalist)
    }, datalist = Popdata.list, isocode = Popdata.names)
Popdata.list <- Popdata.list[order(names(Popdata.list))]

#### Create a metadatatable of the list of pop dataframes
Popdata.metadata <- Popdata.list %>% 
    lapply(function(x) {
        # Get population
        population <- sum(x$pop)
        # Get nrows
        n.poprows <- nrow(x)
        # Get whether regions id'ed by name
        name <- "name" %in% names(x)
        # Get whether regions id'ed by code
        code <- "code" %in% names(x)
        
        return(tibble(population, name, code, n.poprows))
    }) %>% 
    bind_rows(.id = "iso3")
    
#### 3) Prepare shapefile ####
#### Get subset of global shapefile of countries where we have subnational pop data from repo
Global_subnat <- filter(Global_sf, iso3 %in% Popdata.names) %>% 
    # Make sure names also in lowercase & latin ascii
    mutate(across(c(macro_name, micro_name), ~str_to_upper(.) %>% 
                      stri_trans_general(id = "Latin-ASCII"))) %>% 
    select(-m49code, -filename) %>% 
    # Fix spelling of chinese provinces
    mutate(micro_name = case_when(micro_name == "NEI MONGOL" ~ "INNER MONGOLIA", 
                                  micro_name == "XIZANG" ~ "TIBET", 
                                  micro_name == "NINGXIA HUI" ~ "NINGXIA", 
                                  micro_name == "XINJIANG UYGUR" ~ "XINJIANG", 
                                  micro_name == "HONGKONG" ~ "HONG KONG", 
                                  micro_name != "NEI MONGOL" & micro_name != "XIZANG" & micro_name != "NINGXIA HUI" & 
                                      micro_name != "XINJIANG UYGUR" & micro_name != "HONGKONG" ~ micro_name)) %>% 
    # Fix 2 venezuelan provinces
    mutate(micro_name = case_when(micro_name == "DEPENDENCIAS FEDERALES" ~ "LOS ROQUES", 
                                  micro_name == "VARGAS" ~ "LA GUAIRA", 
                                  micro_name != "DEPENDENCIAS FEDERALES" & micro_name != "VARGAS" ~ micro_name)) %>% 
    # Fix GAUTENG
    mutate(micro_name = case_when(micro_name == "GUATENG" ~ "GAUTENG", 
                                  micro_name != "GUATENG" ~ micro_name))
    

#### Get nrows & n unique macro/micro names/codes in each country
Subnat.dims <- split(Global_subnat, Global_subnat$iso3) %>% 
    lapply(function(x) {
        n.geomrows <- nrow(x)
        n.macro_names <- unique(x$macro_name) %>% 
            length()
        n.micro_names <- unique(x$micro_name) %>% 
            length()
        n.macro_codes <- unique(x$macro_code) %>% 
            length()
        n.micro_codes <- unique(x$micro_code) %>% 
            length()
       return(tibble(n.geomrows, n.macro_names, n.micro_names, n.macro_codes, n.micro_codes)) 
    }) %>% bind_rows(.id = "iso3")
### Combine this w/ pop metadata to check whether dims the same
Popdata.metadata <- full_join(Popdata.metadata, Subnat.dims) %>% 
    mutate(n.same = n.poprows == n.geomrows) %>% 
    ### Identify columns to join by
    mutate(join_by = case_when(name == T & n.geomrows == n.macro_names ~ "macro_name", 
                               name == T & n.geomrows == n.micro_names ~ "micro_name", 
                               name == F & n.geomrows == n.macro_codes ~ "macro_code", 
                               name == F & n.geomrows == n.micro_codes ~ "micro_code") %>% 
               ifelse(iso3 == "GBR", "micro_code", .))

#### 4) Join population data & shapefile ####
#### Start with countries joining by name
Geo.Pop <- split(Global_subnat, Global_subnat$iso3)[Popdata.metadata$join_by == "micro_name"] %>% 
    mapply(function(Geo, Pop) {
        left_join(Geo, Pop, by = c("iso3", "micro_name" = "name"))
        }, Geo = ., Pop = Popdata.list[Popdata.metadata$join_by == "micro_name"]) %>% 
    map_if(~all(c("code") %in% colnames(.)), 
           ~ select(., -code)) %>% 
    # Taiwan swap chinese -> english names
    map_if(~all(c("english.name") %in% colnames(.)), 
           ~ mutate(., micro_name = english.name) %>% 
               select(-english.name))
#### Countries joining by code
Geo.Pop <-  mapply(function(Geo, Pop) {
    left_join(Geo, Pop, by = c("iso3", "micro_code" = "code"))
    }, Geo = split(Global_subnat, Global_subnat$iso3)[Popdata.metadata$join_by == "micro_code"],
    Pop = Popdata.list[Popdata.metadata$join_by == "micro_code"], SIMPLIFY = F) %>% 
    map_if(~all(c("name") %in% colnames(.)), 
           ~ select(., -name)) %>% 
    c(Geo.Pop) %>% 
    bind_rows()

##### Export combined Geo-Pop data
st_write(Geo.Pop, "../../../data/raw/GeoPop.shp", append = F)
