dataJapan <- jsonlite::read_json("https://raw.githubusercontent.com/reustle/covid19japan-data/master/docs/summary/latest.json", encoding = "UTF-8")

# Get regions data & name
Japanregions <- dataJapan$regions
names(Japanregions) <- sapply(Japanregions, function(x) {
    x$name
})

# Get names of prefectures in each region
JPNprefectures <- sapply(Japanregions, function(x) {
    unlist(x$prefectures)
})

# Get table linking regions to prefectures
JPN_reg_pref <- tibble(region = names(unlist(JPNprefectures)), prefecture = unlist(JPNprefectures)) %>% 
    mutate(region = str_remove_all(region, "[:digit:]"))
