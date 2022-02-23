
# Quickly make a Flourish dataframe to look at the two key metrics for test capacity by country. 

rm(list=ls())
FIND_TEST_METRICS<-'../../data/processed/find_test_metrics.csv'
OLD_FIND_MAP_PATH<-url("https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/LMIC%20centered%20map/NGS_flourish_file_11.9.2021_TEST3.csv")
ECONOMY_PATH<-'../../data/static/CLASS.xls'

find_test<-read.csv(FIND_TEST_METRICS)

# round for Flourish
find_test<-find_test%>%
    mutate(avg_daily_tests_per_1000_last_year_smoothed = round(avg_daily_tests_per_1000_last_year_smoothed, 3),
           tpr_year_raw = round(tpr_year_raw,3),
           tpr_year_smoothed = round(tpr_year_smoothed,3),
           tpr_year_smoothed_truncated = round(tpr_year_smoothed_truncated,3),
           avg_tpr_find = round(avg_tpr_find,3))


old_find<-read.csv(OLD_FIND_MAP_PATH)%>%select(code, country, dx_testing_capacity_clean, archetype)%>%
    rename(old_test_archetype = dx_testing_capacity_clean,
           old_archetype = archetype)
write.csv(old_find, '../../data/static/FIND_map_Nov_2021.csv')

# countries in old dataset not in FIND
codes_FIND<-find_test$code
left_out_FIND<- old_find%>%filter(!(code %in% codes_FIND))

write.csv(left_out_FIND, '../../data/processed/countries_not_in_FIND_data.csv')

# countries in FIND not in old dataset
codes_old<-unique(old_find$code)
left_out_old<-find_test%>%filter(!(code %in% codes_old))

find_test<-left_join(find_test, old_find, by = c("code"))%>%
    select(code,country, avg_tpr_find, avg_daily_tests_per_1000_last_year_smoothed, avg_daily_test_per_1000_last_year_raw,
           tpr_year_raw, tpr_year_smoothed, tpr_year_smoothed_truncated, tests_in_last_year_raw, tests_in_last_year_smoothed, 
           cases_in_last_year_raw, cases_in_last_year_smoothed, cases_in_last_year_smoothed_truncated,
           old_test_archetype, old_archetype
)%>%filter(!is.na(code))%>%filter(country != "West Bank and Gaza")

n_countries<-length(unique(find_test$code)) # 197


#find_test<-find_test%>%filter(old_test_archetype != 'High Income*')

#troubleshoot missing data
n_no_tests<-sum(find_test$tests_in_last_year_smoothed==0 | is.na(find_test$tests_in_last_year_smoothed))
n_no_avg_tpr_find<-sum(is.na(find_test$avg_tpr_find))




find_test_avg_tpr_missing<-find_test%>%filter(is.na(avg_tpr_find))
write.csv(find_test_avg_tpr_missing, '../../data/processed/countries_w_no_avg_tpr.csv')

find_test<-find_test%>%filter(!is.na(avg_tpr_find)) # 167
find_test_impossible_tpr<-find_test%>%filter(tpr_year_smoothed_truncated>1 | tpr_year_smoothed_truncated<0)
write.csv(find_test_impossible_tpr, '../../data/processed/countries_w_implausible_tpr.csv')


n_HIC<-sum(find_test$old_archetype == 'High Income*')

n_TEST_OLD<-sum(find_test$old_test_archetype == "Unreliable testing capacity")

# With HICs
write.csv(find_test, '../../data/processed/find_test_scatter.csv')

n_codes<-length(unique(find_test$code))
# With only LMICS
find_test<-find_test%>%filter(old_test_archetype != 'High Income*')
n_codes_LMICS<-length(unique(find_test$code))
write.csv(find_test, '../../data/processed/find_test_scatter_LMICs.csv')




