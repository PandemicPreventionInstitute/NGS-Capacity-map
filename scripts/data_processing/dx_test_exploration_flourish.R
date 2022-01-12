
# Quickly make a Flourish dataframe to look at the two key metrics for test capacity by country. 

rm(list=ls())
FIND_TEST_METRICS<-'../../data/processed/find_test_metrics.csv'
OLD_FIND_MAP_PATH<-url("https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/LMIC%20centered%20map/NGS_flourish_file_11.9.2021_TEST3.csv")
ECONOMY_PATH<-'../../data/static/CLASS.xls'

find_test<-read.csv(FIND_TEST_METRICS)


find_test<-find_test%>%
    mutate(median_daily_tests_per_1000_last_year = round(median_daily_tests_per_1000_last_year, 3),
           tpr_year_raw = round(tpr_year_raw,2),
           tpr_year_smoothed = round(tpr_year_smoothed,2),
           med_tpr_find = round(med_tpr_find,2))


old_find<-read.csv(OLD_FIND_MAP_PATH)%>%select(code, dx_testing_capacity_clean, archetype)%>%
    rename(old_test_archetype = dx_testing_capacity_clean,
           old_archetype = archetype)
write.csv(old_find, '../../data/static/FIND_map_Nov_2021.csv')

find_test<-left_join(find_test, old_find, by = "code")

find_test<-distinct(find_test) # 198

#troubleshoot missing data
n_no_tests<-sum(find_test$tests_in_last_year==0)
n_no_med_tpr_find<-sum(is.na(find_test$med_tpr_find))


find_test<-find_test%>%filter(!is.na(med_tpr_find)) # 167

find_test_med_tpr_missing_but_tests_present<-find_test%>%filter(is.na(med_tpr_find))%>%filter(tests_in_last_year_raw!=0)
write.csv(find_test_med_tpr_missing_but_tests_present, '../../data/processed/countries_w_no_med_tpr_but_tests.csv')
find_test_impossible_tpr<-find_test%>%filter(tpr_year_smoothed>1 | tpr_year_smoothed<0 |tpr_year_raw>1 | tpr_year_raw<0)
write.csv(find_test_impossible_tpr, '../../data/processed/countries_w_implausible_tpr.csv')

#find_test<-find_test%>%filter(tpr_year_raw<=1)%>%filter(tpr_year_raw>=0)
#find_test<-find_test%>%filter(tpr_year_smoothed<=1)%>%filter(tpr_year_smoothed>=0)

find_test<-find_test%>%filter(old_test_archetype != 'High Income*')

n_TEST_OLD<-sum(find_test$old_test_archetype == "Unreliable testing capacity")


write.csv(find_test, '../../data/processed/find_test_scatter.csv')




