#### Testing and NGS capacity timeseries- global level plots ####
#### Jordan Klein

#### 1) Setup ####
rm(list = ls())
#### Run scripts to process GISAID metadata & OWID data, & calculate testing & NGS metrics from these + FIND data
source("../PPI/auto_extract_gisaid_metadata.R")
source("../PPI/gisaid_metadata_processing.R")
source("get_test_and_ngs_metrics_by_country.R")

#### 2) Get time series of testing metrics (test positivity & tests per 1,000 per day over 3 & 12 month windows) ####
#### Get simplified time series of testing data I need from find
testing_ts <- select(find_testing_t, date, code, new_cases_smoothed_truncated, new_tests_smoothed, pop) %>% 
  mutate(month = as.yearmon(date))

#### Get dataframe of pop of each country in the testing data
country_pops <- group_by(find_testing_t, code) %>% 
  summarise(pop = max(pop, na.rm = T))

#### Get daily time series of testing metrics on the global level
global_testing <- group_by(testing_ts,date,month) %>% 
  summarise(cases = sum(new_cases_smoothed_truncated, na.rm = T), 
            tests = sum(new_tests_smoothed, na.rm = T)) %>% 
  tibble(., pop = sum(country_pops$pop))

#### Calculate TPR & daily tests per 1,000 over 12 & 3 month moving windows
testing_metrics_ts <- global_testing %>% 
  group_by(month, pop) %>% 
  summarise(cases = sum(cases, na.rm = T), tests = sum(tests, na.rm = T), days = n()) %>% 
  ungroup() %>%
  filter(month < as.yearmon("2022-07-01")) %>% 
  mutate(cases.12mo = rollsum(cases, 12, fill = NA, align = "right"), cases.3mo = rollsum(cases, 3, fill = NA, align = "right"), 
         tests.12mo = rollsum(tests, 12, fill = NA, align = "right"), tests.3mo = rollsum(tests, 3, fill = NA, align = "right"), 
         tpr.12mo = cases.12mo/tests.12mo*100, tpr.3mo = cases.3mo/tests.3mo*100) %>% 
  mutate(daily_tests_per1000.12mo = 1000*(tests.12mo/rollsum(days, 12, fill = NA, align = "right"))/pop, 
         daily_tests_per1000.3mo = 1000*(tests.3mo/rollsum(days, 3, fill = NA, align = "right"))/pop)

#### 3) Get time series of sequencing metrics (percent of cases sequenced & sequences per 100k over 3 & 12 month windows) ####
#### Get dataframe of pop of each country in the sequencing data
country_pops_seq <- filter(gisaid_t, !is.na(owid_population)) %>%
  group_by(country_code) %>% 
  summarise(pop = max(owid_population))

#### Get daily time series of sequencing metrics on the global level
global_sequencing <- filter(gisaid_t, collection_date > ymd("2019-12-31")) %>%
  mutate(month = as.yearmon(collection_date)) %>% 
  group_by(month) %>% 
  summarise(sequences = sum(n_new_sequences), cases = sum(owid_new_cases)) %>% 
  tibble(., pop = sum(country_pops_seq$pop))

#### Calculate % cases sequenced & sequences per 100k over 12 & 3 month moving windows
sequencing_metrics_ts <- global_sequencing %>% 
  group_by(month, pop) %>% 
  summarise(sequences = sum(sequences, na.rm = T), cases = sum(cases, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(sequences.12mo = rollsum(sequences, 12, fill = NA, align = "right"), sequences.3mo = rollsum(sequences, 3, fill = NA, align = "right"),
         cases.12mo = rollsum(cases, 12, fill = NA, align = "right"), cases.3mo = rollsum(cases, 3, fill = NA, align = "right"), 
         pct_seq.12mo = sequences.12mo/cases.12mo*100, pct_seq.3mo = sequences.3mo/cases.3mo*100, 
         seq_per_100k.12mo = 100000*sequences.12mo/pop, seq_per_100k.3mo = 100000*sequences.3mo/(pop/4))

#### 4) Combine sequencing & testing global time series data & save it ####
global_ts_metrics <- select(testing_metrics_ts, month, tpr.12mo:daily_tests_per1000.3mo) %>% 
  left_join(select(sequencing_metrics_ts, month, pct_seq.12mo:seq_per_100k.3mo))

write_csv(global_ts_metrics, '../../../data/processed/global_test_ngs_timeseries.csv')

#### 5) Create plots ####
#### Transform data for plotting
global_tsplot_data <- pivot_longer(global_ts_metrics, tpr.12mo:seq_per_100k.3mo, names_to = "metric", values_to = "value") %>% 
  mutate(timescale = case_when(grepl("12mo", metric) ~ "12 months", 
                               grepl("3mo", metric) ~ "3 months"), 
         metric = str_remove_all(metric, "\\.(?<=\\.).*$"))

#### Plot data
filter(global_tsplot_data, metric == "tpr") %>% 
  ggplot(aes(x = month, y = value, linetype = timescale, color = timescale)) + geom_line() +
  scale_linetype_manual(values = c("12 months" = "solid", "3 months" = "dashed"), name = "Timescale") + 
  scale_color_manual(values = c("12 months" = "darkred", "3 months" = "red"), name = "Timescale") +
  scale_x_yearmon(breaks = as.yearmon(seq.Date(ymd("2020-01-01"), ymd("2022-07-01"), by = "3 months")), name = "Month") + 
  scale_y_continuous(name = "Test positivity rate (%)", limits = c(0, 16.5)) + 
  labs(title = "Global testing and sequencing capacity time series", subtitle = "Test positivity rate")
ggsave("../../../figures/tpr.png", width = 1400, height = 900, units = "px", scale = 3)

filter(global_tsplot_data, metric == "daily_tests_per1000") %>% 
  ggplot(aes(x = month, y = value, linetype = timescale, color = timescale)) + geom_line() +
  scale_linetype_manual(values = c("12 months" = "solid", "3 months" = "dashed"), name = "Timescale") + 
  scale_color_manual(values = c("12 months" = "darkblue", "3 months" = "blue"), name = "Timescale") +
  scale_x_yearmon(breaks = as.yearmon(seq.Date(ymd("2020-01-01"), ymd("2022-07-01"), by = "3 months")), name = "Month") + 
  scale_y_continuous(name = "Tests (per 1,000 people per day)", limits = c(0, 2)) +
  labs(title = "Global testing and sequencing capacity time series", subtitle = "Tests per capita")
ggsave("../../../figures/daily_tests.png", width = 1400, height = 900, units = "px", scale = 3)

filter(global_tsplot_data, metric == "pct_seq") %>% 
  ggplot(aes(x = month, y = value, linetype = timescale, color = timescale)) + geom_line() +
  scale_linetype_manual(values = c("12 months" = "solid", "3 months" = "dashed"), name = "Timescale") + 
  scale_color_manual(values = c("12 months" = "darkgreen", "3 months" = "darkolivegreen4"), name = "Timescale") +
  scale_x_yearmon(breaks = as.yearmon(seq.Date(ymd("2020-01-01"), ymd("2022-07-01"), by = "3 months")), name = "Month") + 
  scale_y_continuous(name = "% of cases sequenced", limits = c(0, 8)) +
  labs(title = "Global testing and sequencing capacity time series", subtitle = "Cases sequenced")
ggsave("../../../figures/pct_seq.png", width = 1400, height = 900, units = "px", scale = 3)

filter(global_tsplot_data, metric == "seq_per_100k") %>% 
  ggplot(aes(x = month, y = value, linetype = timescale, color = timescale)) + geom_line() +
  scale_linetype_manual(values = c("12 months" = "solid", "3 months" = "dashed"), name = "Timescale") + 
  scale_color_manual(values = c("12 months" = "deeppink4", "3 months" = "deeppink"), name = "Timescale") +
  scale_x_yearmon(breaks = as.yearmon(seq.Date(ymd("2020-01-01"), ymd("2022-07-01"), by = "3 months")), name = "Month") + 
  scale_y_continuous(name = "Sequences (per 100,000 person-years)", limits = c(0, 162)) +
  labs(title = "Global testing and sequencing capacity time series", subtitle = "Sequences per capita")
ggsave("../../../figures/seq_percap.png", width = 1400, height = 900, units = "px", scale = 3)

