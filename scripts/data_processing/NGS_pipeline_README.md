# Description of pipeline to update NGS Capacity map (static outputs)

## `PPI/auto_extract_gisaid_metadata.R` 
Imports GISAID credentials and exports `data/raw/metadata.csv`

##`PPI/gisaid_metadata_processing.R` 
Imports `data/raw/metadata.csv` and exports `data/processed/gisaid_owid_merged.csv`

## `public/get_test_and_ngs_metrics_by_country.R` 
Imports `data/processed/gisaid_owid_merged.csv`, [FIND testing data](https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv), [data/additiona-sources/WHO_regional_data.csv] (https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/WHO_region_data.csv), [data/additional_sources/WB_class_data.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/WB_class_data.csv), [data/Geospatial_Data/geometric_polygons_country.txt](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/Geospatial_Data/geometric_polygons_country.txt), [data/Geospatial_Data/geography.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/Geospatial_Data/geography.csv), [FIND investment data](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/FIND_partner_support_maps/FIND_partner_maps_May2022.csv), and [sequencing facility data](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/Sequencing_labs_data.csv). 

Outputs all files needed for static maps and scatter plots into `data/NGS_Data_Tables/Month_Year/`.


# Description of scripts used to generate NGS timeseries data (run after static outputs)

### `public/gisaid_worldbank_dictionary.R`
Imports `data/raw/metadata.csv`, <https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv>, and [data/additional_sources/WB_class_data.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/WB_class_data.csv). 

This outputs a table of all countries present in the FIND testing data and GISAID sequencing data, their country codes, and their world bank income level: [data/NGS_DATA_Tables/timeseries/gisaid_countries.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/timeseries/gisaid_countries.csv).

### `public/gisaid_sequence_counter.R` (run second)

Imports `data/raw/metadata.csv`. For each date for each country in the GISAID data, calculates the number of sequences that have been submitted by that date that were collected in the previous 30 days. 

Outputs [data/NGS_Data_Tables/timeseries/countries_seqs.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/timeseries/countries_seqs.csv).

## The following scripts produce the results used in the NGS timeseries visualizations and can be run in any order.

### `public/cumulative_sequences_by_incomegroup.R`

Imports `data/raw/metadata.csv` and [data/NGS_Data_Tables/timeseries/gisaid_countries.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/timeseries/gisaid_countries.csv). 

Outputs [data/NGS_Data_Tables/timeseries/sequences_by_month.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/timeseries/sequences_by_month.csv) and [data/NGS_Data_Tables/timeseries/cumulative_sequences.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/timeseries/cumulative_sequences.csv) which are used to generate the [Sequences Submission Timeseries flourish story](https://public.flourish.studio/story/1664349/).

### `metrics_by_income_group_timeseries.R` # Jordan I got up to here with editing the file paths. Going to leave the rest for you

Imports [gisaid_countries.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/gisaid_countries.csv), <https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv>, <https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv> and [countries_seqs.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/countries_seqs.csv). Outputs [metrics_timeseries.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/metrics_timeseries.csv), which is used to generate the [Metrics Timeseries flourish chart](https://public.flourish.studio/visualisation/10679226/).

### `archetypes_over_time.R`

Imports `data/raw/metadata.csv`, [gisaid_countries.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/gisaid_countries.csv), <https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv>, <https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv> and [data/additional_sources/Sequencing_labs_data.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/Sequencing_labs_data.csv). Outputs [sequencing_archetypes.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/sequencing_archetypes.csv), [testing_archetypes.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/testing_archetypes.csv), [sequencing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/sequencing_archetypes_pop.csv), [testing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/testing_archetypes_pop.csv), [sequencing_archetypes_byincome.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/sequencing_archetypes_byincome.csv), and [testing_archetypes_byincome.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/testing_archetypes_byincome.csv). [sequencing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/sequencing_archetypes_pop.csv) and [testing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/testing_archetypes_pop.csv) are used to generate the [Archetypes Over Time flourish story](https://public.flourish.studio/story/1664428/), [sequencing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/sequencing_archetypes_pop.csv) and [sequencing_archetypes_byincome.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/sequencing_archetypes_byincome.csv) are used to generate the [NGS Archetypes over time flourish story](https://public.flourish.studio/story/1665386/), and [testing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/testing_archetypes_pop.csv) and [testing_archetypes_byincome.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/testing_archetypes_byincome.csv) are used to generate the [Testing Archetypes over time flourish story](https://public.flourish.studio/story/1665411/). [sequencing_archetypes.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/sequencing_archetypes.csv) and [testing_archetypes.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/ppi-output/main/timeseries/testing_archetypes.csv) are used to generate the [NGS Archetypes](https://public.flourish.studio/visualisation/10694441/) and [Testing Archetypes](https://public.flourish.studio/visualisation/10694500/) flourish charts, respectively.
