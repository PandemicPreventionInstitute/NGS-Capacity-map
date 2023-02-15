# Description of pipeline to update NGS Capacity map (static outputs)

###  Update GISAID metadata
Manually upload the metadata.csv to the `data/raw/` folder. 

### `PPI/gisaid_metadata_processing.R` 
Imports `data/raw/metadata.csv` and exports `data/processed/gisaid_owid_merged.csv`

### `public/get_test_and_ngs_metrics_by_country.R` 
Imports `data/processed/gisaid_owid_merged.csv`, [FIND testing data](https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv), [data/additiona-sources/WHO_regional_data.csv] (https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/WHO_region_data.csv), [data/additional_sources/WB_class_data.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/WB_class_data.csv), [data/Geospatial_Data/geometric_polygons_country.txt](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/Geospatial_Data/geometric_polygons_country.txt), [data/Geospatial_Data/geography.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/Geospatial_Data/geography.csv), [FIND investment data](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/FIND_partner_support_maps/FIND_partner_maps_May2022.csv), and [sequencing facility data](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/Sequencing_labs_data.csv). 

Outputs all files needed for static maps and scatter plots into `data/NGS_Data_Tables/Month_Year/`.


# Description of scripts used to generate NGS timeseries data (run after static outputs)

### `public/gisaid_worldbank_dictionary.R` (run first)
Imports `data/raw/metadata.csv`, <https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv>, and [data/additional_sources/WB_class_data.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/WB_class_data.csv). 

This outputs a table of all countries present in the FIND testing data and GISAID sequencing data, their country codes, and their world bank income level: [data/NGS_Data_Tables/Timeseries/gisaid_countries.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/gisaid_countries.csv).

### `public/gisaid_sequence_counter.R` (run second)

Imports `data/raw/metadata.csv`. For each date for each country in the GISAID data, calculates the number of sequences that have been submitted by that date that were collected in the previous 30 days.

Outputs [data/NGS_Data_Tables/Timeseries/countries_seqs.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/countries_seqs.csv).

## The following scripts produce the results used in the NGS timeseries visualizations and can be run in any order.

### `public/cumulative_sequences_by_incomegroup.R`

Imports `data/raw/metadata.csv` and [data/NGS_Data_Tables/Timeseries/gisaid_countries.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/gisaid_countries.csv).

Outputs [data/NGS_Data_Tables/Timeseries/sequences_by_month.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/sequences_by_month.csv) and [data/NGS_Data_Tables/Timeseries/cumulative_sequences.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/cumulative_sequences.csv) which are used to generate the [Sequences Submission Timeseries flourish story](https://public.flourish.studio/story/1664349/).

### `public/metrics_by_income_group_timeseries.R`

Imports [data/NGS_Data_Tables/Timeseries/gisaid_countries.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/gisaid_countries.csv), <https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv>, <https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv> and [data/NGS_Data_Tables/Timeseries/countries_seqs.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/countries_seqs.csv).

Outputs [data/NGS_Data_Tables/Timeseries/metrics_timeseries.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/metrics_timeseries.csv), which is used to generate the [Metrics Timeseries flourish chart](https://public.flourish.studio/visualisation/10679226/).

### `public/archetypes_over_time.R`

Imports `data/raw/metadata.csv`, [data/NGS_Data_Tables/Timeseries/gisaid_countries.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/gisaid_countries.csv), <https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv>, <https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv> and [data/additional_sources/Sequencing_labs_data.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/additional_sources/Sequencing_labs_data.csv).

Outputs [data/NGS_Data_Tables/Timeseries/sequencing_archetypes.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/sequencing_archetypes.csv), [data/NGS_Data_Tables/Timeseries/testing_archetypes.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/testing_archetypes.csv), [data/NGS_Data_Tables/Timeseries/sequencing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/sequencing_archetypes_pop.csv), [data/NGS_Data_Tables/Timeseries/testing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/testing_archetypes_pop.csv), [data/NGS_Data_Tables/Timeseries/sequencing_archetypes_byincome.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/sequencing_archetypes_byincome.csv), and [data/NGS_Data_Tables/Timeseries/testing_archetypes_byincome.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/testing_archetypes_byincome.csv). [data/NGS_Data_Tables/Timeseries/sequencing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/sequencing_archetypes_pop.csv) and [data/NGS_Data_Tables/Timeseries/testing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/testing_archetypes_pop.csv) are used to generate the [Archetypes Over Time flourish story](https://public.flourish.studio/story/1664428/), [data/NGS_Data_Tables/Timeseries/sequencing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/sequencing_archetypes_pop.csv) and [data/NGS_Data_Tables/Timeseries/sequencing_archetypes_byincome.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/sequencing_archetypes_byincome.csv) are used to generate the [NGS Archetypes over time flourish story](https://public.flourish.studio/story/1665386/), and [data/NGS_Data_Tables/Timeseries/testing_archetypes_pop.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/testing_archetypes_pop.csv) and [data/NGS_Data_Tables/Timeseries/testing_archetypes_byincome.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/testing_archetypes_byincome.csv) are used to generate the [Testing Archetypes over time flourish story](https://public.flourish.studio/story/1665411/). [data/NGS_Data_Tables/Timeseries/sequencing_archetypes.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/sequencing_archetypes.csv) and [data/NGS_Data_Tables/Timeseries/testing_archetypes.csv](https://raw.githubusercontent.com/PandemicPreventionInstitute/NGS-Capacity-map/main/data/NGS_Data_Tables/Timeseries/testing_archetypes.csv) are used to generate the [NGS Archetypes](https://public.flourish.studio/visualisation/10694441/) and [Testing Archetypes](https://public.flourish.studio/visualisation/10694500/) flourish charts, respectively.

