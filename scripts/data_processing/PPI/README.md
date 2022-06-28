# Description of subnational NGS capacity mapping scripts by Jordan Klein

## GISAID subnational location data processing scripts
Relevant for all subnational NGS capacity mapping, not just 26-country MVP.

### auto_extract_gisaid_metadata.R
Extracts and stores all sequencing metadata from the gisaaid api.

### gisaid_metadata_geokeys_processing.R
Runs auto_extract_gisaid_metadata.R and saves a key of the unique locations contained in the gisaid metadata- geo_keys.csv.

### geokeys_cleaning.R
Imports, cleans, and standardizes geo_keys.csv.

### gmaps_api_geocode.R
Geocodes unmatched/not already-geocoded geokeys. To be run within other scripts which give it an object To.geocode. The output is an object Geocoded.

## Scripts for MVP
These scripts are specific for building the 26-country MVP. These 26 countries are those that sjbeckett/subregionalcovid19 has subnational population data for.

### subregionalcovid19_processing.R
Processes data from the sjbeckett/subregionalcovid19 github repo and creates a shapefile with populations of subnational divisions for 26 MVP countries- GeoPop.shp.

### germany_rawdata.R
This script is only run within subregionalcovid19_processing.R, as the data for germany (one of the 26 MVP countries) are not processed properly without it.

### geokey_popgeo_join.R
This script was used for the 1st time the cleaned geo_keys from running geokeys_cleaning.R were matched with GeoPop.shp. Creates first iteration of geo_keys_ids.csv file, which assigns the geokeys from geo_keys.csv an ID associated with a subnational polygon with population data from GeoPop.shp. Not part of ongoing workflow, see geokey_id_update.R for updating the geo_keys_ids.csv file as more data from gisaid comes in.

### geokey_id_update
Pulls in existing geo_keys_ids.csv file and updates if it there are new geokeys in the latest gisaid pull.

### ngs_subnational_capacity_data.R
Runs gisaid_metadata_geokeys_processing.R and geokey_id_update.R and imports gisaid metadata.csv, GeoPop.shp, and geo_keys_ids.csv to create an up to date geospatially explicit datafile of subnational NGS capacity- NGS_subnational_capacity.csv.

### subnational_mapping.R
Used to generate the maps for the MVP (does not save the files, this was done manually).

## Miscellaneous scripts
These scripts are not used in current workflows.

### gpw_processing.R
Processes raw gridded population of the world data from https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-admin-unit-center-points-population-estimates-rev11/gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv.zip.

### google_covid_opendata_processing.R
This script processes data from the Google Covid-19 Open Data repo- https://health.google.com/covid-19/open-data/.

