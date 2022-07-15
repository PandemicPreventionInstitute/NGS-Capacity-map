# Description of scripts used to create the relational data files for the 26-country subnational population basemap by Jordan Klein

### basemap_mvp.R
Imports GeoPop.shp and outputs files in data/raw/basemap:
    - keys.csv- contains unique identifiers for country (iso3), administrative division level 1 (adm1_id), administrative division level 2 (adm2_id), and the original unique identifiers used in sjbeckett/subregionalcovid19 (geoid).
    - countries.geojson- Country level population and geometry file. Contains unique identifiers for each country (iso3), country name (cntry_nm), population, and geometry.
    - adm1.geojson- Administrative division level 1 population and geometry file. Contains unique identifiers for each country (iso3), admin division level 1 (adm1_id), their names (cntry_nm, adm1_name), population, and geometry.
    - adm2.geojson- Administrative division level 2 population and geometry file. Contains unique identifiers for each country (iso3), admin division level 1 (adm1_id), admin division level 2 (adm2_id), their names (cntry_nm, adm1_name, adm2_name), population, and geometry.

#### japan_rawdata.R and belgium_rawdata.R
These scripts are only run inside basemap_mvp.R to process the data for japan and belgium.