# NGS Capacity-map 🌍
 
### **Purpose**
As the COVID-19 pandemic continues, extensive research demonstrates the need for genomic sequencing to monitor  infectious disease transmission, outbreak risk, and novel variant detection. Using current COVID-19 and SARS-CoV-2 genomic surveillance data, The Pandemic Prevention Institute (PPI) & FIND partners to construct the Next-Generation Sequencing (NGS) Capacity Map, as a tool to monitor country-level diagnostic testing and sequencing capacity. The NGS Capacity Map integrates various data to highlight global testing and sequencing disparities and suggest evidence-based mitigation strategies. 
 
### **Significance**
Since February 2020, FIND & PPI has maintain NGS Capacity map and continues to update underlying data on a monthly basis. In February 2022, PPI updated the NGS Capacity Map methodologies by using a combination of reliable data sources, robust methodologies, and recent data (2021-2022). The updated NGS Capacity data and recommendations are able to withstand the test of time and precisely monitor the current state of COVID-19 diagnostic testing and SARS-CoV-2 genomic surveillance in resource-limited settings.  The NGS Capacity Map is a useful tool for decision-makers to conceptualize and elucidate LMICs COVID-19 diagnostic testing capacity, their access to NGS sequencing facilities, and SARS-CoV-2 sequencing capacity in an effective and efficient manner. The updated NGS Capacity data and action-oriented recommendations are able to withstand the test of time and precisely monitor the current state of COVID-19 diagnostic testing and SARS-CoV-2 genomic surveillance in resource-limited settings.


### **Methods**
To get started with the NGS Capacity project, review the NGS methodologies. A deep-dive of the NGS Capacity map methodologies can be found under the ```methods``` repository. A more conciser version of the methodologies can be found at the following links:
<br> [Overview of NGS Capacity concepts, data, methods, and metrics](https://public.flourish.studio/visualisation/8647680/)
<br> [Classification Scheme](https://public.flourish.studio/visualisation/8604436/)

#### Data
The NGS Capacity project integrated COVID-19 case data from [FIND github](https://github.com/dsbbfinddx/FINDCov19TrackerData/blob/master/processed/data_all.csv) and [Our World in Data (OWID) github](https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv). Hosted in the ```data``` repo are four nested repos. The ```FIND_partner_support_maps``` contain data used to support the assessment of FIND-funded international labs. The ```Geospatial_Data``` repo consists of various geospatial data, such as geometric polygons, latitude/longitude, and iso3 codes for US states and international countries. The ```NGS_EDA``` contains data necessary for the exploratory data analysis phase. The```NGS_Data_Tables``` compromises of NGS Capacity data, both ```full_dataset``` and a ```clean_dataset```, with associated codebooks. Additionally, ```NGS_Data_Tables``` includes integrated static facility access data from WHO. 

<br>If interested in forking or assessing underlying datasets, you have two options 1) use the short version of the NGS data containing 15 key variables under the ```data --> NGS_Data_Tables --> Month, Year --> clean_dataset```, or 2) use the full underlying dataset, containing all 25 metrics used in calculation and formation of new variables under ```data --> NGS_Data_Tables --> Month, Year --> full_dataset```. Associated codebooks per dataset can be found in the same repositories the datasets are hosted under.

#### Scripts
Our github also hosts further information regarding the NGS exploratory data analysis at ```scripts --> data_analysis --> NGS_v1_redefine_archetypes.ipynb```. The exploratory data analysis python script walks users through the thorough methodologies, from study population inclusion/exclusion criteria, key metric selection, summary statistics exploration, and threshold values determined to define "sufficient diagnostic testing capacity" and "sufficient SARS-CoV-2 sequencing capacity". Additionally, a script deomonstrating the calculation of key metrics from raw data can be found at ```scripts --> data_processing --> get_test_and_ngs_metrics_by_country.R```

### **Resources:**
If you are intersted in learning more, please follow the associated links.
- FIND & PPI NGS Capacity map project, please visit the [PPI & FIND webpage](https://www.finddx.org/sequencing/ngs-capacity-mapping/)
- Next-Generation Sequencing, visit the [Illumina website](https://www.illumina.com/science/technology/next-generation-sequencing.html)
- The Pandemic Prevention Institute, visit the [PPI website](https://www.rockefellerfoundation.org/pandemicpreventioninstitute/)
- FIND, visit the [FIND website](https://www.finddx.org/about/)


