# NGS Capacity-map 🌍
 
### **Purpose**
As the COVID-19 pandemic continues, extensive research demonstrates the need for genomic sequencing to monitor  infectious disease transmission, outbreak risk, and novel variant detection. Using current COVID-19 and SARS-CoV-2 genomic surveillance data, The Pandemic Prevention Institute (PPI) & FIND partners to construct the Next-Generation Sequencing (NGS) Capacity Map, as a tool to monitor country-level diagnostic testing and sequencing capacity. The NGS Capacity Map integrates various data to highlight global testing and sequencing disparities and suggest evidence-based strategies to close the identified gaps. 
 
### **Significance**
Since May 2021, FIND & PPI have maintained the NGS Capacity map and continus to update underlying data on a monthly basis. In February 2022, PPI updated the NGS Capacity Map (version 1.1) by adjusting the data sources, changing the underlying metrics and methodology, and including only the latest data (2021-2022). These updates support a more stable and representative methodology, reflective of the current state of COVID-19 diagnostic testing and SARS-CoV-2 genomic surveillance in Low- and Middle-income countries (LMICs).  We hope the NGS Capacity Map provides decision-makers with a consoolidated, useful, and actionable view of countries' COVID-19 surveillance capacity by combining measures of COVID-19 diagnostic testing capacity, access to NGS sequencing facilities, and SARS-CoV-2 sequencing capacity.


### **Methods**
To get started with the NGS Capacity project, review the NGS methodologies. A deep-dive of the NGS Capacity map methodologies can be found under the ```methods``` repository. A more concise version of the methodologies can be found at the following links:
- [Overview of NGS Capacity concepts, data, methods, and metrics](https://public.flourish.studio/visualisation/8647680/)
- [Classification Scheme](https://public.flourish.studio/visualisation/8604436/)

#### Data
The NGS Capacity project integrated COVID-19 case data from [FIND github](https://github.com/dsbbfinddx/FINDCov19TrackerData/blob/master/processed/data_all.csv) and [Our World in Data (OWID) github](https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv), SARS-CoV-2 RNA genomic sequencing data from [GISAID](gisaid.org), and sequencing labs data from WHO. 

Hosted in the ```data``` repo are four nested repos. The ```FIND_partner_support_maps``` contain data describing the footprint of projects funded by partners of the [Access to COVID-19 Tools Accelerator Diagnostics Pillar (ACT-A Dx) genomic surveillance working group.](https://www.finddx.org/sequencing/mapping-act-a-sequencing-activities/) The ```Geospatial_Data``` repo consists of various geospatial data, such as geometric polygons, latitude/longitude, and iso3 codes per country, world-wide. The ```NGS_EDA``` contains data necessary for the exploratory data analysis phase. 

In the```NGS_Data_Tables``` the ```public``` repo compromises of NGS Capacity data, both ```full_dataset``` and a ```clean_dataset```, with associated codebooks. Additionally, ```additional_sources``` repo includes integrated static sequencing facility access data from WHO, World Bank socio-economic data, and WHO regional data. 

<br>If interested in forking or assessing underlying datasets, you have two options 1) use the short version of the NGS data containing 15 key variables under the ```data --> NGS_Data_Tables --> Month, Year --> public --> clean_dataset```, or 2) use the full underlying dataset, containing all 25 metrics used in calculation and formation of new variables under ```data --> NGS_Data_Tables --> Month, Year --> public--> full_dataset```. Associated codebooks per dataset can be found in the same repositories the datasets are hosted under.

#### Scripts
Our Github repository also hosts further information regarding the NGS exploratory data analysis at ```scripts --> data_analysis --> pubic--> NGS_v1_redefine_methods.ipynb```. The exploratory data analysis python script walks users through the thorough methodologies, from study population inclusion/exclusion criteria, key metric selection, summary statistics exploration, and threshold values determined to define "sufficient diagnostic testing capacity" and "sufficient SARS-CoV-2 sequencing capacity". Additionally, a script deomonstrating the calculation of key metrics from raw data can be found at ```scripts --> data_processing --> public--> get_test_and_ngs_metrics_by_country.R```

### **Resources:**
If you are intersted in learning more, please follow the associated links.
- FIND & PPI NGS Capacity map project, please visit the [PPI & FIND webpage](https://www.finddx.org/sequencing/ngs-capacity-mapping/)
- Next-Generation Sequencing, visit the [FIND and PHG Foundation landscape](https://www.finddx.org/wp-content/uploads/2021/05/2021_04_21_NGS-for-sars-cov-2-compr.pdf)
- GISAID global genomi sequencing repository, please visit the [GISAID website](gisaid.org)
- The Pandemic Prevention Institute, visit the [PPI website](https://www.rockefellerfoundation.org/pandemicpreventioninstitute/)
- FIND, visit the [FIND website](https://www.finddx.org/about/)


### **Acknowledgements:**
We are grateful to the data contributors who shared the data used in the NGS Capacity Map via the GISAID Initiative*: the Authors, the Originating Laboratories responsible for obtaining the specimens, and the Submitting Laboratories that generated the genetic sequences and metadata.

*(a) Elbe, S., and Buckland-Merrett, G. (2017) Data, disease and diplomacy: GISAID’s innovative contribution to global health.
Global Challenges, 1:33-46. DOI: [10.1002/gch2.1018](https://onlinelibrary.wiley.com/doi/10.1002/gch2.1018) PMCID: [31565258](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6607375/)

(b) Shu, Y., McCauley, J. (2017) GISAID: From vision to reality.
EuroSurveillance, 22(13). DOI: [10.2807/1560-7917.ES.2017.22.13.30494](https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2017.22.13.30494) PMCID: [PMC5388101](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5388101/)

GISAID data provided via this tool are subject to GISAID [Terms and Conditions](https://www.gisaid.org/DAA).

