## G2F RESOURCES


This repository provides resources (data and pipelines) linked to data generated by the GxE initiative of the [Genomes to Fields](https://www.genomes2fields.org/) project.

### Daily weather data

|Source |Output-file|Inputs|Script|Contact|
|---------|------|------|------|--------|
| G2F-GxE data|[G2Fdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/G2Fdaily.csv)|[repository](https://github.com/QuantGen/G2F_RESOURCES/tree/main/Data/EnvironmentalCovariates)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/G2FWeatherData.md)| matiasfe@msu.edu |
| Consensus |  [wdaily_final.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/wdaily_final.csv)  | [G2Fdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/G2Fdaily.csv) ; [NASAdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/NASAdaily.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/ExamineEnvData.md)  |  matiasfe@msu.edu |

* The Consensus weather data combine the best quality data of G2F and NASA weather data. Guides on how to obtain information from these weather networks are in folder "mdfiles".

### Soil data

|Source |Output-file|Inputs|Script|Contact|
|---------|------|------|------|--------|
| USDA |[SoilData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/SoilData.csv) | [info_loc.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/soil_data_from_USDA.md) | jim.holland@usda.gov |

### Growing degree days (GDD) and predicted flowering date (FD) per year-location

|What? |Output-file|Inputs|Script|Contact|
|---------|------|------|------|--------|
| GDD & FDD   | ? | [wdaily_final.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/wdaily_final.csv)  | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/GDDPredictFlowering.md)   | matiasfe@msu.edu |

## Phenotypes

|What?|Output-file|Inputs|Script|Contact|
|---------|------|------|------|--------|
|Fortmatted phenotypes |[phenotype.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv)|[repository](https://github.com/QuantGen/G2F_RESOURCES/tree/main/Data/Phenotypes) ; [G2F site](https://www.genomes2fields.org/resources/)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/phenotypes.md)|matiasfe@msu.edu|

## Analysis pipelines

<!-- 
 - [Tools to examine weather data](https://github.com/QuantGen/G2F_RESOURCES/blob/main/ExamineEnvData.md)
       - Consensus data: [wdaily_final.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/wdaily_final.csv)
  - [Calculate growing degree days (GDD) and predict flowering date](https://github.com/QuantGen/G2F_RESOURCES/blob/main/GDDPredictFlowering.md)

 - [Baseline model with lme4]()
 - [Genomic relationships and DNA-derived PCs]()
 - [Genomic Regession using BGLR]()
 - [...]
-->
 
