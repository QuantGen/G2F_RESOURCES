## G2F RESOURCES


This repository provides resources (data and pipelines) linked to data generated by the GxE initiative of the [Genomes to Fields](https://www.genomes2fields.org/) project.

### Daily weather data

|Source |Output-file|Inputs|Script|Data dictionary|
|---------|------|------|------|--------|
| G2F-GxE data|[G2Fdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/G2Fdaily.csv)|[repository](https://github.com/QuantGen/G2F_RESOURCES/tree/main/Data/EnvironmentalCovariates)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/G2FWeatherData.R)| [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/G2FWeatherData.md) |
| Consensus |  [ConsensusData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/ConsensusData.csv)  | [G2Fdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/G2Fdaily.csv) ; [NASAdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/NASAdaily.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/ConsensusWeather.R)  | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/ExamineEnvData.md) |

The Consensus weather data combines data from G2F with data from NASA. NASA data was used for variables not recorded by G2F and to fill records from G2F that were either missing or appeared to be wrong. For further information about each of these networks please see the corresponding "mdfiles".
* Weather data maintained by Fernando Aguate (matiasfe@msu.edu), Marco Lopez-Cruz (lopezcru@msu.edu), and Gustavo de los Campos (gustavoc@msu.edu)

### Soil data

|Source |Output-file|Inputs|Script|Data dictionary|
|---------|------|------|------|--------|
| USDA |[SoilData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/SoilData.csv) | [info_loc.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv) | [link]() | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/soil_data_from_USDA.md) |
* Weather data maintained by Jim Holland (jim.holland@usda.gov)

### Growing degree days (GDD) and predicted flowering date (FD) per year-location

|What? |Output-file|Inputs|Script|Data dictionary|
|---------|------|------|------|--------|
| GDD & FDD   | ? | [wdaily_final.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/wdaily_final.csv)  | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/GDDPredictFlowering.md)   |  |

## Phenotypes

|What?|Output-file|Inputs|Script|Data dictionary|
|---------|------|------|------|--------|
|Fortmatted phenotypes |[phenotype.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv)|[repository](https://github.com/QuantGen/G2F_RESOURCES/tree/main/Data/Phenotypes) ; [G2F site](https://www.genomes2fields.org/resources/)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/phenotypes.md)| |

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
 
