## G2F RESOURCES


This repository provides resources (data and pipelines) linked to data generated by the GxE initiative of the [Genomes to Fields](https://www.genomes2fields.org/) project.

### Daily weather data

|Source |Output-file|Inputs|Script|Data dictionary|
|---------|------|------|------|--------|
| G2F-GxE data|[G2Fdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/G2Fdaily.csv)|[repository](https://github.com/QuantGen/G2F_RESOURCES/tree/main/Data/EnvironmentalCovariates)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/G2FWeatherData.R)| [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/G2FWeatherData.md) |
| NASA data|[NASAdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/NASAdaily.csv)|[info_loc.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/NASAWeatherData.R)| [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/NASAWeatherData.md) |
| Consensus |  [ConsensusData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/ConsensusData.csv)  | [G2Fdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/G2Fdaily.csv) ; [NASAdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/NASAdaily.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/ConsensusWeather.R)  | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/ExamineEnvData.md) |

The Consensus weather data combines data from G2F with data from NASA. NASA data was used for variables not recorded by G2F and to fill records from G2F that were either missing or appeared to be wrong. For further information about each of these networks please see the corresponding "mdfiles".
* Weather data maintained by Fernando Aguate (matiasfe@msu.edu), Marco Lopez-Cruz (lopezcru@msu.edu), and Gustavo de los Campos (gustavoc@msu.edu)

### Soil data

|Source |Output-file|Inputs|Script|Data dictionary|
|---------|------|------|------|--------|
| G2F |[g2f_soil.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/g2f_soil.csv) | [g2f_2018_soil_data.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/Metadata/g2f_2018_soil_data.csv) ; [g2f_2019_soil_data.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/Metadata/g2f_2019_soil_data.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/g2f_SoilData.R) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/g2f_soil_data.md) |
| USDA |[SoilData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/SoilData.csv) | [info_loc.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/SoilDataCode.R) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/soil_data_from_USDA.md) |
* Soil data maintained by Jim Holland (jim.holland@usda.gov)

### Phenotypes

|What?|Output-file|Inputs|Script|Data dictionary|
|---------|------|------|------|--------|
|Fortmatted phenotypes |[phenotype.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv)|[repository](https://github.com/QuantGen/G2F_RESOURCES/tree/main/Data/Phenotypes) ; [G2F site](https://www.genomes2fields.org/resources/)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/Phenotypes.R) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/phenotypes.md)|

## Analysis pipelines

### Predicting Growing Degree Days (GDD) needed to silking for each hybrid

|What? |Output-file|Inputs|Script|
|---------|------|------|------|
| GDD to silking | [data_model.rdata](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/GDDtoFlowering/data_model.rdata) | [ConsensusData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/ConsensusData.csv) ; [phenotype.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/GDDPredictFlowering.md) |


<!-- 
 - [Tools to examine weather data](https://github.com/QuantGen/G2F_RESOURCES/blob/main/ExamineEnvData.md)
       - Consensus data: [wdaily_final.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/wdaily_final.csv)
  - [Calculate growing degree days (GDD) and predict flowering date](https://github.com/QuantGen/G2F_RESOURCES/blob/main/GDDPredictFlowering.md)

 - [Baseline model with lme4]()
 - [Genomic relationships and DNA-derived PCs]()
 - [Genomic Regession using BGLR]()
 - [...]
-->
 
