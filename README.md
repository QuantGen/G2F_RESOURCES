## G2F RESOURCES


This repository provides resources (data and pipelines) linked to data generated by the GxE initiative of the [Genomes to Fields](https://www.genomes2fields.org/) project.

## Daily temperature and rainfall data

|Output |Output-file|Input|Script|Contributors|
|---------|------|------|------|--------|
|Daily Temperature and Precipitation (derived from G2F-GxE data)|[G2Fdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/G2Fdaily.csv)|[repository](https://github.com/QuantGen/G2F_RESOURCES/tree/main/Data/EnvironmentalCovariates)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/G2FWeatherData.md)|  matiasfe@msu.edu |
|ASOS/AWOS weather data|[ASOSdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/ASOSdaily.csv)|[location_coordinates.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/Metadata/location_coordinates.csv)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/ASOSWeatherData.md)|Gets daily weather data from ASOS/AWOS networks|
|NOAA weather data|[NOAAdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/NOAAdaily.csv)|[info_loc.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/NOAAWeatherData.md)|Gets daily weather data from NOAA networks|
|Concensus Temperature and Rainfall data  |  [wdaily_final.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/wdaily_final.csv)  | [G2Fdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/G2Fdaily.csv) / [NOAAdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/NOAAdaily.csv)  \| [ASOSdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/ASOSdaily.csv) / NOAAdaily.csv)   | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/ExamineEnvData.md)  |  matiasfe@msu.edu |

## Calculation of growing degree days per year-location

|Output |Output-file|Input|Script|Contributors|
|---------|------|------|------|--------|
| Growind degree days   |  [?]()  |  [wdaily_final.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/wdaily_final)  | [Calculate growing degree days (GDD) and predict flowering date](https://github.com/QuantGen/G2F_RESOURCES/blob/main/GDDPredictFlowering.md)   | matiasfe@msu.edu |

## Phenotypes

|Output |Output-file|Input|Script|Contributors|
|---------|------|------|------|--------|
|Phenotype|[phenotype.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv)|[repository](https://github.com/QuantGen/G2F_RESOURCES/tree/main/Data/Phenotypes) - [G2F site](https://www.genomes2fields.org/resources/)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/phenotypes.md)|Formats the phenotype data into a friendly dataset|

## Analysis pipelines

 - [Tools to examine weather data](https://github.com/QuantGen/G2F_RESOURCES/blob/main/ExamineEnvData.md)
       - Consensus data: [wdaily_final.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/wdaily_final.csv)
  - [Calculate growing degree days (GDD) and predict flowering date](https://github.com/QuantGen/G2F_RESOURCES/blob/main/GDDPredictFlowering.md)
<!-- 
 - [Baseline model with lme4]()
 - [Genomic relationships and DNA-derived PCs]()
 - [Genomic Regession using BGLR]()
 - [...]
-->
 
