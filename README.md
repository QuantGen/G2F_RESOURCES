## G2F RESOURCES


This repository provides resources (data and pipelines) linked to data generated by the GxE initiative of the [Genomes to Fields](https://www.genomes2fields.org/) project.

## Data and data pre-procesing

|Data type|Output|Source|Script|Comments|
|---------|------|------|------|--------|
|Phenotype|[phenotype.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv)|[repository](https://github.com/QuantGen/G2F_RESOURCES/tree/main/Data/Phenotypes) - [G2F site](https://www.genomes2fields.org/resources/)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/phenotypes.md)|Formats the phenotype data into a friendly dataset|
|G2F weather data|[G2Fdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/G2Fdaily.csv)|[repository](https://github.com/QuantGen/G2F_RESOURCES/tree/main/Data/EnvironmentalCovariates)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/G2FWeatherData.md)|Reads G2F weather data and calculates daily data|
|ASOS/AWOS weather data|[ASOSdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/ASOSdaily.csv)|[location_coordenates.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/Metadata/location_coordenates.csv)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/ASOSWeatherData.md)|Gets daily weather data from ASOS/AWOS networks|
|NOAA weather data|[NOAAdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/NOAAdaily.csv)|[info_loc.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/NOAAWeatherData.md)|Gets daily weather data from NOAA networks|


## Analysis pipelines

 - [Tools to examine environmental data](https://github.com/QuantGen/G2F_RESOURCES/blob/main/ExamineEnvData.md)
       - Final weather data: [wdaily_final.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/wdaily_final.csv)
 - [Baseline model with lme4]()
 - [Genomic relationships and DNA-derived PCs]()
 - [Genomic Regession using BGLR]()
 - [...]
 
