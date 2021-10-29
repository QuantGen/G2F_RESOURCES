## G2F RESOURCES


This repository provides resources (data and pipelines) linked to data generated by the GxE initiative of the [Genomes to Fields](https://www.genomes2fields.org/) project.

### Daily weather data

|Source |Output-file|Inputs|Script|Data dictionary|
|---------|------|------|------|--------|
| NASA |[NASA_WeatherData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/NASA_WeatherData.csv)|[info_loc.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/NASAWeatherData.R)| [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/NASAWeatherData.md) |
| G2F |  [g2f_WeatherData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/g2f_WeatherData.csv)  | [G2F_weather_2018.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/EnvironmentalCovariates/G2F_weather_2018.csv.zip) ; [G2F_weather_2019.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/EnvironmentalCovariates/G2F_weather_2019.csv.zip) ; [NASA_WeatherData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/NASA_WeatherData.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/g2f_WeatherData.R)  | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/g2f_WeatherData.md) |

The Consensus weather data combines data from G2F with data from NASA. NASA data was used for variables not recorded by G2F and to fill records from G2F that were either missing or appeared to be wrong. For further information about each of these networks please see the corresponding "mdfiles".
* Weather data maintained by Fernando Aguate (matiasfe@msu.edu), Marco Lopez-Cruz (lopezcru@msu.edu), and Gustavo de los Campos (gustavoc@msu.edu)

### Soil data

|Source |Output-file|Inputs|Script|Data dictionary|
|---------|------|------|------|--------|
| G2F |[g2f_SoilData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/g2f_SoilData.csv) | [g2f_2018_soil_data.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/Metadata/g2f_2018_soil_data.csv) ; [g2f_2019_soil_data.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/Metadata/g2f_2019_soil_data.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/g2f_SoilData.R) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/g2f_soil_data.md) |
| USDA |[USDA_SoilData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/USDA_SoilData.csv) | [info_loc.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/SoilDataCode.R) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/soil_data_from_USDA.md) |
| Consensus G2F and USDA | [SoilData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/SoilData.csv) | [g2f_SoilData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/g2f_SoilData.csv) ; [USDA_SoilData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/USDA_SoilData.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/Consensus_SoilData.R) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/Consensus_SoilData.md) | 
* Soil data maintained by Jim Holland (jim.holland@usda.gov)

### Phenotypes

| What? | Input | Script | Output | Data dictionary |
|-------|-------|--------|--------|-----------------|
|Fortmatted phenotypes | [repository](https://github.com/QuantGen/G2F_RESOURCES/tree/main/Data/Phenotypes) ; [G2F site](https://www.genomes2fields.org/resources/)|[link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/Phenotypes.R) |[phenotype.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/phenotypes.md)|
|Processed phenotypes | [phenotype.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv) ; [info_loc.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv) ; [loc_GDD_clusters.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/loc_GDD_clusters.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/prepare_pheno_data.R) | [clean_pheno_yearloc_means.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/clean_pheno_yearloc_means.csv) ; [clean_pheno_raw.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/clean_pheno_raw.csv) | |

## Analysis pipelines

### Predicting Growing Degree Days (GDD) needed to silking for each hybrid

| What? | Output | Inputs | Script |
|-------|--------|--------|--------|
| GDD to silking | [data_model.rdata](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/GDDtoFlowering/data_model.rdata) | [ConsensusData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/ConsensusData.csv) ; [phenotype.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/GDDPredictFlowering.md) |
| GDD to silking/harvest| [GDD_hat_by_hybrids.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/GDD_hat_by_hybrids.csv) | [phenotype.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/GDDPredict_MM.md) |

### Run APSIM Crop model to generate environmental covariates

[Weather data](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/APSIM_sim/Weather_data.csv) for APSIM can be downloaded with [this script](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/APSIM_getWeather.R)

| What? | Input | Script | Output | Description |
|-------|-------|--------|--------|-------------|
|   v0   | [info_loc.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/APSIM_v0_code.R) | [v0Sim.rdata](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/APSIM_sim/v0Sim.rdata) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/APSIM_v0.md) |
| v0 Post hoc  | [v0Sim.rdata](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/APSIM_sim/v0Sim.rdata) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/APSIM_v0_posthoc.R) | [simulated_data_v0.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/simulated_data_v0.csv) | [plots](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/simulation_plots.md) |
|   v1     | [info_loc.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/APSIM_v1_code.R) | [v1Sim.rdata](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/APSIM_sim/v1Sim.rdata) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/APSIM_v1.md) |
| v1 Post hoc   | [v1Sim.rdata](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/APSIM_sim/v1Sim.rdata) |   [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Rcodes/APSIM_v1_posthoc.R) | [simulated_data_v1.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/simulated_data_v1.csv), [envir_p1.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/envir_p1.csv), [envir_p2.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/envir_p2.csv) | [link](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/simulation_v1.md) |

<!-- 
 - [Tools to examine weather data](https://github.com/QuantGen/G2F_RESOURCES/blob/main/ExamineEnvData.md)
       - Consensus data: [wdaily_final.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/wdaily_final.csv)
  - [Calculate growing degree days (GDD) and predict flowering date](https://github.com/QuantGen/G2F_RESOURCES/blob/main/GDDPredictFlowering.md)

 - [Baseline model with lme4]()
 - [Genomic relationships and DNA-derived PCs]()
 - [Genomic Regession using BGLR]()
 - [...]
-->
 
