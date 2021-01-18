## Environmental Covariates

The following script allows to get daily records of precipitation and temperature for the locations that were evaluated in 2018 and 2019.
We also provide code to download these data from external sources (ASOS weather network and NOAA).

### G2F weather data

```r
# Download G2F data

# THIS DOES NOT WORK ON PRIVATE REPOSITORIES

urlg2f2018 <- 'https://github.com/QuantGen/G2F_RESOURCES/raw/main/Data/EnvironmentalCovariates/G2F_weather_2018.csv.zip'
urlg2f2019 <- 'https://github.com/QuantGen/G2F_RESOURCES/raw/main/Data/EnvironmentalCovariates/G2F_weather_2019.csv.zip'

wdata_G2F <- list()
wdata_G2F[[1]] <- read.csv(unz(urlg2f2018, filename = 'G2F_weather_2018.csv'))
wdata_G2F[[2]] <- read.csv(unz(urlg2f2019, filename = 'G2F_weather_2019.csv'))

# Load function to rename columns
ftoken <- '?token=ADHZTMISK6CIKMVRAHCQRTLAB3UWC' # Remove token
source(paste0('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Code/Functions.R', ftoken))

# lower case all column names
wdata_G2F <- lapply(wdata_G2F, function(x){colnames(x) <- tolower(colnames(x));x})

# Renaming columns
rep_matrix <- matrix(c('(record.*number)', NA,
                       '(field.*location)', 'location',
                       '(station.*id)', 'station_id',
                       'nws.*network', NA,
                       'nws.*station', NA,
                       '^temperature', 'temp',
                       '(dew.*point)', 'dew_point',
                       '(relative.*humidity)', 'rel_humidity',
                       '(solar.*radiation)', 'solar_rad',
                       'rainfall', 'rainfall',
                       '(wind.*speed)', 'wind_speed',
                       '(wind.*direction)', 'wind_dir',
                       '(wind.*gust)', 'wind_gust',
                       '(soil.*temperature)', 'soil_temp',
                       '(soil.*moisture)', 'soil_moist',
                       '(soil.*ec)', NA,
                       '(uv.*light)', NA,
                       '(par.*um)', NA,
                       '(co2.*ppm)', NA,
                       '(column.*altered)', NA,
                       '(altered.*column)', NA,
                       '(cleaning.*method)', NA,
                       'comment', NA,
                       '^X$', NA), ncol=2, byrow=T)
for (i in 1:nrow(rep_matrix)) 
  wdata_G2F <- rename_columns(rep_matrix[i,1], rep_matrix[i,2], mydata = wdata_G2F)
```

#### Calculate daily mean temperature and precipitation


### ASOS weather data

### NOAA weather data

