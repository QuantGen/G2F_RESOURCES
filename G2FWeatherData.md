### G2F weather data

The following script reads G2F weather data from the locations that were evaluated in 2018 and 2019.
It also calculates daily mean temperature and daily accumulated rainfall.

```r
# Download G2F data

urlg2f2018 <- 'https://github.com/QuantGen/G2F_RESOURCES/raw/main/Data/EnvironmentalCovariates/G2F_weather_2018.csv.zip'
urlg2f2019 <- 'https://github.com/QuantGen/G2F_RESOURCES/raw/main/Data/EnvironmentalCovariates/G2F_weather_2019.csv.zip'

wdata_G2F <- list()
wdata_G2F[[1]] <- read.csv(unz(urlg2f2018, filename = 'G2F_weather_2018.csv'))
wdata_G2F[[2]] <- read.csv(unz(urlg2f2019, filename = 'G2F_weather_2019.csv'))

# Load function to rename columns
source('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Code/Functions.R')

# lower case all column names
wdata_G2F <- lapply(wdata_G2F, function(x){colnames(x) <- tolower(colnames(x));x})

# Renaming columns
rep_matrix <- matrix(c('(record.*number)', NA,
                       '(field.*location)', 'location',
                       '(station.*id)', 'station_id',
                       'date_local', NA,
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
                       '(uv.*um)', NA,
                       '(par.*um)', NA,
                       '(co2.*ppm)', NA,
                       'photoperiod', NA,
                       '(column.*altered)', NA,
                       '(altered.*column)', NA,
                       '(cleaning.*method)', NA,
                       'comment', NA,
                       '^X$', NA), ncol=2, byrow=T)
for (i in 1:nrow(rep_matrix)) 
  wdata_G2F <- rename_columns(rep_matrix[i,1], rep_matrix[i,2], mydata=wdata_G2F)
```

#### Calculate daily mean temperature and precipitation

```r
# Load package to manipulate dates
library(lubridate)

# Create a valid date variable
wdata_G2F <- lapply(wdata_G2F, function(data){
  if(lengths(regmatches(data$time[1], gregexpr(":", data$time[1]))) == 1)
    data$time <- paste0(data$time, ':00') # add if missing seconds
  tmp <- matrix(unlist(strsplit(data$time, ':')), ncol=3, byrow=T)
  data$valid <- with(data,
                    date(ISOdate(year, month, day,
                                 hour = as.numeric(tmp[,1]),
                                 min = as.numeric(tmp[,2]),
                                 sec = as.numeric(tmp[,3])
                    )))
  return(data)
})

# Transform weather list to data frame
wdf_G2F <- do.call(rbind, wdata_G2F)

# Remove locations without data
wdf_G2F <- wdf_G2F[wdf_G2F$location %in% names(which(table(wdf_G2F$location) > 10)),]

# Rainfall as numeric
wdf_G2F$rainfall <- as.numeric(wdf_G2F$rainfall)

# Calculate daily temperature
temp <- aggregate(temp ~ valid + location, data = wdf_G2F, FUN = function(x) c(mean(x, na.rm=T), min(x, na.rm=T), max(x, na.rm=T)))
daily_temp <-  data.frame(date = temp$valid, location = temp$location, 
                          temp = temp$temp[,1], temp_min = temp$temp[,2], temp_max = temp$temp[,3])

# Calculate daily rainfall
daily_rf <- do.call(rbind, by(wdf_G2F, paste0(wdf_G2F$location, '_', wdf_G2F$year), calculate_daily))
daily_rf$location <- sapply(strsplit(rownames(daily_rf), '_'), function(x) x[1])
rownames(daily_rf) <- NULL

# Merge daily rainfall with temperature
wdaily_G2F <- merge(daily_rf, daily_temp, by = c('date', 'location'))

write.csv(wdaily_G2F, file = '../OutputFiles/G2Fdaily.csv', quote = F, row.names = F)
```
The resulting file is [G2Fdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/G2Fdaily.csv)

This file has the following columns:

|Column|Description|
|------|-----------|
|date| (yyyy-mm-dd) Sowing date |
|location| G2F field location name |
|rainfall| (mm) Daily rainfall |
|temp| (°C) Daily mean temperature |
|temp_min| (°C) Daily minimum temperature |
|temp_max| (°C) Daily maximum temperature |


[Go back to main](https://github.com/QuantGen/G2F_RESOURCES)
