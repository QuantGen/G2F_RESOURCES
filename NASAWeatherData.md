```r
# NASA power
library(nasapower)

allpars <- do.call(rbind, lapply(parameters, function(x){
  data.frame(description = x$longname, 
             daily = 'DAILY' %in% x$include, 
             AG = 'AG' %in% x$community, 
             unit = paste(x$SB_Units))
}))

# daily Agroclimatic variables
AGvar <- allpars[allpars$AG & allpars$daily,]
AGvar <- AGvar[rownames(AGvar) != 'WSC',] # Removing corrected wind speed because it's not available 
AGvar

# Load location data
info_loc <- read.csv('/OutputFiles/info_loc.csv')

# as.Date format
info_loc$sowing <- as.Date(info_loc$sowing)
info_loc$harvesting <- as.Date(info_loc$harvesting)

# Get data for all locations
AGdata <- list()
for (i in 1:nrow(info_loc)) {
  AGdata[[i]] <- cbind(location = info_loc$Location[i],
                       as.data.frame(get_power(community = "AG",
                                               lonlat = c(info_loc$lon[i], info_loc$lat[i]),
                                               pars = rownames(AGvar),
                                               dates = c(info_loc$sowing[i], info_loc$harvesting[i]),
                                               temporal_average = "DAILY")))
  message(i, ' loc = ', info_loc$Location[i], ' done.')
}
AGdata <- do.call(rbind, AGdata)
AGdata <- AGdata[,c('YYYYMMDD', 'location', 'PRECTOT', 'T2M', 'T2M_MIN', 'T2M_MAX', 'ALLSKY_SFC_LW_DWN', 'ALLSKY_SFC_SW_DWN',
                    'ALLSKY_TOA_SW_DWN', 'PS', 'RH2M', 'T2MDEW', 'T2MWET', 'TS', 'WS10M')]
colnames(AGdata)[c(1, 3:6)] <- c('date', 'rainfall', 'temp', 'temp_min', 'temp_max')
colnames(AGdata) <- casefold(colnames(AGdata))

# Save data
write.csv(AGdata, file = '/OutputFiles/NASAdaily.csv', quote = F, row.names = F)
```


The file NASAdaily.csv has the following columns:

|Column|Description|
|------|-----------|
|date| (yyyy-mm-dd) Sowing date |
|location| G2F field location name |
|rainfall| (mm) Daily rainfall |
|temp| (°C) Daily mean temperature |
|temp_min| (°C) Daily minimum temperature |
|temp_max| (°C) Daily maximum temperature |
