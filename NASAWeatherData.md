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
colnames(AGdata)[grep("YYYYMMDD", colnames(AGdata))] <- 'date'

# Save data
write.csv(AGdata, file = '/OutputFiles/NASAdaily.csv', quote = F, row.names = F)
```
