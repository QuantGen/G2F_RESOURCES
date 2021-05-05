# Install and load needed package
install.packages('nasapower')
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
info_loc <- read.csv('Data/OutputFiles/info_loc.csv')

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

# Format colnames
AGdata <- AGdata[,c('YYYYMMDD', 'location', 'PRECTOT', 'T2M', 'T2M_MIN', 'T2M_MAX', 'ALLSKY_SFC_LW_DWN', 'ALLSKY_SFC_SW_DWN',
                    'ALLSKY_TOA_SW_DWN', 'PS', 'RH2M', 'T2MDEW', 'T2MWET', 'TS', 'WS10M')]
colnames(AGdata)[c(1, 3:6)] <- c('date', 'rainfall', 'temp', 'temp_min', 'temp_max')
colnames(AGdata) <- casefold(colnames(AGdata))

# Add evapotranspiration (eto) isomg ClimMobTools package
install.packages('ClimMobTools')
library(ClimMobTools)
# More information about this calculation: http://www.fao.org/3/s2022e/s2022e07.htm

res <- list()
for (i in 1:nrow(info_loc)) {
  # growth period
  dates <- seq(info_loc[i, 'sowing'], info_loc[i, 'harvesting'], 1)
  gp_Kc <- c(0.4, 0.8, 1.15, 0.7) # Kc for Maize, grain from Table 8
  gp_proportion <- cumsum(c(30/180, 50/180, 60/180, 40/180)) # Proportion of growth stages based on Table 7
  gp_starting <- as.Date(quantile(as.numeric(dates), probs = c(0, gp_proportion)), origin = '1970-01-01')
  gp_stages <- as.numeric(cut(dates, breaks = gp_starting, right = T, include.lowest = T))
  gp_eto <- gp_stages
  for (st in 1:4) {
    eto <- as.numeric(ETo(info_loc[i, c('lon', 'lat')],
                          day.one = gp_starting[st],
                          span = sum(gp_stages == st), Kc = gp_Kc[st]))
    gp_eto[gp_stages == st] <- eto
  }
  res[[i]] <- data.frame(location = info_loc$Location[i], date = dates, eto = gp_eto)
}
res2 <- do.call(rbind, res)
AGdata <- merge(AGdata, res2, by = c('date', 'location'))

# Save data
write.csv(AGdata, file = 'Data/OutputFiles/NASAdaily.csv', quote = F, row.names = F)
