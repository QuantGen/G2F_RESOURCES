The Power project provides solar and meteorological data sets from NASA research for support of renewable energy, building energy efficiency and agricultural needs.
The following script uses the "nasapower" package to download agroclimatic variables from The Power project.

```r
# Install package
install.packages('nasapower')

# Load library
library(nasapower)
```

The package contains a list that describes each variable. We read that list and select variables that are of agroclimatic interest in daily format.
```r
allpars <- do.call(rbind, lapply(parameters, function(x){
  data.frame(description = x$longname, 
             daily = 'DAILY' %in% x$include, 
             AG = 'AG' %in% x$community, 
             unit = paste(x$SB_Units))
}))

# daily Agroclimatic variables
AGvar <- allpars[allpars$AG & allpars$daily,]
AGvar <- AGvar[rownames(AGvar) != 'WSC',] # Removing corrected wind speed because it's not available 
> AGvar
                                                          description daily   AG          unit
ALLSKY_SFC_LW_DWN Downward Thermal Infrared (Longwave) Radiative Flux  TRUE TRUE kW-hr/m^2/day
ALLSKY_SFC_SW_DWN All Sky Insolation Incident on a Horizontal Surface  TRUE TRUE kW-hr/m^2/day
ALLSKY_TOA_SW_DWN                        Top-of-atmosphere Insolation  TRUE TRUE kW-hr/m^2/day
PRECTOT                                                 Precipitation  TRUE TRUE      mm day-1
PS                                                   Surface Pressure  TRUE TRUE           kPa
RH2M                                    Relative Humidity at 2 Meters  TRUE TRUE             %
T2M                                           Temperature at 2 Meters  TRUE TRUE             C
T2MDEW                                    Dew/Frost Point at 2 Meters  TRUE TRUE             C
T2MWET                               Wet Bulb Temperature at 2 Meters  TRUE TRUE             C
T2M_MAX                               Maximum Temperature at 2 Meters  TRUE TRUE             C
T2M_MIN                               Minimum Temperature at 2 Meters  TRUE TRUE             C
TS                                             Earth Skin Temperature  TRUE TRUE             C
WS10M                                         Wind Speed at 10 Meters  TRUE TRUE           m/s
```

```r
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
# Format colnames
AGdata <- AGdata[,c('YYYYMMDD', 'location', 'PRECTOT', 'T2M', 'T2M_MIN', 'T2M_MAX', 'ALLSKY_SFC_LW_DWN', 'ALLSKY_SFC_SW_DWN',
                    'ALLSKY_TOA_SW_DWN', 'PS', 'RH2M', 'T2MDEW', 'T2MWET', 'TS', 'WS10M')]
colnames(AGdata)[c(1, 3:6)] <- c('date', 'rainfall', 'temp', 'temp_min', 'temp_max')
colnames(AGdata) <- casefold(colnames(AGdata))
```

We calculate a theoretical evapotranspiration using the ClimMobTools package. To derive the crop factor (Kc) needed by growth stage, we used Tables 7 and 8 of the following documentation: http://www.fao.org/3/s2022e/s2022e07.htm

```r
# Add evapotranspiration (eto)

# install.packages('ClimMobTools')
library(ClimMobTools) # Package used to calculate eto

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
|allsky_sfc_lw_dwn| (kW-hr/m^2/day) Downward Thermal Infrared (Longwave) Radiative Flux|
|allsky_sfc_sw_dwn| (kW-hr/m^2/day) All Sky Insolation Incident on a Horizontal Surface|
|allsky_toa_sw_dwn| (kW-hr/m^2/day) Top-of-atmosphere Insolation|
|ps|(kPa) Surface Pressure|
|rh2m|(%) Relative Humidity at 2 Meters|
|t2mdew|(°C) Dew/Frost Point at 2 Meters|
|t2mwet|(°C) Wet Bulb Temperature at 2 Meters|
|ts|(°C) Earth Skin Temperature|
|ws10m| (m/s) Wind Speed at 10 Meters|
|eto (mm/day)| General theoretical evapotranspiration calculated using Blaney-Criddle method. 



