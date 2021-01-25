### External weather data

Here we provide functions to download weather data from ASOS/AWOS and NOAA networks. 
The following code gets daily accumulated rainfall and temperature from the closest weather station to a given trial location.
Provided location coordinates and time period of interest, the code can be adapted to download weather data for any location (preferably within the US).

#### Download weather data from the closest NOAA station 

```r
# Read table of weather stations around the world
inventory <- read.table('https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt')
plot(inventory$V3, inventory$V2, xlab = 'lon', ylab = 'lat', main = 'All weather stations in the network')
```
![plot](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/Images/NOAAstations.png)

To access these data one have to request a web service token on this site: https://www.ncdc.noaa.gov/cdo-web/webservices/v2

```r
options(noaakey = "Insert your token here")
# Load rnoaa library
library(rnoaa)

# Load functions
source('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Code/Functions.R')

# Load location coordenates
info_loc <- read.csv('../Data/OutputFiles/info_loc.csv')

# Download by trial
wdata_NOAA <- list()
for (i in 1:nrow(info_loc)){
  loc <- info_loc[i,]
  # Get weather stations close to the trial
  iinv <- inventory[loc$lat + 1 > inventory$V2 & inventory$V2 > loc$lat - 1 
                    & loc$lon + 1 > inventory$V3 & inventory$V3 > loc$lon - 1,]
  # Filter for stations with information after 2016 and active today
  iinv <- iinv[iinv$V5 < 2017 & iinv$V6 == max(iinv$V6),]
  # Remove duplicated
  iinv <- iinv[!duplicated(iinv$V1),]
  
  # Calculate distance
  for (j in 1:nrow(iinv)) iinv$NOAAdist[j] <- distGeo(c(loc$lon, loc$lat), c(iinv$V3[j], iinv$V2[j])) / 1000
  
  # Identify closest station
  possible_stations <- sort(setNames(iinv$NOAAdist, nm = iinv$V1))

  w <- 1
  w_loc <- names(possible_stations[w])
  wdata_NOAA[[i]] <- try(getWeatherNOAA(time_period=c(info_loc[i, 'sowing'], info_loc[i, 'harvesting']), sid=w_loc), silent=T)

  # Download from the next closest weather station if there are missing days
  while (ncol(wdata_NOAA[[i]]) < 4 & length(possible_stations) > w) {
    w <- w + 1
    w_loc <- names(possible_stations[w])
    wdata_NOAA[[i]] <- try(getWeatherNOAA(time_period = c(info_loc[i, 'sowing'], info_loc[i, 'harvesting']), sid=w_loc), silent=T)
  }
  
  info_loc[i,'NOAAstation'] <- w_loc
  info_loc[i,'NOAAdist'] <- possible_stations[w]
  print(info_loc[i, 'Location'])
}

```

#### Calculate daily mean temperature and accumulated rainfall from NOAA data

```r
wdaily_NOAA <- list()
for (i in 1:length(wdata_NOAA)) {
  x <- wdata_NOAA[[i]]
  rainfall <- x$PRCP
  temp_min <- switch(as.numeric(is.null(x$TMIN))+1, x$TMIN, NA)
  temp_max <- switch(as.numeric(is.null(x$TMAX))+1, x$TMAX, NA)
  temp <- (temp_max + temp_min) / 2
  wdaily_NOAA[[i]] <- data.frame(year=info_loc$year[i],
                                 location=info_loc$Location[i],
                                 date=date(wdata_NOAA[[i]]$date), 
                                 rainfall, temp, temp_min, temp_max)
}
wdaily_NOAA <- do.call(rbind, wdaily_NOAA)
```

Files are [info_loc.csv]() and [NOAAdaily.csv]()

```r
write.csv(info_loc, file = '../Data/OutputFiles/info_loc.csv', quote = F, row.names = F)
write.csv(wdaily_ASOS, file = '../Data/OutputFiles/NOAAdaily.csv', quote = F, row.names = F)
```
