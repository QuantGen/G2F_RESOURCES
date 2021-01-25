### External weather data

Here we provide functions to download weather data from ASOS/AWOS and NOAA networks. 
The following code gets daily accumulated rainfall and temperature from the closest weather station to a given trial location.
Provided location coordinates and time period of interest, the code can be adapted to download weather data for any location (preferably within the US).

#### Read location information

```r
# Loading location information
info_loc <- read.csv('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Data/Metadata/location_coordenates.csv')

# Read characters as date format
info_loc$sowing <- strptime(info_loc$sowing, "%m/%d/%y")
info_loc$harvesting <- strptime(info_loc$harvesting, "%m/%d/%y")
```

#### Creates a dataframe of ASOS/AWOS weather stations in the network
(warning: this code takes several minutes)
```r
# Load functions
source('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Code/Functions.R')
# States of interest
states <- c('AR', 'DE', 'GA', 'IA', 'IL', 'IN', 'KS', 'MI', 'MN', 'MO', 'NC', 'NE', 'NY', 'OH', 'CA_ON', 'SC', 'TX', 'WI', 'CO', 'AWOS')

stations_list <- list()
for (i in 1:length(states)){
  net <- paste0(states[i], '_ASOS')
  if(states[i] == 'AWOS') net <- 'AWOS'
  stations_list[[i]] <- getStationASOS(network = net)
  if(states[i] == 'AWOS') stations_list[[i]]$sid <- paste0(stations_list[[i]]$sid, '_AWOS')
  print(states[i])
}
ASOSstations <- do.call(rbind, stations_list)
# Remove NAs in lat-long
ASOSstations <- ASOSstations[rowSums(is.na(ASOSstations[c('lat', 'lon')])) == 0,]
write.csv(ASOSstations, file='../OutputFiles/ASOS_Stations.csv')

> head(ASOSstations)
  X elevation              sname       county state country sid     lat      lon
1 1   57.5938        Arkadelphia        Clark    AR      US ADF 34.0998 -93.0661
3 3  141.0000  BATESVILLE (AWOS) Independence    AR      US BVX 35.7261 -91.6472
5 5  395.0000 BENTONVILLE (AWOS)       Benton    AR      US VBT 36.3457 -94.2194
6 6   78.0000        BLYTHEVILLE  Mississippi    AR      US HKA 35.9400 -89.8300
7 7   39.6000             Camden      Calhoun    AR      US CDH 33.6228 -92.7634
8 8  156.7000            Clinton    Van Buren    AR      US CCA 35.5978 -92.4516

```
Find here a link to [ASOS_Stations.csv]()

#### Calculates distance between ASOS stations and G2F trial locations

```r
library(geosphere)

dist_matrix <- matrix(NA, ncol=nrow(info_loc), nrow=nrow(ASOSstations))
colnames(dist_matrix) <- info_loc$Location
rownames(dist_matrix) <- ASOSstations$sid
for(i in 1:nrow(info_loc)){
  for(j in 1:nrow(ASOSstations)){
    dist_matrix[j,i] <- distGeo(c(info_loc[i,'lon'], info_loc[i,'lat']), c(ASOSstations[j, 'lon'], ASOSstations[j, 'lat']))
  }
}

```

#### Download weather data from the closest ASOS/AWOS station 

```r
wdata_ASOS <- list()
for (i in 1:nrow(info_loc)){
  loc <- info_loc[i,]
  # Identify closest station
  possible_stations <- sort(dist_matrix[, loc$Location])
  w <- 1
  w_loc <- names(possible_stations[w])
  # Identify network
  network <- switch (length(grep('AWOS', w_loc))+1, 
                     paste0(substr(loc$Location, 1, 2), '_ASOS'),
                     'AWOS')
  # Remove AWOS identifier
  w_loc <- gsub('_AWOS', '', w_loc)
  # Download data
  wdata_ASOS[[i]] <- getWeatherASOS(time_period=c(loc$sowing, loc$harvesting), network=network, sid=w_loc)
  
  # Download from the next closest weather station if there are missing days
  tmp_req_ndays <- as.numeric(loc$harvesting - loc$sowing) - 1
  tmp_ndays <- length(unique(date(wdata_ASOS[[i]]$valid)))
  
  while (tmp_req_ndays > tmp_ndays) {
    w <- w + 1
    w_loc <- names(possible_stations[w])
    # Identify network
    network <- switch (length(grep('AWOS', w_loc))+1, 
                       paste0(substr(loc$Location, 1, 2), '_ASOS'),
                       'AWOS')
    # Remove AWOS identifier
    w_loc <- gsub('_AWOS', '', w_loc)
    wdata_ASOS[[i]] <- getWeatherASOS(time_period = c(loc$sowing, loc$harvesting), network = network, sid = w_loc)
    tmp_ndays <- length(unique(date(wdata_ASOS[[i]]$valid)))
    if (w == length(possible_stations)) stop('No more stations to look at..')
  }
  
  info_loc[i,'ASOSstation'] <- w_loc
  info_loc[i,'ASOSdist'] <- possible_stations[w] / 1000 # Distance in km
  print(loc$Location)
}
```

#### Calculate daily mean temperature and accumulated rainfall from ASOS data

```r
wdaily_ASOS <- list()
for (i in 1:length(wdata_ASOS)) {
  wtmp <- wdata_ASOS[[i]]
  # Transform rainfall to mm
  wtmp$rainfall <- as.numeric(wtmp$p01i) * 25.4
  # get date from time variable
  wtmp$valid <- date(wtmp$valid)
  # Calculate daily accumulated rainfall
  dpp <- cbind(year=info_loc$year[i], location=info_loc$Location[i], calculate_daily(wtmp))
  # Transform temperature to celsius
  wtmp$tmpc <- (as.numeric(wtmp$tmpf) - 32) * 5/9
  # Calculate daily temperature
  if (all(is.na(wtmp$tmpc))) {
    wdaily_ASOS[[i]] <- cbind(dpp, data.frame(temp=NA, temp_min=NA, temp_max=NA))
  } else {
    dtemp <- aggregate(tmpc ~ valid, data=wtmp, FUN=function(x) c(mean(x, na.rm=T), min(x, na.rm=T), max(x, na.rm=T)))
    dtemp$tmpc <- round(dtemp$tmpc, 2)
    dtemp2 <- data.frame(date=dtemp$valid, temp=dtemp$tmpc[,1], temp_min=dtemp$tmpc[,2], temp_max=dtemp$tmpc[,3])
    wdaily_ASOS[[i]] <- merge(dpp, dtemp2, by='date')
  }
}
wdaily_ASOS <- do.call(rbind, wdaily_ASOS)
}
```

The resulting files are [info_loc.csv]() and [ASOSdaily.csv]()

```r
write.csv(info_loc, file = '../Data/OutputFiles/info_loc.csv', quote = F, row.names = F)
write.csv(wdaily_ASOS, file = '../Data/OutputFiles/ASOSdaily.csv', quote = F, row.names = F)
```
