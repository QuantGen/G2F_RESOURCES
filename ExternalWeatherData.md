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
  
  # download from the next closest weather station if there are missing days
  tmp_req_ndays <- floor(as.numeric(loc$harvesting - loc$sowing))
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
  }
  
  info_loc[i,'ASOSstation'] <- w_loc
  info_loc[i,'ASOSdist'] <- possible_stations[w] / 1000 # Distance in km
  print(loc$Location)
}

save(wdata_ASOS, info_loc, file = '../Data/OutputFiles/wdata_ASOS.rdata')
```

#### Download weather data from the closest NOAA station 

```r
# Read table of weather stations around the world
inventory <- read.table('https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt')
plot(inventory$V3, inventory$V2, xlab = 'lon', ylab = 'lat', main = 'All weather stations in the network')
```
![plot](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/Images/NOAAstations.png)
