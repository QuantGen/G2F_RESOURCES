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
write.csv(ASOSstations, file='../OutputFiles/ASOS_Stations.csv')

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


