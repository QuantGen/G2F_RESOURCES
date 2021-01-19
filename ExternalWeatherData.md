### External weather data

The following script provides functions to download weather data from ASOS/AWOS and NOAA networks.
We will download daily temperature and daily accumulated rainfall data for the locations evaluated in the G2F project. However, the functions can be adapt to download data for other locations as well.

#### Read location information

```r
# Loading location information
info_loc <- read.csv('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Data/Metadata/location_coordenates.csv')

# Read characters as date format
info_loc$sowing <- strptime(info_loc$sowing, "%m/%d/%y")
info_loc$harvesting <- strptime(info_loc$harvesting, "%m/%d/%y")
```

#### Creates a dataframe of all ASOS/AWOS weather stations in the network
(warning: this code takes several minutes)
```r
# Load functions
source('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Code/Functions.R')

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
write.csv(ASOSstations, file = '../OutputFiles/ASOS_Stations.csv')

```
