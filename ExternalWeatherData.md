### External weather data

The following script downloads weather data for each trial location, from sowing to harvesting.
The data come from two external weather networks: ASOS and NOAA.

#### Read location information

```r
# Loading location information
info_loc <- read.csv('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Data/Metadata/location_coordenates.csv')

# Read characters as date format
info_loc$sowing <- strptime(info_loc$sowing, "%m/%d/%y")
info_loc$harvesting <- strptime(info_loc$harvesting, "%m/%d/%y")
```

#### Download data from ASOS weather stations

```r

```
