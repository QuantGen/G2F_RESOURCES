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

#### Download data from ASOS/AWOS weather stations

```r
# Load functions
source('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Code/Functions.R')


```
