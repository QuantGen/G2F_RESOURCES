# Function to rename columns

rename_columns <- function(existing_colname, new_colname, mydata = pData){
  lapply(mydata, function(x){
    x_col <- grep(existing_colname, colnames(x), ignore.case=T)
    if(is.na(new_colname) & length(x_col) == 1) { x[,x_col] <- NULL } else {
      if(length(x_col) == 1) colnames(x)[x_col] <- new_colname }
    if(length(x_col) > 1) stop(paste('Two columns matched existing_colname:', paste(colnames(x)[x_col], collapse = ' and ')))
    if(length(x_col) < 1) warning('no column matched existing_colname in at least one dataset.')
    return(x)
  })
}

#####

# Calculate daily rainfall from a dataset with columns 'valid' and 'rainfall'

calculate_daily <- function(x) {
  require(tidyverse)
  options(dplyr.summarise.inform = FALSE) # added
  if (!all(c('rainfall', 'valid') %in% colnames(x))) stop('columns named (rainfall) and (valid) are needed.')
  precip <- x %>% 
    filter(!is.na(rainfall)) %>% 
    select(valid, rainfall) %>% 
    mutate(date = with_tz(as.POSIXct(valid, tz='UCT'),
                          "America/New_York"),
           date = date-dst(date)*3600,
           year = year(date),
           month = month(date),
           day = day(date),
           hour = hour(date),
           minute = minute(date)) %>% 
    group_by(year, month, day, hour) 
  
  modalminute <- precip %>%
    arrange(desc(minute), .by_group=TRUE) %>% 
    mutate(maxminute = minute[which.max(rainfall)]) %>% 
    {which.max(tabulate(.$maxminute))}
  
  prec <- precip %>% 
    filter(minute <= modalminute) %>% 
    summarize(hourlyprecip = max(rainfall, na.rm=TRUE)) %>% 
    group_by(year, month, day) %>% 
    summarize('rainfall' = sum(hourlyprecip, na.rm=TRUE))
  prec <- as.data.frame(prec)
  prec$date <- date(ISOdate(prec$year, prec$month, prec$day))
  prec[,5:4]
}

#####

# Get weather data from ASOS/AWOS network through https://mesonet.agron.iastate.edu

getWeatherASOS <- function(time_period = NA, network = "IA_ASOS", sid = NA) {
  
  time_period <- as.Date(time_period)
  if (is.na(sid) | is.na(time_period)[1]) {
    uri <- paste("https://mesonet.agron.iastate.edu/geojson/network/", network, ".geojson", sep = "")
    data <- url(uri)
    jdict <- fromJSON(data)
    return(jdict$features$properties)
  } else {
    if(time_period[1] > time_period[2]) stop('error: time period 1 must be before 2')
    service <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"
    service <- paste(service, "data=all&tz=Etc/UTC&format=comma&latlon=yes&", sep = "")
    service <- paste(service, "year1=", year(time_period)[1], "&month1=", month(time_period)[1], "&day1=", mday(time_period)[1], "&", sep = "")
    service <- paste(service, "year2=", year(time_period)[2], "&month2=", month(time_period)[2], "&day2=", mday(time_period)[2], "&", sep = "")
    
    uri2 <- paste(service, "station=", sid, sep = '')
    data_table <- read.table(uri2, header = T, sep = ',')
    
    return(data_table)
  }
}

# Get weather station information from ASOS/AWOS

getStationASOS <- function(network){
  stations <- getWeatherASOS(network = network)
  if (is.null(stations[1])) stop('network not found')
  stations$lat <- NA
  stations$lon <- NA
  for (i in 1:nrow(stations)){
    tmp <- getWeatherASOS(time_period = c('2018-10-10', '2018-10-10'), network, stations$sid[i])
    stations$lat[i] <- tmp$lat[1]
    stations$lon[i] <- tmp$lon[1]
  }
  stations[,c('elevation', 'sname', 'county', 'state', 'country', 'sid', 'lat', 'lon')]
}
