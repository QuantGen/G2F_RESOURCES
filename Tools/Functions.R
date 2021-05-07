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
                          ""),
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
  require(jsonlite)
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

# Get weather data from NOAA

getWeatherNOAA <- function(time_period = c('2018-01-01', '2018-12-31'), sid = 'ZI000067983', dataid = 'GHCND') {
  sid2 <- paste0(dataid, ':', sid)
  weather <- list()
  tmp_offset <- 0
  while(T){
    tmp <- ncdc(datasetid = dataid, stationid = sid2, startdate = time_period[1], enddate = time_period[2], limit = 1000, offset = tmp_offset)$data
    tmp_offset <- tmp_offset + 1000
    weather[[length(weather) + 1]] <- tmp
    if (nrow(tmp) < 1000) break
  }
  weather <- as.data.frame(do.call(rbind, weather))
  dtypes <- grep('PRCP|TMAX|TMIN|TOBS', unique(weather$datatype), value = T)
  dweather <- data.frame(date = unique(weather$date))
  for (ty in dtypes){
    tmpw <- weather[weather$datatype == ty, c('date', 'value')]
    dweather[,ty] <- tmpw$value[match(dweather$date, tmpw$date)] / 10
  }
  return(dweather)
}

################

# Function to calculate GDD from weather data

calcGDD <- function(wdata, phenotype, envCol = 'env', intCol = c('date_plant', 'date_silking'), basetemp = 10) {
  # works with celsius degrees only
  if (!(envCol %in% colnames(wdata) & envCol %in% colnames(phenotype))) stop('envCol must be in colnames of wdata and phenotype')
  
  phenotype <- phenotype[order(phenotype$env),]
  phenotype$GDD <- NA
  phenotype[,intCol[1]] <- as.Date(phenotype[,intCol[1]])
  phenotype[,intCol[2]] <- as.Date(phenotype[,intCol[2]])
  # Remove missing dates and environments
  phenotype <- phenotype[!is.na(phenotype[,intCol[1]]),]
  phenotype <- phenotype[!is.na(phenotype[,intCol[2]]),]
  phenotype <- phenotype[phenotype[,envCol] %in% unique(wdata[,envCol]),]
  
  for (env in unique(phenotype$env)) {
    envi <- wdata[wdata$env == env, c('date', 'temp')]
    if (nrow(envi) > 0){
      # missing mean temperature is filled in with the environment's mean temp
      envi$temp[is.na(envi$temp)] <- mean(envi$temp, na.rm = T)
      envi$temp[envi$temp > 30] <- 30
      envi$temp[envi$temp < basetemp] <- basetemp
      envi$gdd <- envi$temp - basetemp
    }
    for (i in which(phenotype$env == env)){
      envrows <- match(as.Date(phenotype[i, intCol[1]]:phenotype[i, intCol[2]], '1970-1-1'), as.Date(envi$date))
      phenotype$GDD[i] <- sum(envi$gdd[envrows], na.rm = T)
    }
  }
  return(phenotype)
}
