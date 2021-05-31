# Install and load needed package
#install.packages('nasapower')
library(nasapower)

allpars <- do.call(rbind, lapply(parameters, function(x){
  data.frame(description = x$longname,
             daily = 'DAILY' %in% x$include,
             AG = 'AG' %in% x$community,
             unit = paste(x$SB_Units))
}))

# Information for NSRDB
api_key = 'JvlZ7aZ5EdAec3wGCvTqGuiCcDAmoMpOhff03Dcg'
attributes = 'ghi,dhi,dni'
leap_year = 'false'
interval = '60'  # per hour data
utc = 'false'
your_name = 'Marco+LopezCruz'
reason_for_use = 'beta+testing'
your_affiliation = 'Michigan+State+University'
your_email = 'lopezcru@msu.com'
mailing_list = 'true'
url00 <- paste0("https://developer.nrel.gov/api/solar/nsrdb_psm3_download.csv?wkt=POINT(",
  lon,"%20",lat,")&names=",year,"&leap_day=",leap_year,"&interval=",interval,"&utc=",utc,
  "&full_name=",your_name,"&email=",your_email,"&affiliation=",your_affiliation,
  "&mailing_list=",mailing_list,"&reason=",reason_for_use,"&api_key=",api_key,"&attributes=",attributes)

# Variables of interest
varInt <- c('PRECTOT','T2M','T2M_MIN','T2M_MAX','ALLSKY_SFC_SW_DWN','PS','RH2M','T2MDEW','T2MWET','WS2M','DNR')

AGvar <- allpars[varInt,]

# Load location data
info_loc <- read.csv('Data/OutputFiles/info_loc.csv')

# as.Date format
info_loc$sowing <- as.Date(info_loc$sowing)
info_loc$harvesting <- as.Date(info_loc$harvesting)

# Get data for all locations
data1 <- list()
for (i in 1:nrow(info_loc)) {
  location = info_loc$Location[i]
  tmp <- as.data.frame(get_power(community = 'AG',
                                 lonlat = c(info_loc$lon[i], info_loc$lat[i]),
                                 pars = rownames(AGvar)[AGvar$daily],
                                 dates = c(info_loc$sowing[i], info_loc$harvesting[i]),
                                 temporal_average = 'DAILY'))
  tmp2 <- as.data.frame(t(get_power(community = 'AG',
                                 lonlat = c(info_loc$lon[i], info_loc$lat[i]),
                                 pars = rownames(AGvar)[!AGvar$daily],
                                 dates = c(info_loc$sowing[i], info_loc$harvesting[i]),
                                 temporal_average =  'CLIMATOLOGY')))
  tmp3 <- tmp2[tmp$MM+3,,drop=F]
  colnames(tmp3) <- tmp2['PARAMETER',]
  tmp3 <- cbind(location,tmp,tmp3)

  # Data from NSRDB
  url0 = paste0("https://developer.nrel.gov/api/solar/nsrdb_psm3_download.csv?wkt=POINT(",
    info_loc$lon[i],"%20",info_loc$lat[i],")&names=",info_loc$year[i],"&leap_day=",leap_year,"&interval=",interval,"&utc=",utc,
    "&full_name=",your_name,"&email=",your_email,"&affiliation=",your_affiliation,
    "&mailing_list=",mailing_list,"&reason=",reason_for_use,"&api_key=",api_key,"&attributes=",attributes)

  tmp4 = read.csv(url(url0),sep=",",skip=2)

  # Keep only dates between sowing and hasvest
  tmp3$DNI <- NULL
  tmp4$YYYYMMDD <- paste(tmp4$Year,sprintf("%02d",tmp4$Month),sprintf("%02d",tmp4$Day),sep="-")
  for(dd in 1:nrow(tmp3)){
    tmp <- tmp4[tmp4$YYYYMMDD %in% as.character(tmp3$YYYYMMDD[dd]),]
    tmp3$DNI[dd] <- sum(tmp$DNI)*0.0036  #(1WH = 0.03336 MJ)
  }

  data1[[i]] <- tmp3
  message(i, ' loc = ', info_loc$Location[i], ' done.')
}
data2 <- do.call(rbind, data1)

# Format row and col names
rownames(data2) <- NULL
data2 <- data2[,c(8,1,9:ncol(data2))]
colnames(data2)[c(1, 3:6)] <- c('date', 'rainfall', 'temp', 'temp_min', 'temp_max')
colnames(data2) <- casefold(colnames(data2))

# Save data
write.csv(data2, file = 'Data/OutputFiles/NASA_WeatherData.csv', quote = F, row.names = F)

