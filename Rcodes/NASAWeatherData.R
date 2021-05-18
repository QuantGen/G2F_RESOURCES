# Install and load needed package
install.packages('nasapower')
library(nasapower)

allpars <- do.call(rbind, lapply(parameters, function(x){
  data.frame(description = x$longname, 
             daily = 'DAILY' %in% x$include, 
             AG = 'AG' %in% x$community, 
             unit = paste(x$SB_Units))
}))

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
  
  data1[[i]] <- cbind(location,tmp,tmp3)  
  message(i, ' loc = ', info_loc$Location[i], ' done.')
}
data2 <- do.call(rbind, data1)

# Format row and col names
rownames(data2) <- NULL
data2 <- data2[,c(8,1,9:ncol(data2))]
colnames(data2)[c(1, 3:6)] <- c('date', 'rainfall', 'temp', 'temp_min', 'temp_max')
colnames(data2) <- casefold(colnames(data2))

# Save data
write.csv(data2, file = 'Data/OutputFiles/NASAdaily.csv', quote = F, row.names = F)
