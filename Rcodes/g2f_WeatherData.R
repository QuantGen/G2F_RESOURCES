# Read original files
wdata_G2F <- list()
wdata_G2F[[1]] <- read.csv('Data/EnvironmentalCovariates/G2F_weather_2018.csv')
wdata_G2F[[2]] <- read.csv('Data/EnvironmentalCovariates/G2F_weather_2019.csv')

# Remove imputed from weather networks
wdata_G2F[[1]] <- wdata_G2F[[1]][wdata_G2F[[1]]$Cleaning.Method != 'Imputed',]
wdata_G2F[[2]] <- wdata_G2F[[2]][wdata_G2F[[2]]$Cleaning.Method != 'Imputed',]

# Load function to rename columns
source('Tools/Functions.R')

# Lowercase for all column names
wdata_G2F <- lapply(wdata_G2F, function(x){colnames(x) <- tolower(colnames(x));x})

# Rename columns
rep_matrix <- matrix(c('(record.*number)', NA,
                       '(field.*location)', 'location',
                       '(station.*id)', 'station_id',
                       'date_local', NA,
                       'nws.*network', NA,
                       'nws.*station', NA,
                       '^temperature', 'temp',
                       '(dew.*point)', 'dew_point',
                       '(relative.*humidity)', 'rel_humidity',
                       '(solar.*radiation)', 'solar_rad',
                       'rainfall', 'rainfall',
                       '(wind.*speed)', 'wind_speed',
                       '(wind.*direction)', 'wind_dir',
                       '(wind.*gust)', 'wind_gust',
                       '(soil.*temperature)', 'soil_temp',
                       '(soil.*moisture)', 'soil_moist',
                       '(soil.*ec)', NA,
                       '(uv.*um)', NA,
                       '(par.*um)', NA,
                       '(co2.*ppm)', NA,
                       'photoperiod', NA,
                       '(column.*altered)', NA,
                       '(altered.*column)', NA,
                       '(cleaning.*method)', NA,
                       'comment', NA,
                       '^X$', NA), ncol=2, byrow=T)

# Ignore warnings
for (i in 1:nrow(rep_matrix)) 
  wdata_G2F <- rename_columns(rep_matrix[i,1], rep_matrix[i,2], mydata=wdata_G2F)

# Install and load package to manipulate dates
install.packages('lubridate')
library(lubridate)

# Create a valid date variable
wdata_G2F <- lapply(wdata_G2F, function(data){
  if(lengths(regmatches(data$time[1], gregexpr(":", data$time[1]))) == 1)
    data$time <- paste0(data$time, ':00') # add if missing seconds
  tmp <- matrix(unlist(strsplit(data$time, ':')), ncol=3, byrow=T)
  data$valid <- with(data,
                     date(ISOdate(year, month, day,
                                  hour = as.numeric(tmp[,1]),
                                  min = as.numeric(tmp[,2]),
                                  sec = as.numeric(tmp[,3])
                     )))
  return(data)
})

# Transform weather list to data frame
wdf_G2F <- do.call(rbind, wdata_G2F)

# Remove locations without data
wdf_G2F <- wdf_G2F[wdf_G2F$location %in% names(which(table(wdf_G2F$location) > 10)),]

# Rainfall as numeric
wdf_G2F$rainfall <- as.numeric(wdf_G2F$rainfall)

# Calculate daily temperature
temp <- aggregate(temp ~ valid + location, data = wdf_G2F, FUN = function(x) c(mean(x, na.rm=T), min(x, na.rm=T), max(x, na.rm=T)))
daily_temp <-  data.frame(date = temp$valid, location = temp$location, 
                          temp = temp$temp[,1], temp_min = temp$temp[,2], temp_max = temp$temp[,3])

# Install and load package used to calculate daily rainfall
install.packages('tidyverse')
library(tidyverse)

# Calculate daily rainfall (ignore time zone warnings)
daily_rf <- do.call(rbind, by(wdf_G2F, paste0(wdf_G2F$location, '_', wdf_G2F$year), calculate_daily))
daily_rf$location <- sapply(strsplit(rownames(daily_rf), '_'), function(x) x[1])
rownames(daily_rf) <- NULL

# Merge daily rainfall with temperature
wdaily_G2F <- merge(daily_rf, daily_temp, by = c('date', 'location'))

# Save intermediate file
write.csv(wdaily_G2F, file = 'Data/OutputFiles/G2Fdaily.csv', quote = F, row.names = F)

# Read NASA data and other datasets needed
wd_NASA <- read.csv('Data/OutputFiles/NASAdaily.csv')
pheno <- read.csv('Data/OutputFiles/phenotypes.csv')
info_loc <- read.csv('Data/OutputFiles/info_loc.csv')

# After quality assessment, NASA data is used in cases of missing or corrupted G2F data 
info_loc$source_temp <- c('G2F', 'NASA')[c(2,2,2,2,1,1,1,1,1,2,1,2,2,1,1,1,2,1,1,1,2,1,2,2,2,1,2,2,1,1,2,1,1,1,1,2,1,1,2,1,2,2,2,1,1,2,2,1,1,1,2,2)]
info_loc$source_rain <- c('G2F', 'NASA')[c(2,2,2,2,2,1,1,2,1,2,2,2,2,1,1,1,2,2,1,1,2,1,2,2,2,2,2,2,2,1,2,1,1,1,1,2,2,2,1,1,2,2,2,2,2,2,1,1,1,2,2,2)]

# Plots for quality assessment
wd_final <- list()
pdf('Data/OutputFiles/plots_collection.pdf')

# 3 panels plot
for (i in 1:nrow(info_loc)) {
  iloc <- info_loc[i,]
  idate <- as.character(seq(date(iloc$sowing), date(iloc$harvesting), 1))
  idata <- list(G2F = wd_G2F[wd_G2F$date %in% idate & wd_G2F$location == iloc$Location,],
                NASA = wd_NASA[wd_NASA$date %in% idate & wd_NASA$location == iloc$Location,])
  ipheno <- pheno[pheno$location == iloc$Location & pheno$year == iloc$year,]
  layout(matrix(c(1,2,2,1,3,3), ncol = 2))
  # yield plot
  boxplot(pheno$yield, horizontal = T, xlab = 'Yield (kg/ha)', main = paste0('Location: ', iloc$Location, ' - Year: ', year(idate)[1]))
  abline(v = mean(ipheno$yield, na.rm = T), col = 2, lty = 2)
  text(mean(ipheno$yield, na.rm = T), 1.3, labels = paste0(iloc$Location, ': mean'), col = 2)
  # Temperature plot
  itemp_min <- min(unlist(lapply(idata, function(x)x$temp_min)), na.rm = T)
  itemp_max <- max(unlist(lapply(idata, function(x)x$temp_max)), na.rm = T) * 1.2
  plot(as.Date(idate), rep(0, length(idate)), ylim = c(itemp_min, itemp_max),
       cex = 0, xlab = 'Time (days)', ylab = 'Temperature (°C)',
       main = 'G2F and NASA data')
  for (j in 1:2){
    lines(as.Date(idata[[j]]$date), idata[[j]]$temp, col = j + 1, cex = .7)
    points(as.Date(idata[[j]]$date), idata[[j]]$temp_min, col = j + 1, cex = .7, pch = 3)
    points(as.Date(idata[[j]]$date), idata[[j]]$temp_max, col = j + 1, cex = .7, pch = 4)
  }
  # Average silking
  abline(v = mean(as.Date(ipheno$date_silking), na.rm = T), col = 6, lty = 2)
  # Top-right Legend
  leglab <- c('Symbol:', 'min', 'mean', 'max', 'Silking')
  legtyp <- c('t', 's', 'l', 's', 'l') # t = title, s = symbol, l = line
  legpch <- c(NA, 3, 1, 4, 2) # pch or lty
  legcol <- c(1, 1, 1, 1, 6) # color
  legylim <- c(itemp_min, itemp_max)
  legxlim <- range(as.Date(idate))
  legpos <- c(.7, .85) # Bottom left corner, proportion of x and y axes
  leg_xali <- 1.11 # left indentation
  legcex <- 1 # legend size
  legxline <- 8 # line length
  
  leglow <- (legylim[2] - legylim[1]) * legpos[2] + legylim[1]
  legran <- seq(legylim[2], leglow, length.out = length(leglab))
  legxran <- seq(legxlim[1], legxlim[2], length.out = as.numeric(legxlim[2] - legxlim[1]))
  legxt <- legxran[round(length(legxran) * legpos[1])]
  legx <- legxran[round(length(legxran) * legpos[1] * leg_xali)]
  for (j in 1:length(leglab)) {
    text(switch((legtyp[j] == 't') + 1, legx, legxt), legran[j], 
         labels = leglab[j], pos = 4, cex = legcex)
    if (legtyp[j] == 's')
      points(legx, legran[j], pch = legpch[j], col = legcol[j], cex = legcex)
    if (legtyp[j] == 'l')
      lines(c(legx - round(legxline / 2), legx + round(legxline / 2)), rep(legran[j], 2), lty = legpch[j], col = legcol[j], cex = legcex)
  }
  
  
  # Montly Rainfall bar plots
  irain <- lapply(idata, function(x) x[, c('date', 'rainfall')])
  for (j in which(sapply(irain, nrow) != 0)) irain[[j]]$source <- names(irain)[j]
  irain <- do.call(rbind, irain)
  ibars <- expand.grid(month = unique(month(irain$date)), source = c('G2F', 'NASA'))
  ibars$source <- as.character(ibars$source)
  ibars$rainfall <- 0
  for (z in 1:nrow(ibars)) ibars$rainfall[z] <- sum(irain$rainfall[month(irain$date) == ibars$month[z] & irain$source == ibars$source[z]])
  ibars <- ibars[order(ibars[,1], ibars[,2]),]
  imax <- max(ibars$rainfall) * 1.3
  icolors <- sapply(ibars$source, function(x) switch(x, 'G2F' = 2, 'NASA' = 3))
  plt <- barplot(ibars$rainfall, col = icolors, xlab = 'Month', ylab = 'Monthly rainfall (mm)', ylim = c(0, imax),
                 main = paste0('Rainfall - Period: ', diff(range(as.Date(irain$date))), ' days'))
  axis(1, at = plt, labels = ibars[,1])
  
  # Top-right Legend
  leglab <- c('Color:', 'G2F', 'NASA')
  legtyp <- c('t', 's', 's') # t = title, s = symbol, l = line
  legpch <- c(NA, 19, 19) # pch or lty
  legcol <- c(1, 2, 3) # color
  legylim <- c(0, imax * .9)
  legxlim <- range(plt)
  legpos <- c(.75, .9) # Bottom left corner, proportion of x and y axes
  leg_xali <- 1.11 # left indentation
  legcex <- 1 # legend size
  legxline <- 8 # line length
  
  leglow <- (legylim[2] - legylim[1]) * legpos[2] + legylim[1]
  legran <- seq(legylim[2], leglow, length.out = length(leglab))
  legxran <- seq(legxlim[1], legxlim[2], length.out = as.numeric(legxlim[2] - legxlim[1]))
  legxt <- legxran[round(length(legxran) * legpos[1])]
  legx <- legxran[round(length(legxran) * legpos[1] * leg_xali)]
  for (j in 1:length(leglab)) {
    text(switch((legtyp[j] == 't') + 1, legx, legxt), legran[j], 
         labels = leglab[j], pos = 4, cex = legcex)
    if (legtyp[j] == 's')
      points(legx, legran[j], pch = legpch[j], col = legcol[j], cex = legcex)
    if (legtyp[j] == 'l')
      lines(c(legx - round(legxline / 2), legx + round(legxline / 2)), rep(legran[j], 2), lty = legpch[j], col = legcol[j], cex = legcex)
  }
  
  ######
  wd_final[[i]] <- idata[['NASA']]
  if(iloc$source_temp == 'G2F') {
    wd_final[[i]]$temp <- NA
    wd_final[[i]]$temp_min <- NA
    wd_final[[i]]$temp_max <- NA
    wd_final[[i]][match(idata[['G2F']]$date, wd_final[[i]]$date),c('temp','temp_min','temp_max')] <- idata[['G2F']][,c('temp','temp_min','temp_max')]
    wd_final[[i]]$source_temp <- 'G2F'
  } else {
    wd_final[[i]]$source_temp <- 'NASA'
  }
  if(iloc$source_rain == 'G2F') {
    wd_final[[i]]$rainfall <- NA
    wd_final[[i]][match(idata[['G2F']]$date, wd_final[[i]]$date),'rainfall'] <- idata[['G2F']]$rainfall
    wd_final[[i]]$source_rain <- 'G2F'
  } else {
    wd_final[[i]]$source_rain <- 'NASA'
  }
}
dev.off()

# Save weather data
wd_final <- do.call(rbind, wd_final)

# GDD calculation
wd_final$GDD <- wd_final$temp
wd_final$GDD[wd_final$GDD > 30] <- 30
wd_final$GDD[wd_final$GDD < 10] <- 10
wd_final$GDD <- wd_final$GDD - 10

# Reorder columns
wd_final <- wd_final[,c(1:13,16,14:15)]

write.csv(wd_final, file = 'Data/OutputFiles/G2Fdaily.csv', quote = F, row.names = F)
