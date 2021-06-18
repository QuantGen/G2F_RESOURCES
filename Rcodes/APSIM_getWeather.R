# Load functions
source('Tools/Functions.R')
library(apsimx)

# Load G2F location info
g2f_loc <- read.csv('Data/OutputFiles/info_loc.csv')

# Remove locations with irrigation
g2f_loc <- g2f_loc[g2f_loc$Location != 'ONH2',]
g2f_loc <- g2f_loc[!(g2f_loc$Location %in% c('ARH1','GAH1','GAH2','KSH1','TXH1','TXH2') & g2f_loc$year == 2018),]
g2f_loc <- g2f_loc[!(g2f_loc$Location %in% c('NCH1','GAH1','TXH1','TXH4') & g2f_loc$year == 2019),]
rownames(g2f_loc) <- NULL

# Download weather data
weather_data <- list()
for (i in 1:nrow(g2f_loc)) {
  site_name <- g2f_loc$Location[i]
  site_coords <- c(g2f_loc$lon[i], g2f_loc$lat[i])
  site_period <- as.Date(c(g2f_loc$sowing[i], g2f_loc$harvesting[i]))
  imet <- get_power_apsim_met(site_coords, dates = site_period + c(-60, 60))
  # Impute missing data
  imet$radn <- ifelse(imet$radn == -99, NA, imet$radn)
  if (any(is.na(imet))) imet <- impute_apsim_met(imet)
  weather_data[[i]]  <- cbind(site_name, as.data.frame(imet))
  print(i)
}

# Calculate GDD
weather_data <- lapply(weather_data, function(x) {
  mean_temp <- (x$maxt + x$mint) / 2
  mean_temp[mean_temp < 10] <- 10
  mean_temp[mean_temp > 30] <- 30
  x$gdd <- mean_temp - 10
  return(x)  
})
weather_data <- do.call(rbind, weather_data)
write.csv(weather_data, file = 'APSIM_sim/Weather_data.csv', row.names = F, quote = F)
