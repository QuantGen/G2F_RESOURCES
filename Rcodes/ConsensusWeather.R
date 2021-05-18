# Read and format data
wd_G2F <- read.csv('Data/OutputFiles/G2Fdaily.csv')
wd_NASA <- read.csv('Data/OutputFiles/NASAdaily.csv')
pheno <- read.csv('Data/OutputFiles/phenotypes.csv')
info_loc <- read.csv('Data/OutputFiles/info_loc.csv')

# load library
library(lubridate)

# Selected source
info_loc$source_temp <- c('G2F', 'NASA')[c(2,2,2,2,1,1,1,1,1,2,1,2,2,1,1,1,2,1,1,1,2,1,2,2,2,1,2,2,1,1,2,1,1,1,1,2,1,1,2,1,2,2,2,1,1,2,2,1,1,1,2,2)]
info_loc$source_rain <- c('G2F', 'NASA')[c(2,2,2,2,2,1,1,2,1,2,2,2,2,1,1,1,2,2,1,1,2,1,2,2,2,2,2,2,2,1,2,1,1,1,1,2,2,2,1,1,2,2,2,2,2,2,1,1,1,2,2,2)]


# Final weather data
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
       cex = 0, xlab = 'Time (days)', ylab = 'Temperature (Â°C)',
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

write.csv(info_loc, file = 'Data/OutputFiles/info_loc.csv', quote = F, row.names = F)
# Save weather data
wd_final <- do.call(rbind, wd_final)

# GDD calculation
wd_final$GDD <- wd_final$temp
wd_final$GDD[wd_final$GDD > 30] <- 30
wd_final$GDD[wd_final$GDD < 10] <- 10
wd_final$GDD <- wd_final$GDD - 10

# Reorder columns
wd_final <- wd_final[,c(1:13,16,14:15)]

write.csv(wd_final, file = 'Data/OutputFiles/ConsensusData.csv', quote = F, row.names = F)
