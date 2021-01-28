### Generate a pdf with plots of weather data by trial

The following code produces a series of plots to examine the different sources of weather data. 
Each plot by trial (year-location combination) consists of 3 panels: 
1) Upper boxplot with yield for all trials and average yield for the trial of interest.
2) left scatter plot of temperature by weather station (red: G2F, green: ASOS, blue: NOAA). The title contains the G2F-ASOS distance and G2F-NOAA distance.
3) right bar plot of monthly accumulated rainfall by weather station. The title contains the number of days from sowing to harvesting and the final source of weather data selected based on visual assessment.

All plots are collected into a pdf file: [plots_collection.pdf](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/plots_collection.pdf)

After visual examination, we selected the best source of data for each trial and save it in [wdaily_final.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/wdaily_final.csv)

```r
# Read and format data
wdaily_ASOS <- read.csv('OutputFiles/ASOSdaily.csv')
wdaily_NOAA <- read.csv('OutputFiles/NOAAdaily.csv')
wdaily_G2F <- read.csv('OutputFiles/G2Fdaily.csv')
pheno <- read.csv('OutputFiles/phenotypes.csv')
info_loc <- read.csv('OutputFiles/info_loc.csv')

# load library
library(lubridate)

# Distance plot
pdf('OutputFiles/plots_collection.pdf')
plot(0,0, 
     xlab = 'Distance to ASOS station (km)',
     ylab = 'Distance to NOAA station (km)',
     xlim = range(info_loc$ASOSdist),
     ylim = range(info_loc$NOAAdist))
abline(0, 1, col = 2, lwd = 2)
with(info_loc, points(ASOSdist, NOAAdist, pch = 19, col = 4, cex = .7))
with(info_loc, text(ASOSdist, NOAAdist + 2, Location, cex = .8))

# Selected source
info_loc$source <- NA
sASOS <- c(1,3,18,21,25,26,37,39,40,41,43,52)
sG2F <- c(2,6,7,9,14,15,16,17,19,20,24,27,28,30,32,33,34,35,44,45,48,49,50,51)
sNOAA <- c(4,5,8,10,11,12,13,22,23,29,31,36,38,42,46,47)
info_loc$source[sG2F] <- 'G2F'
info_loc$source[sASOS] <- 'ASOS'
info_loc$source[sNOAA] <- 'NOAA'

# Final weather data
wdaily_final <- list()

# 3 panels plot
for (i in 1:nrow(info_loc)) {
  iloc <- info_loc[i,]
  idate <- as.character(seq(date(iloc$sowing), date(iloc$harvesting), 1))
  idata <- list(G2F = wdaily_G2F[wdaily_G2F$date %in% idate & wdaily_G2F$location == iloc$Location,],
                ASOS = wdaily_ASOS[wdaily_ASOS$date %in% idate & wdaily_ASOS$location == iloc$Location,],
                NOAA = wdaily_NOAA[wdaily_NOAA$date %in% idate & wdaily_NOAA$location == iloc$Location,])
  wdaily_final[[i]] <- idata[[switch(iloc$source, 'G2F' = 1, 'ASOS' = 2, 'NOAA' = 3)]]
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
       main = paste0('ASOS dist.: ', round(iloc$ASOSdist, 1), ' km - NOAA dist.: ', round(iloc$NOAAdist, 1), ' km'))
  for (j in 1:3){
    lines(as.Date(idata[[j]]$date), idata[[j]]$temp, col = j + 1, cex = .7)
    points(as.Date(idata[[j]]$date), idata[[j]]$temp_min, col = j + 1, cex = .7, pch = 3)
    points(as.Date(idata[[j]]$date), idata[[j]]$temp_max, col = j + 1, cex = .7, pch = 4)
  }
  # Average silking
  abline(v = date(mean(strptime(ipheno$date_silking, "%m/%d/%y"), na.rm = T)), col = 6, lty = 2)
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
  ibars <- aggregate(rainfall ~ month(as.Date(date)) + source, data = irain, sum)
  ibars <- ibars[order(ibars[,1], ibars[,2]),]
  imax <- max(ibars$rainfall) * 1.25
  icolors <- sapply(ibars$source, function(x) switch(x, 'G2F' = 2, 'ASOS' = 3, 'NOAA' = 4))
  plt <- barplot(ibars$rainfall, col = icolors, xlab = 'Month', ylab = 'Monthly rainfall (mm)', ylim = c(0, imax),
                 main = paste0('Period: ', diff(range(as.Date(irain$date))), ' days - Selected source: ', iloc$source))
  axis(1, at = plt, labels = ibars[,1])
  
  # Top-right Legend
  leglab <- c('Color:', 'G2F', 'ASOS', 'NOAA')
  legtyp <- c('t', 's', 's', 's') # t = title, s = symbol, l = line
  legpch <- c(NA, 19, 19, 19) # pch or lty
  legcol <- c(1, 2, 3, 4) # color
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
}
dev.off()

# overwrite info_loc.csv with changes
write.csv(info_loc, file = '../Data/OutputFiles/info_loc.csv', quote = F, row.names = F)

# Save final weather data
wdaily_final <- do.call(rbind, wdaily_final)
write.csv(wdaily_final, file = '../Data/OutputFiles/wdaily_final.csv', quote = F, row.names = F)
```

