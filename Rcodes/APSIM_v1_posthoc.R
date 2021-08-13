# Load packages
library(lubridate)
# install.packages('RColorBrewer')
library(RColorBrewer)

# Read V1 results
load('Data/OutputFiles/simulations/simV1.rdata')

# Simulated environmental variables. Accumulated from sowing-harvesting and average from flowering -30/+15)
keep_cols <- c(8:182, 184:187)
envir_p2 <- envir_p1 <- simV1[[1]][1, keep_cols]
simV1_loc$sim_yield <- NA
simV1_loc$sim_silk <- as.Date(NA)
for (i in 1:nrow(simV1_loc)) {
  x <- simV1[[i]]
  loc <- simV1_loc[i,]
  # Period 1: Sowing-Harvesting
  x_tmp <- x[x$Date >= loc$date_sowing & x$Date <= loc$date_harvesting, keep_cols]
  for (j in 1:ncol(x_tmp)) envir_p1[i,j] <- sum(x_tmp[,j], na.rm = T)
  
  # Period 2: Flowering -30/+15
  silk_date <- x$Clock.Today[x$Maize.Phenology.CurrentStageName == 'Flowering']
  # Silking date in simulation
  simV1_loc$sim_silk[i] <- silk_date
  
  x_tmp <- x[x$Date %in% (silk_date-30):(silk_date+15), keep_cols]
  for (j in 1:ncol(x_tmp)) envir_p2[i,j] <- mean(x_tmp[,j], na.rm = T)
  
  # Simulated yield
  simV1_loc$sim_yield[i] <- max(x$Maize.Grain.Wt) * 10
}

# Plot simulated silking date vs real (color by year)
tmp <- c(as.Date('2014-6-1'), as.Date('2014-9-1'))
loc <- simV1_loc
plot(tmp, tmp, pch = '', xlab = 'Flowering date', ylab = 'Simulated flowering date')
abline(0,1, col = 'grey')
mycolors <- setNames(brewer.pal(length(unique(loc$year)), 'Dark2'), nm = unique(loc$year))
for (i in unique(loc$year)) {
  tmp1 <- with(loc, date_silking[year == i]) - ((i - 2014) * 365) - 1
  tmp2 <- with(loc, sim_silk[year == i]) - ((i - 2014) * 365) - 1
  points(tmp1, tmp2, col = mycolors[as.character(i)], pch = 19)
}
legend('bottomright', legend = names(mycolors), col = mycolors, pch = 19)

# plot location cluster
# cluster GDD
layout(1)
library(maps)
map('state', col = "grey", fill = TRUE, bg = "white", xlim = c(-108, -70))
with(simV1_loc, points(lon, lat, col = as.numeric(as.factor(cluster)) + 1, pch = 19))
legend('bottomright', legend = paste(c('a', 'b', 'c'), round(unique(simV1_loc$gdd_silk))), pch = 19, col = 2:4, title = 'Cluster')


# Plot Yield with conditional probabilities
# Function to plot
plot_conditional <- function(x, y, quantiles = c(.2, .5, .8), xpos = .03, ypos = .05, ...) {
  xq <- quantile(x, probs = quantiles)
  yq <- quantile(y, probs = quantiles)
  plot(x, y, col = 'grey', ...)
  abline(0,1, col = 2)
  
  for (i in 1:length(xq)) {
    abline(h = yq[i], col = 4, lty = 3)
    abline(v = xq[i], col = 4, lty = 3)
    #
    chr_x_space <- diff(par("usr")[1:2]) * xpos
    chr_y_space <- diff(par("usr")[3:4]) * ypos
    tmp_x_pos <- par("usr")[1] + nchar(names(yq)[i]) * chr_x_space
    tmp_y_pos <- par("usr")[3] + chr_y_space
    #
    rect(xleft = par("usr")[1] + chr_x_space * .3, xright = tmp_x_pos, 
         ybottom = yq[i] - chr_y_space, yq[i] + chr_y_space, col = 'white', border = NA)
    text(tmp_x_pos, yq[i], labels = names(yq)[i], col = 4, pos = 2)
    #
    rect(xleft = xq[i] - chr_x_space, xright = xq[i] + chr_x_space, 
         ybottom = tmp_y_pos - chr_y_space, tmp_y_pos + chr_y_space, col = 'white', border = NA)
    text(xq[i], tmp_y_pos, labels = names(xq)[i], col = 4)
  }
  x_cut <- cut(x, c(min(x)-1, xq, max(x)))
  y_cut <- cut(y, c(min(y)-1, yq, max(y)))
  for (i in 1:length(levels(x_cut))) {
    for (j in 1:length(levels(y_cut))) {
      tmp <- x_cut == levels(x_cut)[i] & y_cut == levels(y_cut)[j]
      tmp_x_lim <- as.numeric(gsub('\\[|\\]|[()]','', unlist(strsplit(levels(x_cut)[i], ','))))
      tmp_y_lim <- as.numeric(gsub('\\[|\\]|[()]','', unlist(strsplit(levels(y_cut)[j], ','))))
      text(mean(tmp_x_lim), mean(tmp_y_lim), round(sum(tmp) / table(x_cut)[i], 3))
    }
  }
}

# Plot simulated yield vs real yield
with(simV1_loc, plot_conditional(sim_yield, yield, main = paste0('V1; Correlation = ', round(cor(yield, sim_yield), 2)), 
                     ylab = 'Yield (kg/ha)', xlab = 'Simulated yield (kg/ha)'))

# Remove NAs in environmental covariates
envir_p1 <- envir_p1[,colSums(is.na(envir_p1)) == 0]
envir_p2 <- envir_p2[,colSums(is.na(envir_p2)) == 0]

# Remove environmental covariates that are constant
envir_p1 <- envir_p1[,names(which(sapply(envir_p1, var) != 0))]
envir_p2 <- envir_p2[,names(which(sapply(envir_p2, var) != 0))]

# Remove environmental covariates with correlation > .98
cormat_p1 <- cor(envir_p1)
diag(cormat_p1) <- 0
envir_p1 <- envir_p1[,names(which(colSums(cormat_p1 > .98) == 0))]
cormat_p2 <- cor(envir_p2)
diag(cormat_p2) <- 0
envir_p2 <- envir_p2[,names(which(colSums(cormat_p2 > .98) == 0))]

pairs(envir_p1[,1:10], col = as.numeric(as.factor(simV1_loc$cluster)) + 1, pch = 19)
pairs(envir_p2[,1:10], col = as.numeric(as.factor(simV1_loc$cluster)) + 1, pch = 19)

# How many environmental covariates?
ncol(envir_p1)
ncol(envir_p2)

write.table(simV1_loc, file = paste0("Data/OutputFiles/simulated_data_v1.csv"), sep = ",", row.names = FALSE)
write.table(envir_p1, file = paste0("Data/OutputFiles/envir_p1.csv"), sep = ",", row.names = FALSE)
write.table(envir_p2, file = paste0("Data/OutputFiles/envir_p2.csv"), sep = ",", row.names = FALSE)
