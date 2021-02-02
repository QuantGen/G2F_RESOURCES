
The following code reads the consensus weather data and the phenotype data to calculate growing degree days (GDD) and predict the average GDD needed to flowering.

```r
# Load consensus weather data
wdaily <- read.csv('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Data/OutputFiles/wdaily_final.csv?')

# Calculate GDD
mean_temp <- wdaily$temp
# Base temperature 10°C (50°F)
mean_temp[mean_temp < 10] <- 10
# Corn cutoff temperature 30°C (86°F)
mean_temp[mean_temp > 30] <- 30
wdaily$gdd <- mean_temp - 10

# Load phenotype
pheno <- read.csv('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Data/OutputFiles/phenotypes.csv')

# Character to date
pheno$date_plant <- as.Date(pheno$date_plant)
wdaily$date <- as.Date(wdaily$date)

# Select 2018 and 2019
library(lubridate)
pheno <- pheno[year(pheno$date_plant) %in% c(2018, 2019),]

# GDD from sowing to R1
gdd_r1 <- seq(1300, 1500, 10)

# Matrix with predictions
pred_R1 <- matrix(NA, nrow = nrow(pheno), ncol = length(gdd_r1))
colnames(pred_R1) <- paste0('gdd', gdd_r1)
for (i in 1:nrow(pheno)) {
  iweather <- wdaily[wdaily$location == pheno$location[i] & wdaily$date > pheno$date_plant[i],]
  igdd <- cumsum(iweather$gdd)
  for (j in 1:ncol(pred_R1)) {
    pred_R1[i,j] <- iweather$date[which.min(abs(igdd - gdd_r1[j]))]
  }
}

# as.Date(pred_R1[i,j], origin = "1970-01-01")

# Linear mixed model to select the needed GDD from sowing to R1
library(lme4)
for (i in 1:ncol(pred_R1)) {
  data <- data.frame(date_silking = as.numeric(as.Date(pheno$date_silking)), 
                     pred_r1 = pred_R1[,i], 
                     location = pheno$location)
  AIC_vector[i] <- AIC(lmer(date_silking ~ pred_r1 + (1|location), data = data))
}

gdd_r1[which.min(AIC_vector)] # on average, 1340 GDD are needed from sowing to R1
```
