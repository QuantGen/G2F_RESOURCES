### Calculate GDD and predict GDD needed to flowering

The following code uses the "calcGDD" function (in tools) to calculate growing degree days (GDD) and fits a linear mixed model to predict GDD needed to silking of the hybrids tested by the G2F project.

```r
# Load function to calculate GDD
source('Tools/Functions.R')

# Read the consensus weather data
consensus <- read.csv('Data/OutputFiles/ConsensusData.csv')

# Read the phenotype data
phen1 <- read.csv('Data/OutputFiles/phenotypes.csv')
# remove NAs in source (hybrid)
phen1 <- phen1[!is.na(phen1$source),]

# Keep years 2018 and 2019
phen1 <- phen1[phen1$year %in% c(2018, 2019),]

# Install package for reading dates
install.packages('lubridate')
library(lubridate)

# Create environment
phen1$env <- paste0(phen1$year, phen1$location)
consensus$env <- paste0(year(consensus$date), consensus$location)

# Use the GDD function to calculate the GDD needed from sowing to flowering
phen <- calcGDD(consensus, phen1)

# Plot GDD vs Flowering date by location
colors <- as.factor(phen$location)
levels(colors) <- rainbow(length(levels(colors)))
plot(phen$GDD, phen$date_silking, col = as.character(colors), xlab = 'accumulated GDD', ylab = 'Flowering date', main = 'colors by location')
```
![](/Data/GDDtoFlowering/Plot1.png)
```r
# Install lme4 package for linear mixed models
install.packages('lme4')
library(lme4)

# year as factor
phen$fyear <- as.factor(phen$year)

# Model to predict GDD to flowering
mod1 <- lmer(GDD ~  -1 + fyear + (1|source) + (1 | location) + (1| source:location) + (1| env:rep), data = phen)
summary(mod1)
# Linear mixed model fit by REML ['lmerMod']
# Formula: GDD ~ -1 + fyear + (1 | source) + (1 | location) + (1 | source:location) +      (1 | env:rep)
# Data: phen

# REML criterion at convergence: 389075.2

# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -13.9368  -0.2957  -0.0169   0.2720   5.3451 

# Random effects:
# Groups          Name        Variance Std.Dev.
# source:location (Intercept)   23.45   4.843  
# source          (Intercept)  281.13  16.767  
# env:rep         (Intercept) 1122.75  33.507  
# location        (Intercept) 6679.30  81.727  
# Residual                    4563.54  67.554  
# Number of obs: 34360, groups:  source:location, 23056; source, 2541; env:rep, 89; location, 24

# Fixed effects:
#           Estimate Std. Error t value
# fyear2018   794.58      17.48   45.45
# fyear2019   799.24      17.56   45.51

# Correlation of Fixed Effects:
#          fy2018
# fyear2019 0.904


# Check observed vs predicted data
phen$pred <- predict(mod1)
plot(phen$GDD, phen$pred, col = colors, xlab = 'Observed data', ylab = 'Predicted data')
```
![](/Data/GDDtoFlowering/Plot2.png)
```r
# Example with one hybrid in different locations (on an average year)
hybrid <- unique(phen$source)[10] # hybrid = 17SGTF:LH195:0983
locs <- unique(phen$location)
# GDD needed to flowering on each environment
eff_sf <- ranef(mod1)$'source:location'[paste0(hybrid, ':', locs),]
eff_sf[is.na(eff_sf)] <- 0
eff_s <- ranef(mod1)$source[hybrid,]
eff_f <- ranef(mod1)$location[locs,]
hGDD <- mean(fixef(mod1)) + eff_s + eff_f + eff_sf

plot(hGDD, xaxt = 'n', xlab = 'location', ylab = 'GDD to silking', main = paste('hybrid =', hybrid, 'on an average year'))
axis(1, at = 1:length(locs), labels = locs, las = 2, cex.axis = .8)
```
![](/Data/GDDtoFlowering/Plot3.png)
```r
# Save results
save(mod1, phen, file = 'Data/GDDtoFlowering/data_model.rdata')
```


