### Calculate GDD and predict flowering date

The following code reads the consensus weather data and the phenotype data to calculate growing degree days (GDD) and predict the average GDD needed to flowering.

```r
# Load function to calculate GDD
source('Tools/Functions.R')

# Read the consensus weather data
consensus <- read.csv('Data/OutputFiles/ConsensusData.csv')

# Read the phenotype data
phen1 <- read.csv('Data/OutputFiles/phenotypes.csv')

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

# Plot GDD vs Flowering date by hybrid
colors <- as.factor(phen$source)
levels(colors) <- rainbow(length(levels(colors)))
plot(phen$GDD, phen$date_silking, col = as.character(colors), xlab = 'accumulated GDD', ylab = 'Flowering date', main = 'colors by hybrid')
```
![Alt text](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/PredictFlowering/Plot1.pdf)

```r
# Plot GDD vs Flowering date by location
colors <- as.factor(phen$location)
levels(colors) <- rainbow(length(levels(colors)))
plot(phen$GDD, phen$date_silking, col = as.character(colors), xlab = 'accumulated GDD', ylab = 'Flowering date', main = 'colors by location')

# There is no hybrid effect
# There is a strong environmental effect on the intercept, but the slope is the same

# Install lme4 package for linear mixed models
install.packages('lme4')
library(lme4)
phen$numFlowering <- as.numeric(phen$date_silking)
# centering flowering
phen$numFlowering <- scale(phen$numFlowering, scale = F)

# Model to predict Flowering
mod1 <- lmer(numFlowering ~ GDD + (1 | env), data = phen)
summary(mod1)

# Linear mixed model fit by REML ['lmerMod']
# Formula: numFlowering ~ GDD + (1 | env)
# Data: phen

# REML criterion at convergence: 139267.5

# Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-5.8343 -0.2709 -0.0175  0.2793 10.4979 

# Random effects:
#   Groups   Name        Variance  Std.Dev.
#   env      (Intercept) 36964.617 192.262 
#   Residual                 2.798   1.673 
#   Number of obs: 35837, groups:  env, 43

# Fixed effects:
#             Estimate    Std. Error  t value
# (Intercept) -6.354e+01  2.932e+01   -2.167
# GDD          9.725e-02  1.218e-04   798.126

# Correlation of Fixed Effects:
# (Intr)
# GDD -0.003

# Check observed vs predicted data
phen$pred <- predict(mod1)
plot(phen$numFlowering, phen$pred, col = colors, xlab = 'Observed data', ylab = 'Predicted data')

# Save results
save(mod1, phen, file = 'Data/PredictFlowering/PredictFlowering.rdata')
```
