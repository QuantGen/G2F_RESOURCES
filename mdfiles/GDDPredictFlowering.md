### Calculate GDD and predict GDD needed to flowering

The following code uses the "calcGDD" function (in tools) to calculate growing degree days (GDD) and fits a linear mixed model to predict GDD to flowering for the hybrids tested by the G2F project.

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
#install.packages('lubridate')
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
# install.packages('lme4')
library(lme4)

# remove NAs in source (hybrid)
phen <- phen[!is.na(phen$source),]
# year as factor
phen$fyear <- as.factor(phen$year)

# Model to predict GDD to flowering
mod1 <- lmer(GDD ~  -1 + fyear + (1|source) + (1 | location) + (1| source:location) + (1| env:rep), data = phen)


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


