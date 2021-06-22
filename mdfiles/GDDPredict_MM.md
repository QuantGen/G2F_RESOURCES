```
source('Tools/Functions.R')

library(lme4)
library(BGLR)

githubsite <- "https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Data"

token <- "ADFBAYYBP6NZ6ONGVTYNGQLA3J4EK"
#pheno <- read.csv(url(paste0(githubsite,"/OutputFiles/phenotypes.csv?token=",token)),stringsAsFactors=FALSE)
pheno <- read.csv("Data/OutputFiles/phenotypes.csv",stringsAsFactors=FALSE)

token <- "ADFBAY56SWSUE465PTBHRJTA3J354"
#wea <- read.csv(url(paste0(githubsite,"/APSIM_sim/Weather_data.csv?token=",token)),stringsAsFactors=FALSE)
wea <- read.csv("Data/APSIM_sim/Weather_data.csv",stringsAsFactors=FALSE)

token <- "ADFBAY6JYDRBP3N456CVXD3A3J3XE"
#g2f_loc <- read.csv(url(paste0(githubsite,"/OutputFiles/info_loc.csv?token=",token)),stringsAsFactors=FALSE)
g2f_loc <- read.csv("Data/OutputFiles/info_loc.csv",stringsAsFactors=FALSE)

g2f_loc <- g2f_loc[g2f_loc$Location != 'ONH2',]
g2f_loc <- g2f_loc[!(g2f_loc$Location %in% c('ARH1','GAH1','GAH2','KSH1','TXH1','TXH2') & g2f_loc$year == 2018),]
g2f_loc <- g2f_loc[!(g2f_loc$Location %in% c('NCH1','GAH1','TXH1','TXH4') & g2f_loc$year == 2019),]
rownames(g2f_loc) <- NULL

pheno <- pheno[!is.na(pheno$date_plant) & !is.na(pheno$date_silking) & !is.na(pheno$date_harvest) & !is.na(pheno$yield),]
# FIX HARVEST DATE IN PHENOTYPE
# tmp <- year(pheno$date_harvest) == 2016
tmp <- format(as.Date(as.character(pheno$date_harvest)), format="%Y") ==2016
tmp <- format(as.Date(pheno$date_harvest), format="%Y") ==2016
pheno$date_harvest[tmp] <- gsub(2016, pheno$year[tmp][1], pheno$date_harvest[tmp])

pheno1 <- list()
for (i in 1:nrow(g2f_loc)) {
  print(i)
  site_name <- g2f_loc$Location[i]
  site_coords <- c(g2f_loc$lon[i], g2f_loc$lat[i])
  site_period <- as.Date(c(g2f_loc$sowing[i], g2f_loc$harvesting[i]))
  # site_year <- year(site_period)[1]
  site_year <- format(as.Date(site_period), format="%Y")[1]
  site_wea <- wea[wea$site_name == site_name & wea$year == site_year,]
  site_wea$Date <- as.Date(paste0(site_year,'-1-1')) + site_wea$day - 1
  tmp <- pheno$location == site_name & pheno$year == site_year
  if(any(tmp)) {
    site_pheno <- pheno[tmp,]
    site_pheno$gdd_silk <- NA
    site_pheno$gdd_harv <- NA
    for (j in 1:nrow(site_pheno)) {
      index1 <- site_wea$Date >= as.Date(site_pheno$date_plant[j])  
      site_pheno$gdd_silk[j] <- sum(site_wea$gdd[index1 & site_wea$Date <= as.Date(site_pheno$date_silking[j])])
      site_pheno$gdd_harv[j] <- sum(site_wea$gdd[index1 & site_wea$Date <= as.Date(site_pheno$date_harvest[j])])
    }
    pheno1[[i]] <- site_pheno
  }
}
pheno2 <- do.call(rbind, pheno1)
# remove wrong dates
pheno2 <- pheno2[pheno2$gdd_silk != 0,]
pheno2 <- pheno2[pheno2$gdd_harv != 0,]

pheno2$year <- as.factor(as.character(pheno2$year))
pheno2$location <- as.factor(as.character(pheno2$location))
pheno2$source <- as.factor(as.character(pheno2$source))

# For BGLR
YY <- model.matrix(~1+ pheno2$year)
LL <- model.matrix(~1+ pheno2$location)
HH <- model.matrix(~1+ pheno2$source)
#KLL <- tcrossprod(LL); sdvLL <- svd(KLL)
#KHH <- tcrossprod(HH); sdvHH <- svd(KHH)

## Run LMER for GDD to flowering 
mod1 <- lmer(gdd_silk ~ year + (1|location) + (1|source), data = pheno2)
mean0 <- as.vector(fixef(mod1)["(Intercept)"])
mod1_blup <- mean0 + ranef(mod1)$source
VarCorr(mod1)
```
> VarCorr(mod1)
 Groups   Name        Std.Dev.
 source   (Intercept) 27.111  
 location (Intercept) 61.244  
 Residual             35.593  
 
```
## Run LMER for GDD to harvesting
mod2 <- lmer(gdd_harv ~ year + (1|location) + (1|source), data = pheno2)
mean0 <- as.vector(fixef(mod2)["(Intercept)"])
mod2_blup <- mean0 + ranef(mod2)$source
VarCorr(mod2)
```
> VarCorr(mod2)
 Groups   Name        Std.Dev.  
 source   (Intercept) 4.3013e-04
 location (Intercept) 3.1328e+02
 Residual             3.8403e+01
 
```
dat2 <- aggregate(gdd_silk~source,data=pheno2,mean)
dat3 <- aggregate(gdd_harv~source,data=pheno2,mean)
if(sum(dat2$source != dat3$source) >0) stop("Matching error")

dat2 <- data.frame(dat2,dat3[,2,drop=FALSE])
dat2$gdd_silk_hat <- mod1_blup[match(dat2$source,rownames(mod1_blup)),1]
dat2$gdd_harv_hat <- mod2_blup[match(dat2$source,rownames(mod2_blup)),1]

plot(dat2[,grep("gdd_silk",colnames(dat2))])
plot(dat2[,grep("gdd_harv",colnames(dat2))])
```
<p align="center">
<img src="https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/Images/trash1.png" width="450">
</p>

<p align="center">
<img src="https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/Images/trash2.png" width="450">
</p>

write.csv(dat2,"Data/OutputFiles/GDD_hat_by_hybrids.csv",row.names=F)

