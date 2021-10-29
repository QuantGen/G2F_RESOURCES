
rm(list=ls())
setwd("~/Dropbox/projects/G2F_RESOURCES")

library(lubridate)

# Load all data
GDD_gp <- read.csv("Data/loc_GDD_clusters.csv")
info_loc <- read.csv('Data/OutputFiles/info_loc.csv')
pheno <- read.csv('Data/OutputFiles/phenotypes.csv')

# Remove locations with irrigation
info_loc <- info_loc[!info_loc$Irrigated,]

# Change date format
info_loc$sowing <- as.Date(info_loc$sowing, format = '%m/%d/%Y')
info_loc$harvesting <- as.Date(info_loc$harvesting, format = '%m/%d/%Y')

# GDD to flowering by cluster
info_loc$cluster <- GDD_gp[match(info_loc$location,GDD_gp$location),'cluster']
info_loc$gdd_silk <- GDD_gp[match(info_loc$location,GDD_gp$location),'GDD']

# Change some names in pheno data
colnames(pheno)[grep("date_plant",colnames(pheno))] <- "sowing"
colnames(pheno)[grep("date_harvest",colnames(pheno))] <- "harvesting"
colnames(pheno)[grep("date_anthesis",colnames(pheno))] <- "anthesis"
colnames(pheno)[grep("date_silking",colnames(pheno))] <- "silking"

# Fix pheno data
drop <- which(is.na(pheno$sowing) | is.na(pheno$silking) | is.na(pheno$anthesis) | is.na(pheno$harvesting))
if(length(drop) >0 ) pheno <- pheno[-drop,]
rownames(pheno) <- NULL

# Check dates
dateNames <- c("sowing","harvesting")
dd <- as.Date(pheno[,dateNames[2]]) - as.Date(pheno[,dateNames[1]])  # Sowing > Harvest
index <- which(dd < 0 | pheno[,"year"] != year(pheno[,dateNames[2]]))
drop <- tofix <- c()
if(length(index)>0){
  for(i in 1:length(index)){
    year0 <- pheno[index[i],"year"]
    yy <- as.character(year(as.matrix(pheno[index[i],dateNames])))
    if(yy[1] == year0){
        tofix <- rbind(tofix,pheno[index[i],c('year','location',dateNames)])
        pheno[index[i],dateNames[2]] <- gsub(yy[2],year0,pheno[index[i],dateNames[2]])
    }else{
      if(yy[2] == year0){
        tofix <- rbind(tofix,pheno[index[i],c('year','location',dateNames)])
        pheno[index[i],dateNames[1]] <- gsub(yy[1],year0,pheno[index[i],dateNames[1]])
      }else drop <- c(drop,index[i]) # Do not know what to do
    }
  }
}

if(length(drop)>0){
  warning("The following observation(s) dismatched in sowing and harvesting years and",
          "\n couldn't be fixed thus were removed")
  print(pheno[drop,c('year','location',dateNames)])
  pheno <- pheno[-drop,]
}
if(length(tofix)>0){
  warning("The following observation(s) dismatched in sowing and harvesting years and were fixed")
  print(tofix)
}

# Check if any silking or anthesis is greater than harversting
dd1 <- as.Date(pheno[,dateNames[2]]) - as.Date(pheno[,"silking"])
dd2 <- as.Date(pheno[,dateNames[2]]) - as.Date(pheno[,"anthesis"])
if(any(dd1 < 0) | any(dd2 < 0)) stop("Error any swing or harvesting are after harvesting")

# Calculate plant density at sowing
pheno$plant_density <- pheno$seed_number / (pheno$plot_area / 10.764)

# Create location and year combination variable
info_loc <- data.frame(year_loc=paste0(info_loc$year,"-",info_loc$location),info_loc)
pheno <- data.frame(year_loc=paste0(pheno$year,"-",pheno$location),pheno)

# Select only locations with G2F information
pheno1 <- pheno[pheno$year_loc %in% intersect(pheno$year_loc, info_loc$year_loc), ]
index <- match(pheno1$year_loc, info_loc$year_loc)
pheno1 <- data.frame(pheno1,info_loc[index,c("lon","lat","gdd_silk","cluster")])

pheno1 <- pheno1[pheno1$gdd_silk != 0,]

# Remove checks
drop <- grep("CHECK",pheno1$source)
if(length(drop)>0){
  pheno1 <- pheno1[-drop,]
  cat("Removed",length(drop),"CHECKS!\n")
}

# Get location-year combination average
pheno2 <- do.call(rbind,lapply(split(pheno1,pheno1$year_loc),function(x){
  data.frame(
    x[1,c("year_loc","year","location","lat","lon","cluster")],
    sowing=mean(as.Date(x$sowing),na.rm=TRUE),
    silking=mean(as.Date(x$silking),na.rm=TRUE),
    anthesis=mean(as.Date(x$anthesis),na.rm=TRUE),
    harvesting=mean(as.Date(x$harvesting),na.rm=TRUE),
    t(apply(x[,c("yield","gdd_silk","plant_density")],2,mean,na.rm=T))
  )
}))
rownames(pheno2) <- NULL

# if NA in plant density fill with average
index <- which(is.na(pheno2$plant_density))
if(length(index) >0) pheno2$plant_density[index] <- mean(pheno2$plant_density[-index])

head(pheno1)
head(pheno2)

write.csv(pheno1,file="Data/clean_pheno_raw.csv",row.names=FALSE)
write.csv(pheno2,file="Data/clean_pheno_yearloc_means.csv",row.names=FALSE)
