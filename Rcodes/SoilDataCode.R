
# Install packages needed
install.packages("httr", dep=TRUE)
install.packages("aqp", dep=TRUE)
install.packages("soilDB", dep=TRUE)
install.packages("rgdal", dep = TRUE)
install.packages("raster", dep = TRUE)
install.packages("rgeos", dep = TRUE)

# load packages
library(aqp)
library(soilDB)
library(sp)
library(rgdal)
library(plyr)
library(raster)
library(rgeos)
library(tidyverse)


# Table of column descriptions
# https://data.nal.usda.gov/system/files/SSURGO_Metadata_-_Table_Column_Descriptions.pdf

# Function to get soil data
soil_data <- function(Location, lat, lon, combineMultipleHorizons = T){
  print(Location)
  gps <- paste('point(', lon, lat, ')')
  q <- paste0("SELECT mukey, muname
  FROM mapunit
  WHERE mukey IN (
  SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('  ",
              gps, "'))")
  mk <- SDA_query(q)
  query <- paste("
SELECT 
saversion, saverest, -- attributes from table sacatalog l.areasymbol, l.areaname, l.lkey, -- attributes from table legend 
mu.musym, mu.muname, museq, mu.mukey, iacornsr, -- attributes from table mapunit
comppct_r, compname, localphase, slope_r, c.cokey, tfact, -- attributes from table component 
hzdept_r, hzdepb_r, ch.chkey, sieveno4_r, sieveno10_r, sieveno40_r, sieveno200_r, sandtotal_r, sandvc_r, sandco_r, sandmed_r, sandfine_r, sandvf_r, silttotal_r, siltco_r, siltfine_r, claytotal_r, om_r, partdensity, ksat_r, awc_r, wtenthbar_r, wthirdbar_r, wfifteenbar_r, wsatiated_r, kwfact, caco3_r, gypsum_r, sar_r, ec_r, cec7_r, ecec_r, ph01mcacl2_r, ptotal_r, sumbases_r, freeiron_r, extracid_r, extral_r, pbray1_r, -- attributes from table chorizon
resdepb_r, cor.cokey -- attributes from corestrictions
FROM sacatalog sac   
  INNER JOIN legend l ON l.areasymbol = sac.areasymbol   
    INNER JOIN mapunit mu ON mu.lkey = l.lkey       
    AND mu.mukey = " ,
                 mk$mukey, 
                 "LEFT OUTER JOIN component c ON c.mukey = mu.mukey                 LEFT OUTER JOIN chorizon ch ON ch.cokey = c.cokey  
          LEFT OUTER JOIN corestrictions cor ON cor.cokey = c.cokey ")
  
  db <- cbind(Location, lat, lon, SDA_query(query))
  
  # standarize horizons
  if (combineMultipleHorizons) {
    db2 <- as.data.frame(db)
    soilvars <- c('sieveno4_r', 'sieveno10_r', 'sieveno40_r', 'sieveno200_r', 'sandtotal_r', 'sandvc_r', 'sandco_r', 'sandmed_r',
                  'sandfine_r', 'sandvf_r', 'silttotal_r', 'siltco_r', 'siltfine_r', 'claytotal_r', 'om_r', 'partdensity', 'ksat_r', 'awc_r', 'ksat_r awc_r',
                  'wtenthbar_r', 'wthirdbar_r', 'wfifteenbar_r', 'wsatiated_r', 'kwfact', 'caco3_r', 'gypsum_r', 'sar_r', 'ec_r', 'cec7_r',
                  'ecec_r', 'ph01mcacl2_r', 'ptotal_r', 'sumbases_r', 'freeiron_r', 'extracid_r', 'extral_r', 'pbray1_r')
    
    hz_dept <- c(0, 10, 25, 50, 80, 100)
    hz_depb <- c(10, 25, 50, 80, 100, 200)
    nhz <- length(unique(db2$compname))
    db3 <- data.frame(location = db2$Location[1], lat = db2$lat[1], lon = db2$lon[1], 
                      saverest = db2$saverest[1], muname = db2$muname[1], 
                      compname = paste(unique(db2$compname), collapse = ' - '),
                      hz_dept, hz_depb)
    
    for (j in which(colnames(db2) %in% soilvars)){
      hz_value <- vector()
      for (i in seq_along(hz_depb)) {
        isoilvar <- colnames(db2)[j]
        iscale <- (hz_depb[i] - hz_dept[i]) * nhz
        tmp <- db2[hz_dept[i] <= db2$hzdepb_r & hz_depb[i] > db2$hzdept_r, c('hzdept_r', 'hzdepb_r', isoilvar)]
        if (all(is.na(tmp[,3]))) {
          hz_value[i] <- NA  
        } else {
          tmp <- tmp[!is.na(tmp[,3]),]
          ihzsize <- tmp$hzdepb_r - tmp$hzdept_r
          ihzfr1 <- tmp$hzdepb_r  - hz_depb[i]
          ihzfr1[ihzfr1 < 0] <- 0
          ihzfr2 <- hz_dept[i] - tmp$hzdept_r
          ihzfr2[ihzfr2 < 0] <- 0
          iprop <- (ihzsize - ihzfr1 - ihzfr2) / iscale
          hz_value[i] <- sum(tmp[,3] * iprop)
        }
      }
      db3[,isoilvar] <- hz_value
    }
    return(db3)
  } else {
    return(db)
  }
}

# Read in the list of G2F environments and extract the data for each.
envs <- read.csv('Data/OutputFiles/info_loc.csv', stringsAsFactors = F) %>% filter(!grepl("^ON", Location)) #for now have to filter out Ontario 

soil_info <- apply(envs[,c('Location', 'lat', 'lon')], 1, function(x) soil_data(x['Location'], x['lat'], x['lon']))

soil_df <- do.call(rbind, soil_info)

# siltco_r and siltfine_r are actually zeroes not NA, so we can replace those
soil_df$siltco_r[is.na(soil_df$siltco_r)] <- 0
soil_df$siltfine_r[is.na(soil_df$siltfine_r)] <- 0
# replace comma for period
soil_df$muname <- gsub(',', '.', soil_df$muname)

write.csv(soil_df, 'Data/OutputFiles/SoilData.csv', row.names = F, quote = F)
