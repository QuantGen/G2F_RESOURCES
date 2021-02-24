The following script gets data from the SSURGO database. This database contains information about soil as collected by the National Cooperative Soil Survey - USDA.
We offer a function to download the data and, in the case of multiple horizons for a particular location, to combine horizons by a weighted mean of standard depths.

```r
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
envs <- read.csv('OutputFiles/info_loc.csv', stringsAsFactors = F) %>% filter(!grepl("^ON", Location)) #for now have to filter out Ontario 

soil_info <- apply(envs[,c('Location', 'lat', 'lon')], 1, function(x) soil_data(x['Location'], x['lat'], x['lon']))

soil_df <- do.call(rbind, soil_info)

# siltco_r and siltfine_r are actually zeroes not NA, so we can replace those
soil_df$siltco_r[is.na(soil_df$siltco_r)] <- 0
soil_df$siltfine_r[is.na(soil_df$siltfine_r)] <- 0

write.csv(soil_df, 'OutputFiles/SoilData.csv', row.names = F, quote = F)
```

The file SoilData.cs has the following columns:

|Column|Description|
|------|-----------|
|location| G2F field location name |
|lat| (decimal degrees) latitude of the location, used to get soil data |
|lon| (decimal degrees) longitude of the location, used to get soil data |
|saverest| Time when the record was saved |
|muname| Mapunit name |
|compname| Component name |
|hz_dept| (cm) distance from the top to the upper boundary of the soil horizon|
|hz_depb| (cm) distance from the top to the base of the soil horizon|
|sieveno4_r| (%) soil fraction passing a number 4 sieve (4.7 mm square opening) |
|sieveno10_r| (%) soil fraction passing a number 10 sieve (2 mm square opening) |
|sieveno40_r| (%) soil fraction passing a number 40 sieve (0.42 mm square opening) |
|sieveno200_r| (%) soil fraction passing a number 200 sieve (0.074 mm square opening) |
|sandtotal_r| (%) mineral particles 0.05mm to 2.0mm in equivalent diameter |
|sandvc_r| (%) mineral particles 1.0mm to 2.0mm in equivalent diameter |
|sandco_r| (%) mineral particles 0.5mm to 1.0mm in equivalent diameter |
|sandmed_r| (%) mineral particles 0.25mm to 0.5mm in equivalent diameter |
|sandfine_r| (%) mineral particles 0.10mm to 0.25mm in equivalent diameter |
|sandvf_r| (%) mineral particles 0.05mm to 0.10mm in equivalent diameter |
|silttotal_r| (%) mineral particles 0.002mm to 0.05mm in equivalent diameter |
|siltco_r| (%) mineral particles 0.02mm to 0.05mm in equivalent diameter |
|siltfine_r| (%) mineral particles 0.002mm to 0.02mm in equivalent diameter |
|claytotal_r| (%) mineral particles less than 0.002mm in equivalent diameter |
|om_r| (%) weight of decomposed plant and animal residue expressed as a weight percentage of the less than 2mm of soil material |
|partdensity| (m/v) mass per unit of volume (not including pore space) of the solid soil particle either mineral or organic |
|ksat_r| amount of water that would move vertically through a unit area of saturated soil in unit time under unit hydraulic gradient |
|awc_r| amount of water that an increment of soil depth, inclusive of fragments, can store that is available to plants |
|wtenthbar_r| volumetric content of soil water retained at a tension of 1/10 bar (10 kPa), expressed as a percentage of the whole soil |
|wthirdbar_r| volumetric content of soil water retained at a tension of 1/3 bar (33 kPa), expressed as a percentage of the whole soil |
|wfifteenbar_r| volumetric content of soil water retained at a tension of 15 bars (1500 kPa), expressed as a percentage of the whole soil |
|wsatiated_r| estimated volumetric soil water content at or near zero bar tension, expressed as a percentage of the whole soil|
|kwfact| erodibility factor which quantifies the susceptibility of soil particles to detachment and movement by water. This factor is adjusted for the effect of rock fragments |
|caco3_r| quantity of Carbonate (CO3) in the soil expressed as CaCO3 and as a weight percentage of the less than 2 mm size fraction |
|gypsum_r| percent by weight of hydrated calcium sulfate in the less than 20 mm fraction of soil|
|sar_r| measure of the amount of Sodium (Na) relative to Calcium (Ca) and Magnesium (Mg) in the water extract from saturated soil paste |
|ec_r| electrical conductivity of an extract from saturated soil paste |
|cec7_r| amount of readily exchangeable cations that can be electrically adsorbed to negative charges in the soil, soil constituent, or other material, at pH 7.0, as estimated by the ammonium acetate method |
|ecec_r| sum of NH4OAc extractable bases plus KCl extractable aluminum |
|ph01mcacl2_r| negative logarithm to base of 10 or the hydrogen ion activity in the soil, using the 0.01M CaCl2 method, in a 1:2 soil:solution ratio |
|ptotal_r| total phosphorous content of the soil, measured after total dissolution of a size fraction of the soil material.  It is reported as a gravimetric percent oxide of the size fraction used |
|sumbases_r| sum of NH4OAc extractable bases (pH 7.0), reported on less than 2mm base |
|freeiron_r| secondary iron oxides such as geothite, hematite, ferrihydrite, lepidocrocite and maghemite. It is iron extracted by dithionite-citrate|
|extracid_r| soil exchangeable hydrogen ions that may become active by cation exchange |
|extral_r| aluminum extracted in 1 normal potassium chloride |
|pbray1_r| amount of phosphorous in the less than 2mm fraction, that is extractable using the Bray1 method |


