**Author: Jim Holland**


```r
 install.packages("httr", dep=TRUE)
 install.packages("aqp", dep=TRUE)
 install.packages("soilDB", dep=TRUE)
 install.packages("rgdal", dep = TRUE)
 install.packages("raster", dep = TRUE)
 install.packages("rgeos", dep = TRUE)
```


Simple Queries

Now that you have the required packages, load them into the current R session.
```r
library(aqp)
library(soilDB)
library(sp)
library(rgdal)
library(plyr)
library(raster)
library(rgeos)
library(tidyverse)
```

What are the units for depth measurements? I think they are cm...but can't access table mdstattabcols to find out

Here is the CSR2 calculation:
https://crops.extension.iastate.edu/cropnews/2015/04/corn-suitability-rating-2-equation-updated
CSR2 formula: CSR2 = S-M-W-F-D ± EJ

Where:

S - is the taxonomic subgroup class of the series of the soil map unit (MU),
M - is the family particle size class,
W - relates to available water holding capacity (AWC) of the series,
F - is the field condition of a particular MU, for example, slope, flooding, ponding, erosion class and topsoil thickness,
D  - is the soil depth and tolerable rate of soil erosion,
EJ - is an expert judgment correction factor. EJ is normally used with parent materials that have very high bulk density and/or are unusually clayey or sandy. Soil series with an EJ correction are described in the posted description.

It's not clear how S is converted to a number. S is a soil taxonomy, and somehow that is related to corn productivity, but it's not explained.

EJ is also not something we can get from the DB.

ponding frequency was not easy to get, so I skipped that.

We can get lots of information on particle size, water holding capacity (awc), slope, T-factor for erosion (tfact), depth of topsoil to restriction zone (resdepb_r). And also the original Iowa Corn suitability rating (iacornsr)

(see here for descriptions of the tables and columns of the DB):
https://data.nal.usda.gov/system/files/SSURGO_Metadata_-_Table_Column_Descriptions.pdf

implement in a function.
```r
rm(list = ls()) #remove all the objects made previously, so there is no chance of conflict
soil_data = function(env_name, lat, long){
  print(env_name)
  gps = paste('point(', long, lat, ')')
  q <- paste0("SELECT mukey, muname
  FROM mapunit
  WHERE mukey IN (
  SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('  ",
  gps, "'))")

  mk <- SDA_query(q)
  
  query = paste("
SELECT 
saversion, saverest, -- attributes from table sacatalog l.areasymbol, l.areaname, l.lkey, -- attributes from table legend 
mu.musym, mu.muname, museq, mu.mukey, iacornsr, -- attributes from table mapunit
comppct_r, compname, localphase, slope_r, c.cokey, tfact, -- attributes from table component 
hzdept_r, hzdepb_r, ch.chkey, sieveno4_r, sieveno10_r, sieveno40_r, sieveno200_r, sandvc_r, sandco_r, sandmed_r, sandfine_r, sandvf_r, siltco_r, siltfine_r, claytotal_r, partdensity, ksat_r, awc_r, wtenthbar_r, wthirdbar_r, wfifteenbar_r, kwfact, caco3_r, gypsum_r, sar_r, cec7_r, ecec_r, ph01mcacl2_r, sumbases_r, freeiron_r, extracid_r, extral_r, pbray1_r, -- attributes from table chorizon
resdepb_r, cor.cokey -- attributes from corestrictions
FROM sacatalog sac   
  INNER JOIN legend l ON l.areasymbol = sac.areasymbol   
    INNER JOIN mapunit mu ON mu.lkey = l.lkey       
    AND mu.mukey = " ,
    mk$mukey, 
    "LEFT OUTER JOIN component c ON c.mukey = mu.mukey                 LEFT OUTER JOIN chorizon ch ON ch.cokey = c.cokey  
          LEFT OUTER JOIN corestrictions cor ON cor.cokey = c.cokey ")

db = SDA_query(query) 

#find the max depth of soil horizons, compute percent of depth
#for each horizon up to 200 cm
db = db[(names(db)[!duplicated(names(db))])] %>%
  group_by(cokey) %>%
mutate(resdepb_r = ifelse(is.na(resdepb_r), max(hzdepb_r, na.rm = T), resdepb_r),
    resdepb_r = ifelse(resdepb_r == -Inf, NA, resdepb_r),   
    hzdepb_r = unlist(pmap(list(hzdepb_r, resdepb_r, 200), min)),
         propor_depth = (hzdepb_r - hzdept_r)/min(resdepb_r, 200))
     
#summarize over horizons within a component
db_sum = db %>%
  summarise_at(vars(comppct_r, slope_r, tfact, sieveno4_r, sieveno10_r, sieveno40_r, sieveno200_r, sandvc_r, sandco_r, sandmed_r, sandfine_r, sandvf_r, siltco_r, siltfine_r, claytotal_r, partdensity, ksat_r, awc_r, wtenthbar_r, wthirdbar_r, wfifteenbar_r, kwfact, caco3_r, gypsum_r, sar_r, cec7_r, ecec_r, ph01mcacl2_r, sumbases_r, freeiron_r, extracid_r, extral_r, pbray1_r, resdepb_r), ~ weighted.mean(., w = propor_depth, na.rm = T)) %>%
  ungroup() %>%
  
  #summarize across components within the environment
  summarise_at(vars(slope_r, tfact, sieveno4_r, sieveno10_r, sieveno40_r, sieveno200_r, sandvc_r, sandco_r, sandmed_r, sandfine_r, sandvf_r, siltco_r, siltfine_r, claytotal_r, partdensity, ksat_r, awc_r, wtenthbar_r, wthirdbar_r, wfifteenbar_r, kwfact, caco3_r, gypsum_r, sar_r, cec7_r, ecec_r, ph01mcacl2_r, sumbases_r, freeiron_r, extracid_r, extral_r, pbray1_r, resdepb_r), ~ weighted.mean(., w = comppct_r, na.rm = T))

#get the character meta-data back with the summary quantitative data
db_sum$Env = env_name
db_sum$lat = lat
db_sum$long = long
db_sum$soil_types = paste(sort(unique(db$compname)), collapse = ", ")

return(db_sum)
}
```

Now read in the list of G2F environments and extract the data for each. This dropped two environments with identical GPS coordinates. They can be added back in later.
```r
envs = read.csv("Q:/My Drive/Anna/Weather Data G2F/G2F_Locations.csv", stringsAsFactors = F) %>%
  filter(!grepl("^ON", Env)) #for now have to filter out Ontario 
```

```r
soil_info = apply(envs[,c("Env", "Latitude", "Longitude")], 1, function(x) soil_data(x['Env'], x['Latitude'], x['Longitude']))
```
Unpack the lists into a common data frame
```r
soil_df = do.call(bind_rows, soil_info)
```

Now summarize each variable for percent that is missing and also for the variation. We can drop variables that are missing a lot of data or have little variance
```r
variable.summary = 
  soil_df %>% summarise_at(vars(slope_r, tfact, sieveno4_r, sieveno10_r, sieveno40_r, sieveno200_r, sandvc_r, sandco_r, sandmed_r, sandfine_r, sandvf_r, siltco_r, siltfine_r, claytotal_r, partdensity, ksat_r, awc_r, wtenthbar_r, wthirdbar_r, wfifteenbar_r, kwfact, caco3_r, gypsum_r, sar_r, cec7_r, ecec_r, ph01mcacl2_r, sumbases_r, freeiron_r, extracid_r, extral_r, pbray1_r, resdepb_r), funs(missing = mean(is.na(.)), CV = (sd(.,na.rm = T))/mean(.,na.rm = T)) )
                            
                            
```

siltco_r and siltfine_r are actually zeroes not NA, so we can replace those

variables with a few missing data points that can be imputed are:
cec7_r
sumbases_r
extracid_r

variables with high missing rates and should be dropped are:
wtenthbar_r
ecec_r
ph01mcacl2_r
freeiron_r
extral_r
pbray1_r

traits with very low CVs (< 0.1) that can be dropped
partdensity_r
sieveno4_r
sieveno10_r

Drop these variables, input the proper zeroes
```r
soil_df = soil_df %>%
  select(-c(wtenthbar_r, ecec_r, ph01mcacl2_r, freeiron_r, extral_r, pbray1_r, partdensity, sieveno4_r, sieveno10_r)) %>%
  replace_na(replace = list(siltco_r = 0, siltfine_r = 0))
```

Impute the few missing values with MICE
```r
library(mice)
traits.to.impute = c('cec7_r', 'sumbases_r', 'extracid_r')
mice.imp = mice(soil_df, m = 10, print = F)
```
Compute the imputed values as averages over the 10 imputations
```r

imp.list = list()
for (rep in 1:10){
  dfi = complete(mice.imp, rep)
  dfi = dfi[traits.to.impute]
  imp.list[[rep]] = dfi
}

imp.sum = Reduce('+', imp.list)
imp.mean = imp.sum/10

```


Make sure the data that already existing have not been changed from the imputation
```r
plot(imp.mean$cec7_r, soil_df$cec7_r)
```
```r
plot(imp.mean$sumbases_r, soil_df$sumbases_r)
```
```r
plot(imp.mean$extracid_r, soil_df$extracid_r)
```
OK, they look fine. Now replace the columns of original data with imputed data
```r
soil_df2 = soil_df 
soil_df2$cec7_r = imp.mean$cec7_r
soil_df2$sumbases_r = imp.mean$sumbases_r
soil_df2$extracid_r = imp.mean$extracid_r
```

Check for missing data in soil_df2
```r
any(is.na(soil_df2[,]))
```
```r
soil_df2 = soil_df2 %>% select(Env, lat, long, everything())
```

```r
write.csv(soil_df2, "Q:/My Drive/Anna/Weather Data G2F/Soil Data/G2F2014_16_SoilData.csv", row.names = F, quote = F)
```

get the data for Canada. They do not have a comparable data base. Best I could do was check the soil map for Ontario at the research station location. It indicated that the two dominant soils are Woolwhich Loam and Conestoga Loam. Both occur in USA as well, so I found their mukey in the USA database and got the soil info that way.
Conestoga Loam 0 - 3% slope: 2403658

Now I am thinking this is a bad idea, it seems like Canada has their own soil naming system with duplicate names for different soils than are in the USA. So let's NOT do this!

Also, I tried downloading the 'shape' data for the Canadian soil maps, but I don't think this is going to help us. It's just spatially mapping the soil names, I believe...

library(rgdal)

shape <- readOGR(dsn = "C:/Users/jholland/Downloads/waterloo_50k", layer = "soil")


