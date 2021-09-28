setwd('c:/Users/fmagu/OneDrive/NewWorks/G2F_RESOURCES/')

# Load functions
source('Tools/Functions.R')
source('Tools/APSIM_run_apsim.R')
library(apsimx)
library(lubridate)

# Load G2F location info
g2f_loc <- read.csv('Data/OutputFiles/info_loc.csv')

# Remove locations with irrigation
g2f_loc <- g2f_loc[!g2f_loc$Irrigated,]

# change date format
g2f_loc$sowing <- as.Date(g2f_loc$sowing, format = '%m/%d/%Y')
g2f_loc$harvesting <- as.Date(g2f_loc$harvesting, format = '%m/%d/%Y')

# Cluster locations (informed decision based on 10 years of weather data)
cluster_a <- c('SDH1', 'MNH1', 'WIH1', 'WIH2', 'MIH1', 'NYH1', 'NYH2', 'NYH3', 'NEH3', 'NEH4')
cluster_b <- c('COH1', 'NEH1', 'NEH2', 'NEH3', 'NEH4', 'IAH1', 'IAH2', 'IAH3', 'IAH4', 'ILH1', 'INH1', 'INH2', 'OHH1')
cluster_c <- c('KSH1', 'MOH1', 'MOH2', 'DEH1', 'SCH1', 'NCH1', 'GAH1', 'GAH2')

g2f_loc$cluster <- NA
g2f_loc$cluster[g2f_loc$location %in% cluster_a] <- 'a'
g2f_loc$cluster[g2f_loc$location %in% cluster_b] <- 'b'
g2f_loc$cluster[g2f_loc$location %in% cluster_c] <- 'c'

# GDD to flowering by cluster
g2f_loc$gdd_silk <- NA
g2f_loc$gdd_silk[g2f_loc$location %in% cluster_a] <- 722
g2f_loc$gdd_silk[g2f_loc$location %in% cluster_b] <- 778
g2f_loc$gdd_silk[g2f_loc$location %in% cluster_c] <- 789

# load phenotype data
pheno <- read.csv('Data/OutputFiles/phenotypes.csv')
# Fix pheno data
pheno <- pheno[!is.na(pheno$date_plant) & !is.na(pheno$date_silking) & !is.na(pheno$date_harvest) & !is.na(pheno$yield),]
tmp <- year(pheno$date_harvest) == 2016 & pheno$year == 2019
pheno$date_harvest[tmp] <- gsub(2016, pheno$date_plant[tmp][1], pheno$date_harvest[tmp])
pheno$year <- year(pheno$date_harvest)

# Create template for simulation
pheno1 <- list()
for (i in 1:nrow(g2f_loc)) {
  print(i)
  site_name <- g2f_loc$location[i]
  site_period <- c(g2f_loc$sowing[i], g2f_loc$harvesting[i])
  site_year <- year(site_period)[1]
  tmp <- pheno$location == site_name & pheno$year == site_year
  if(any(tmp)) {
    site_pheno <- pheno[tmp,]
    site_pheno$lon <- g2f_loc$lon[i]
    site_pheno$lat <- g2f_loc$lat[i]
    site_pheno$gdd_silk <- g2f_loc$gdd_silk[i]
    site_pheno$cluster <- g2f_loc$cluster[i]
    pheno1[[i]] <- site_pheno
  }
}
pheno2 <- do.call(rbind, pheno1)

# remove irrigated locations from pheno data
pheno2 <- pheno2[pheno2$gdd_silk != 0,]

# Calculate plant density at sowing
pheno2$plant_density <- pheno2$seed_number / (pheno2$plot_area / 10.764)

# Create pheno3: location and year combination average
pheno2$loc_yea <- paste(pheno2$location, pheno2$year)
pheno3 <- list()
for (i in 1:length(unique(pheno2$loc_yea))) {
  tmp <- pheno2[pheno2$loc_yea == unique(pheno2$loc_yea)[i],]
  pheno3[[i]] <- data.frame(year = tmp$year[1], location = tmp$location[1], lat = tmp$lat[1], lon = tmp$lon[1], cluster = tmp$cluster[1],
                            date_sowing = mean(as.Date(tmp$date_plant), na.rm = T), 
                            date_silking = mean(as.Date(tmp$date_silking), na.rm = T), 
                            date_harvesting = mean(as.Date(tmp$date_harvest), na.rm = T), 
                            yield = mean(tmp$yield, na.rm = T), 
                            gdd_silk = tmp$gdd_silk[1],
                            plant_density = mean(tmp$plant_density, na.rm = T))
}
pheno3 <- do.call(rbind, pheno3)
rownames(pheno3) <- NULL

# if NA in plant density fill with average
pheno3$plant_density[is.na(pheno3$plant_density)] <- mean(pheno3$plant_density, na.rm = T)

# Run simulation
simfile <- 'Maize.apsimx'
simdir <- 'Data/APSIM_sim/'

# Remove locations with corrupted weather or soil data?
pheno3 <- pheno3[-8,]
pheno3 <- pheno3[-19,]
pheno3 <- pheno3[-61,]

# Run site-specific simulation
simV1 <- list()
for (i in 1:nrow(pheno3)) {
  site_name <- pheno3$location[i]
  site_gdd <- round(pheno3$gdd_silk[i] *.4) # gdd to juvenile!
  site_year <- pheno3$year[i]
  site_coords <- as.numeric(g2f_loc[g2f_loc$year == site_year & g2f_loc$location == site_name, c('lon', 'lat')])
  site_sowing <- pheno3$date_sowing[i]
  plant_density <- round(pheno3$plant_density[i])
  sim <- run_apsim(simfile, simdir, site_name, coord = site_coords, sow_date = site_sowing, gdd_juvenile = site_gdd, plant_density = plant_density)
  simV1[[i]] <- sim
  print(paste0(i, ' / ', nrow(pheno3)))
}
simV1_loc <- pheno3
save(simV1, simV1_loc, file = 'Data/OutputFiles/simulations/simV1.rdata')
