setwd('c:/Users/fmagu/OneDrive/NewWorks/G2F_RESOURCES/')


# Load functions
source('Tools/Functions.R')


# Load G2F location info
g2f_loc <- read.csv('Data/OutputFiles/info_loc.csv')

# Remove locations with irrigation
g2f_loc <- g2f_loc[!g2f_loc$Irrigated,]

# Define APSIM function

run_apsim <- function(simfile = 'Maize.apsimx', simdir, site_name, site_coords, site_period) {
  # Load functions
  require(apsimx)
  require(lubridate)
  
  # Copy file
  exdir <- auto_detect_apsimx_examples()
  file.copy(file.path(exdir, simfile), simdir, overwrite = T)
  
  # Download weather data
  met_filename <- paste0(site_name, '_', year(site_period[1]), '.met')
  met_path <- paste0(getwd(),'/', simdir, met_filename)
  if (!file.exists(met_path)) {
    imet <- get_power_apsim_met(site_coords, dates = site_period + c(-60, 60))
    # Impute missing data
    imet$radn <- ifelse(imet$radn == -99, NA, imet$radn)
    if (any(is.na(imet))) imet <- impute_apsim_met(imet)
    # Write met data
    write_apsim_met(imet, wrt.dir = simdir, filename = met_filename)
  }
  edit_apsimx(simfile, simdir, node = 'Weather', overwrite = T,
              value = gsub('/','\\\\', met_path), verbose = F)
  # Set simulation dates
  edit_apsimx(simfile, simdir, node = 'Clock', parm = c('Start', 'End'), overwrite = T, 
              value = paste0(site_period, 'T00:00:00'), verbose = F)
  # Set sowing rules
  edit_apsimx(simfile, simdir, node = 'Crop', parm = 'MinESW', overwrite = T,
              value = 0, verbose = F)
  edit_apsimx(simfile, simdir, node = 'Crop', parm = 'MinRain', overwrite = T,
              value = 0, verbose = F)
  edit_apsimx(simfile, simdir, node = 'Crop', parm = 'CultivarName', overwrite = T,
              value = 'Pioneer_34K77', verbose = F)
  edit_apsimx(simfile, simdir, node = 'Crop', parm = 'StartDate', overwrite = T,
              value = casefold(format(site_period[1]-2, '%d-%b')), verbose = F)
  edit_apsimx_new(simfile, simdir, node = 'Crop', parm = 'EndDate', overwrite = T,
                  value = casefold(format(site_period[2]+2, '%d-%b')), verbose = F)
  # Set report
  edit_apsimx_new(simfile, simdir, node = 'Report', overwrite = T, verbose = F,
                  parm = 'EventNames', value = list('[Clock].EndOfDay'))
  edit_apsimx_new(simfile, simdir, node = 'Report', overwrite = T, verbose = F,
                  parm = 'VariableNames', value = list('[Clock].Today', 
                                                       '[Maize].Phenology.CurrentStageName',
                                                       '[Maize].AboveGround.Wt',
                                                       '[Maize].Grain.Wt',
                                                       '[Soil].SoilWater.Es',
                                                       '[SoilWater].Eo',
                                                       '[Soil].SoilWater.ESW',
                                                       '[Soil].SoilWater.ESW',
                                                       '[SoilWater].SW',
                                                       '[SoilWater].Evaporation',
                                                       '[Field].Maize.CoverGreen',
                                                       '[Field].Maize.LAI',
                                                       '[SoilWater].SWmm'))
  # Download soil data (US only)
  sp0 <- get_ssurgo_soil_profile(site_coords)
  edit_apsimx_replace_soil_profile(simfile, simdir, overwrite = T,
                                   soil.profile = sp0[[1]], verbose = F)
  # Set soil water parameters
  edit_apsimx(simfile, simdir, node = 'Soil', soil.child = 'SoilWater', overwrite = T,
              parm = 'SummerDate', value = '1-May', verbose = F)
  
  edit_apsimx(simfile, simdir, node = 'Soil', soil.child = 'SoilWater', overwrite = T,
              parm = 'WinterDate', value = '1-Aug', verbose = F)
  
  # Set initial water to 90%
  edit_apsimx(simfile, simdir, node = 'Soil', soil.child = 'InitialWater', overwrite = T,
              parm = 'FractionFull', value = 0.9, verbose = F)
  # Run sim
  tmp <- cbind(year = g2f_loc$year[i], location = site_name, apsimx(simfile, simdir))
  file.remove(paste0(simdir, simfile))
  return(tmp)
}

simV0_loc <- g2f_loc
simV0_loc <- simV0_loc[-8,]
simV0_loc <- simV0_loc[-28,]
simV0_loc <- simV0_loc[-84,]

# load phenotype data
pheno <- read.csv('Data/OutputFiles/phenotypes.csv')
# Fix pheno data
pheno <- pheno[!is.na(pheno$date_plant) & !is.na(pheno$date_silking) & !is.na(pheno$date_harvest) & !is.na(pheno$yield),]
tmp <- year(pheno$date_harvest) == 2016 & pheno$year == 2019
pheno$date_harvest[tmp] <- gsub(2016, pheno$date_plant[tmp][1], pheno$date_harvest[tmp])
pheno$year <- year(pheno$date_harvest)

# Add yield average
simV0_loc$yield <- NA
for (i in 1:nrow(simV0_loc)) {
  simV0_loc$yield[i] <- mean(pheno$yield[pheno$year == simV0_loc$year[i] & pheno$location == simV0_loc$location[i]], na.rm = T)
}
simV0_loc <- simV0_loc[!is.na(simV0_loc$yield),]
rownames(simV0_loc) <- NULL

# Run simulation
simV0 <- list()
for (i in 1:nrow(simV0_loc)) {
  print(i)
  simfile <- 'Maize.apsimx'
  simdir <- 'Data/APSIM_sim/'
  site_name <- simV0_loc$location[i]
  site_coords <- c(simV0_loc$lon[i], simV0_loc$lat[i])
  site_period <- as.Date(c(simV0_loc$sowing[i], simV0_loc$harvesting[i]), format = '%m/%d/%Y')
  simV0[[i]] <- run_apsim(simfile, simdir, site_name, site_coords, site_period)
}

# Save simulation
save(simV0, simV0_loc, file = 'Data/OutputFiles/simulations/simV0.rdata')
