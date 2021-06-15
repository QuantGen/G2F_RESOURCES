# Load functions
source('Tools/Functions.R')


# Load G2F location info
g2f_loc <- read.csv('Data/OutputFiles/info_loc.csv')

# Remove locations with irrigation
g2f_loc <- g2f_loc[g2f_loc$Location != 'ONH2',]
g2f_loc <- g2f_loc[!(g2f_loc$Location %in% c('ARH1','GAH1','GAH2','KSH1','TXH1','TXH2') & g2f_loc$year == 2018),]
g2f_loc <- g2f_loc[!(g2f_loc$Location %in% c('NCH1','GAH1','TXH1','TXH4') & g2f_loc$year == 2019),]
rownames(g2f_loc) <- NULL


# Define APSIM function
run_apsim <- function(simfile = 'Maize.apsimx', simdir = 'APSIM_sim/', 
                      site_name, site_coords, site_period) {
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
                                                       '[SoilWater].Eo',
                                                       '[SoilWater].PAW'))
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


# Run simulation
sim <- list()
for (i in 1:nrow(g2f_loc)) {
  print(i)
  simfile <- 'Maize.apsimx'
  simdir <- 'APSIM_sim/'
  site_name <- g2f_loc$Location[i]
  site_coords <- c(g2f_loc$lon[i], g2f_loc$lat[i])
  site_period <- as.Date(c(g2f_loc$sowing[i], g2f_loc$harvesting[i]))
  sim[[i]] <- run_apsim(simfile, simdir, site_name, site_coords, site_period)
}

# Save simulation
save(sim, file = 'APSIM_sim/v0Sim.rdata')
