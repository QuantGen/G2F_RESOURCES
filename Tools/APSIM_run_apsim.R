# The clock is set to 1825 days (5 years) previous to the actual date by default.
run_apsim <- function(simfile, simdir, site_name, coord, sow_date, gdd_juvenile, plant_density, clock_before = 1825, replace_file = F) {
  
  # debug
  if (!is.Date(sow_date)) sow_date <- as.Date(sow_date)
  if (coord[2] < 0) stop('Check latitude if you are trying to run this with data from the Southern Hemisphere.')
  
  # Load functions
  require(apsimx)
  require(lubridate)
  
  # File management
  file_path <- paste0(getwd(),'/', simdir)
  file_exist <- file.exists(paste0(file_path, simfile))
  if (!file_exist | replace_file) {
    exdir <- auto_detect_apsimx_examples()
    file.copy(file.path(exdir, simfile), simdir, overwrite = T)
    #maize.rep.node <- get_apsimx_json('Maize')
    #insert_replacement_node(simfile, src.dir = simdir, rep.node = maize.rep.node, overwrite = TRUE)
  }
  
  # Download weather data
  met_filename <- paste0(site_name, '_', year(sow_date), '.met')
  met_path <- paste0(file_path, met_filename)
  site_period <- c(sow_date - clock_before, sow_date + 300)
  
  if (!file.exists(met_path)) {
    imet <- get_power_apsim_met(coord, dates = site_period)
    # Impute missing data
    for (w in which(is.na(imet[1,]))) imet[1, w] <- mean(imet[,w], na.rm = T)
    imet$radn <- ifelse(imet$radn == -99, NA, imet$radn)
    if (any(is.na(imet))) imet <- impute_apsim_met(imet)
    # Write met data
    write_apsim_met(imet, wrt.dir = simdir, filename = met_filename)
  }
  edit_apsimx(simfile, simdir, node = 'Weather', overwrite = T,
              value = gsub('/','\\\\', met_path), verbose = F)
  
  # Create new cultivar
  edit_apsimx_new(simfile, simdir, node = 'Cultivar', overwrite = T, verbose = F,
                  parm = 'Name', value = 'Custom')
  edit_apsimx_new(simfile, simdir, node = 'Cultivar', parm = 'Command', overwrite = T, verbose = F,
                  value = list(paste0('[Phenology].Juvenile.Target.FixedValue = ', gdd_juvenile)))  
  
  # Set simulation dates
  edit_apsimx(simfile, simdir, node = 'Clock', parm = c('Start', 'End'), overwrite = T, 
              value = paste0(site_period, 'T00:00:00'), verbose = F)
  
  # Set sowing rules
  edit_apsimx(simfile, simdir, node = 'Crop', parm = 'MinESW', overwrite = T,
              value = 100, verbose = F)
  edit_apsimx(simfile, simdir, node = 'Crop', parm = 'MinRain', overwrite = T,
              value = 25, verbose = F)
  edit_apsimx(simfile, simdir, node = 'Crop', parm = 'CultivarName', overwrite = T,
              value = 'Custom', verbose = F)
  edit_apsimx(simfile, simdir, node = 'Crop', parm = 'Population', overwrite = T,
              value = plant_density, verbose = F)
  edit_apsimx(simfile, simdir, node = 'Crop', parm = 'StartDate', overwrite = T,
              value = casefold(format(sow_date - 30, '%d-%b')), verbose = F)
  edit_apsimx_new(simfile, simdir, node = 'Crop', parm = 'EndDate', overwrite = T,
                  value = casefold(format(sow_date + 30, '%d-%b')), verbose = F)
  # Set report
  edit_apsimx_new(simfile, simdir, node = 'Report', overwrite = T, verbose = F,
                  parm = 'EventNames', value = list('[Clock].EndOfDay'))
  edit_apsimx_new(simfile, simdir, node = 'Report', overwrite = T, verbose = F,
                  parm = 'VariableNames', value = list('[Clock].Today', 
                                                       '[Maize].Phenology.CurrentStageName',
                                                       '[Maize].AboveGround.Wt',
                                                       '[Maize].Grain.Wt',
                                                       '[SoilWater].CatchmentArea',
                                                       '[SoilWater].CN2Bare',
                                                       '[SoilWater].DiffusConst',
                                                       '[SoilWater].DiffusSlope',
                                                       '[SoilWater].Drainage',
                                                       '[SoilWater].Eo',
                                                       '[SoilWater].Eos',
                                                       '[SoilWater].Es',
                                                       '[SoilWater].ESW',
                                                       '[SoilWater].Evaporation',
                                                       '[SoilWater].Flow',
                                                       '[SoilWater].FlowNH4',
                                                       '[SoilWater].FlowNO3',
                                                       '[SoilWater].FlowUrea',
                                                       '[SoilWater].Flux',
                                                       '[SoilWater].Infiltration',
                                                       '[SoilWater].KLAT',
                                                       '[SoilWater].LateralFlow',
                                                       '[SoilWater].LateralOutflow',
                                                       '[SoilWater].LeachNH4',
                                                       '[SoilWater].LeachNO3',
                                                       '[SoilWater].LeachUrea',
                                                       '[SoilWater].PAW',
                                                       '[SoilWater].PAWmm',
                                                       '[SoilWater].Pond',
                                                       '[SoilWater].PotentialInfiltration',
                                                       '[SoilWater].PotentialRunoff',
                                                       '[SoilWater].PrecipitationInterception',
                                                       '[SoilWater].Runoff',
                                                       '[SoilWater].Runon',
                                                       '[SoilWater].Salb',
                                                       '[SoilWater].SoluteFlowEfficiency',
                                                       '[SoilWater].SoluteFluxEfficiency',
                                                       '[SoilWater].SummerCona',
                                                       '[SoilWater].SummerU',
                                                       '[SoilWater].SW',
                                                       '[SoilWater].SWCON',
                                                       '[SoilWater].SWmm',
                                                       '[SoilWater].T',
                                                       '[SoilWater].Thickness',
                                                       '[SoilWater].Water',
                                                       '[SoilWater].WaterTable',
                                                       '[SoilWater].WinterCona',
                                                       '[SoilWater].WinterDate',
                                                       '[SoilWater].WinterU',
                                                       '[Field].Maize.CoverGreen',
                                                       '[Field].Maize.CoverTotal',
                                                       '[Field].Maize.LAI'))
  
  # Download soil data (US only)
  soil_table <- capture.output(inspect_apsimx(simfile, simdir, node = 'Soil'))[1:3]
  if (length(grep(coord[1], soil_table)) == 0 | length(grep(coord[2], soil_table)) == 0) {
    sp0 <- get_ssurgo_soil_profile(coord)
    edit_apsimx_replace_soil_profile(simfile, simdir, overwrite = T,
                                     soil.profile = sp0[[1]], verbose = F)
  }
  
  # Set soil water parameters (Northern Hemisphere)
  edit_apsimx(simfile, simdir, node = 'Soil', soil.child = 'SoilWater', overwrite = T,
              parm = 'SummerDate', value = '1-May', verbose = F)
  
  edit_apsimx(simfile, simdir, node = 'Soil', soil.child = 'SoilWater', overwrite = T,
              parm = 'WinterDate', value = '1-Aug', verbose = F)
  
  # Set initial water to 40%
  edit_apsimx(simfile, simdir, node = 'Soil', soil.child = 'InitialWater', overwrite = T,
              parm = 'FractionFull', value = 0.40, verbose = F)
  # Run simulation
  sim <- apsimx(simfile, simdir)
  
  # Format output
  sim$Clock.Today <- as.Date(sim$Clock.Today)
  sim <- sim[year(sim$Clock.Today) == year(sow_date),]
  sim <- sim[sim$Clock.Today > sim$Clock.Today[sim$Maize.Phenology.CurrentStageName == "Sowing"],]
  sim <- sim[sim$Clock.Today <= sim$Clock.Today[sim$Maize.Phenology.CurrentStageName == "HarvestRipe"],]
  return(sim)
}
