
rm(list=ls())
setwd("~/Dropbox/projects/G2F_RESOURCES")

# Load functions
source('Tools/Functions.R')
source('Tools/APSIM_run_apsim.R')
# run_apsim needs packages "nasapower" and "soilDB"
library(apsimx)
library(lubridate)

# Load all data
pheno <- read.csv('Data/OutputFiles/clean_pheno_yearloc_means.csv')

# Run simulation
simfile <- 'Maize.apsimx'
simdir <- 'Data/APSIM_sim'

# Run site-specific simulation
out <- lapply(1:nrow(pheno), function(i){
  tmp <- pheno[i,]
  res <- NULL
  try(
    res <- run_apsim(simfile, simdir, tmp$location,
                    coord=as.numeric(tmp[,c('lon', 'lat')]),
                    sow_date = tmp$sowing,
                    gdd_juvenile=round(tmp$gdd_silk*0.4), # x 0.4 to obtain GDD to juvenile!
                    plant_density = round(tmp$plant_density),
                    prefix=paste0(tmp$location,"_"))
  )
  message("Done: ", i, " / ", nrow(pheno))

  if(!is.null(res)){
    res <- as.data.frame(cbind(tmp[,c("year_loc","year","location"),drop=T],res))
  }
  res
})

SIM <- do.call(rbind,out)

write.csv(SIM, file = 'Data/OutputFiles/simulation_V1.csv', row.names=FALSE)
