### The Goal
We want to generate environmental covariates that are informative of the non-genetic effects that affect crop yield in non-irrigated settings.  

### APSIM Crop model
#### What is APSIM?
APSIM is a deterministic model that simulates agricultural systems. With APSIM we can make predictions of physiological and environmental covariates. To know more about APSIM go to: [What is APSIM?](https://www.apsim.info/apsim-model/).  
The software works in modules, for our project the most important ones are the weather module and the soil module.

#### APSIM in R
We can use R to call APSIM and make the simulations for us. 
We will be using APSIM Next generation, which is the new version and must be installed to run this code. Here you can install [APSIM Next Gen](https://www.apsim.info/download-apsim/), make sure to select the Next Generation version.  
Fernando Miguez has developed a package that connects APSIM with R. Check: [apsimx](https://cran.r-project.org/web/packages/apsimx/index.html) 
In addition to the apsimx package, we create a few functions to help us running the simulations and setting the necessary parameters. 

#### V0 Simulation
Simulations are done for each environment independently. In our case an environment is a combination of location and year (a row in the info_loc [file](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/info_loc.csv)). 
The V0 simulation is a first try with reduced input. It starts by downloading weather data from [NASApower](https://power.larc.nasa.gov/), setting the clock at 60 days previous to the sowing date, setting a generic maize hybrid for our simulation, and downloading soil data from [SSURGO](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/?cid=nrcs142p2_053627).
The result is a list of output covariates, where each element is a dataframe that contains the daily predictions of these covariates:
Maize.Grain.Wt = Maize grain yield in kilograms / 10.
SoilWater.Eo = Potential evapotransipration of the whole soil-plant system (mm). 
SoilWater.PAW = Plant available water in the soil profile (mm/mm).
