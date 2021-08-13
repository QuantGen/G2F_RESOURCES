### The Goal
We want to generate environmental covariates that are informative of the non-genetic effects that affect crop yield in non-irrigated settings.  

### APSIM Crop model
(more of this [here](https://github.com/QuantGen/G2F_RESOURCES/blob/main/mdfiles/APSIM_v0.md))

#### V1 Simulation
The V1 simulations is an improvement in terms of the input that goes into the simulation. As V0 downloads soil data and weather data, but V1 does it for the past 10 years. This reduces the dependancy on the initial water parameter, as it calculates the water balance for each year.  
We formed 3 clusters of environments based on weather data for the past 10 years and calculated the GDD to flowering (a.k.a. silking) that a generic hybrid require on each cluster. This generic hybrid represents the environmental average. With this, the V1 code adjust the simulated flowering date to the average of the measured flowering date.  
In APSIM NG we use the GDD required to juvenile state and convert this to GDD to flowering by multiplying by 0.4. Although this number is arbitrary, we found that it results in a good correlation between simulated flowering date and the environmental average of flowering date.   
V1 also incorporates the average plant density used on each environment directly calculated from the phenotype data.

The results of V1 are saved in [simV1.rdata](Data/OutputFiles/simulations/simV1.rdata).   
The following is a table of the environmental covariates and its descriptions:  

| Environmental covariate | Description |
|-------------------------|-------------|
| SoilWater.CatchmentArea	| Catchment area for water flow calculations (m^2)	|
| SoilWater.CN2Bare	Runoff | Curve Number (CN) for bare soil with average moisture	|
| SoilWater.DiffusConst	| Constant in the soil water diffusivity calculation (mm2/day)	|
| SoilWater.DiffusSlope	| Effect of soil water storage above the lower limit on soil water diffusivity (/mm)	|
| SoilWater.Drainage	| Drainage (mm)	|
| SoilWater.Eo	| Gets potential evapotransipration of the whole soil-plant system (mm)	|
| SoilWater.Eos	| Gets potential evaporation from soil surface (mm)	|
| SoilWater.Es	| Gets the actual (realised) soil water evaporation (mm)	|
| SoilWater.ESW	| Gets extractable soil water relative to LL15 (mm)	|
| SoilWater.Evaporation	| Evaporation (mm)	|
| SoilWater.Flow	| Flow. Water moving up (mm)	|
| SoilWater.FlowNH4	| Amount of N leaching as NH4 from each soil layer (kg /ha)	|
| SoilWater.FlowNO3	| Amount of N leaching as NO3 from each soil layer (kg /ha)	|
| SoilWater.FlowUrea	| Amount of N leaching as urea from each soil layer (kg /ha)	|
| SoilWater.Flux	| Flux. Water moving down (mm)	|
| SoilWater.Infiltration	| Infiltration (mm)	|
| SoilWater.KLAT	| Lateral saturated hydraulic conductivity	|
| SoilWater.LateralFlow	| Lateral flow (mm)	|
| SoilWater.LateralOutflow	| Amount of water moving laterally out of the profile (mm)	|
| SoilWater.LeachNH4	| Amount of N leaching as NH4-N from the deepest soil layer (kg/ha)	|
| SoilWater.LeachNO3	| Amount of N leaching as NO3-N from the deepest soil layer (kg/ha)	|
| SoilWater.LeachUrea	| Amount of N leaching as urea-N from the deepest soil layer (kg/ha)	|
| SoilWater.PAW	| Plant available water SW-LL15 (mm/mm)	|
| SoilWater.PAWmm	| Plant available water SW-LL15 (mm)	|
| SoilWater.Pond	| Pond	|
| SoilWater.PotentialInfiltration	| This is set by microclimate and is rainfall less that intercepted by the canopy and residue component	|
| SoilWater.PotentialRunoff	| Gets potential runoff (mm)	|
| SoilWater.PrecipitationInterception	| This is set by microclimate as is rainfall less that intercepted by the canopy and residue component	|
| SoilWater.Runoff	| Runoff (mm)	|
| SoilWater.Runon	| Runon (mm)	|
| SoilWater.Salb	| Fraction of incoming radiation reflected from bare soil	|
| SoilWater.SoluteFlowEfficiency	| The efficiency (0-1) that solutes move up with water	|
| SoilWater.SoluteFluxEfficiency	| The efficiency (0-1) that solutes move down with water	|
| SoilWater.SummerCona	| Drying coefficient for stage 2 soil water evaporation in summer (aka ConA)	|
| SoilWater.SummerU	| Cummulative soil water evaporation to reach the end of stage 1 soil water evaporation in summer	|
| SoilWater.SW	| Amount of water in the soil (mm/mm)	|
| SoilWater.SWCON	| Fractional amount of water above DULL that can drain under gravity per day	|
| SoilWater.SWmm	| Gets soil water content (mm)	|
| SoilWater.T	| Time since start of second state evaporation (days)	|
| SoilWater.Thickness	| Soil layer thickness for each layer (mm)	|
| SoilWater.Water	| Amount of water in the soil (mm)	|
| SoilWater.WaterTable	| Water table	|
| SoilWater.WinterCona	| Drying coefficient for stage 2 soil water evaporation in winter	|
| SoilWater.WinterDate	| Start date for switch to winter paramenters for soil water evaporation (dd-mmm)	|
| SoilWater.WinterU	| Cummulative soil water evaporation to reach the end of stage 1 soil water evaporation in winter	|
| Field.Maize.CoverGreen	| Total plant green cover from all organs	|
| Field.Maize.CoverTotal	| Total plant cover from all organs	|
| Field.Maize.LAI	| Leaf area index	|
| Maize.AboveGround.Wt	| Above ground Biomass	|
| Maize.Grain.Wt	| Grain yield	|


