#### Pending from Meeting Jun 8, 2021

v0: Simulation with Clock, Soil, Weather

v0: Check the results and compare with real data

Check create new genotype in APSIM (Hybrid)

Include weather data from G2F

---------------------------------------------------------------------------------


#### Pending from Meeting Jun 1, 2021

Generating a dataset with predictions of **GDD needed to flowering** by hybrid

Extending the weather data with at least **two months before sowing**.

Exploring the R package to implement **APSIM** (apsimx)

Exploring **DSSAT**

Quality checking and formatting of the **management data**, primarily fertilization time, source, and quantity.

Creating a **dataset with a friendly format** ready to run the crop model. To work on this, we will need your help on defining which is the best format.

---------------------------------------------------------------------------------

#### Pending from Meeting May 21, 2021
 
**Soil**: (**Fernando?**)
Merge USDA and G2F. We will prioritize G2F data. We will use USDA primarily by adding to the G2F data variables that are in USDA but not in G2F. We should first identify those variables and discuss how usefult those could be. If we identify some, then we will need to discuss how to create a summary that is appropiate for the horizon being reported in G2F. 

**Meta data**: request 2019 to Dayane. (**Fernando?**)
 
**Fertilization**: complete file witn N/P/K total. (**Fernando ?**)

**Day length**: It may be in the meta data. Perhpas easier, we can generate it using the `daylength(lat, doy)` function (`lat`=`latitude`, `doy`=`day of the year`) of the [geosphere R-package](https://cran.r-project.org/web/packages/geosphere/index.html). (**Marco?**) This data could be appended to the concensus file daily file. 

**Solar radiation**: Anna suggested looking at the [National Solar Radiation Database](https://nsrdb.nrel.gov/). (**Marco?**)

**Regarding NASA/G2F**: We need to discuss this. The biases are serious. It may be that the best approach would be to make the concensus file a file with two cols for temperature and two columns for precipitation (one from NASA and one from G2F, placing NAs in the entires of G2F that are clearly problematic. 





   
