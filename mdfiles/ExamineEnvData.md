### Generate a pdf with plots of weather data by trial

The following code produces a series of plots to examine the different sources of weather data. 
Each plot by trial (year-location combination) consists of 3 panels: 
1) Upper boxplot with yield for all trials and average yield for the trial of interest.
2) left scatter plot of temperature by weather station (red: G2F, green: NASA).
3) right bar plot of monthly accumulated rainfall by weather station. The title contains the number of days from sowing to harvesting and the final source of weather data selected based on visual assessment.

All plots are collected into a pdf file: [plots_collection.pdf](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/plots_collection.pdf)

After visual examination, we selected the best source of data for each trial and save it in [ConsensusData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/ConsensusData.csv):

|Column|Description|Origin|
|------|-----------|------|
|date| (yyyy-mm-dd) Sowing date | |
|location| G2F field location name | |
|rainfall| (mm) Daily rainfall | G2F or NASA |
|temp| (°C) Daily mean temperature | G2F or NASA |
|temp_min| (°C) Daily minimum temperature | G2F or NASA |
|temp_max| (°C) Daily maximum temperature | G2F or NASA |
|allsky_sfc_lw_dwn| (kW-hr/m^2/day) Downward Thermal Infrared (Longwave) Radiative Flux| NASA |
|allsky_sfc_sw_dwn| (kW-hr/m^2/day) All Sky Insolation Incident on a Horizontal Surface| NASA |
|allsky_toa_sw_dwn| (kW-hr/m^2/day) Top-of-atmosphere Insolation| NASA |
|ps|(kPa) Surface Pressure| NASA |
|rh2m|(%) Relative Humidity at 2 Meters| NASA |
|t2mdew|(°C) Dew/Frost Point at 2 Meters| NASA |
|t2mwet|(°C) Wet Bulb Temperature at 2 Meters| NASA |
|ts|(°C) Earth Skin Temperature| NASA |
|ws10m| (m/s) Wind Speed at 10 Meters| NASA |
|eto (mm/day)| General theoretical evapotranspiration calculated using Blaney-Criddle method.| NASA |

[Go back to main](https://github.com/QuantGen/G2F_RESOURCES)
