### Format and quality assessment of G2F weather data

The code reads the original G2F weather files and completes the missing or corrupted datapoints with NASA data. To decide which source of data to use, we made plots for each trial (year-location combination) consisting of 3 panels:
1) Upper panel: boxplot with yield for all trials and average yield for the trial of interest.
2) left panel: scatter plot of temperature by weather station (red: G2F, green: NASA).
3) right panel: bar plot of monthly accumulated rainfall by weather station.

All plots are collected into a pdf file: [plots_collection.pdf](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/plots_collection.pdf)

The resulting file is [g2f_WeatherData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/g2f_WeatherData.csv):

|Column|Description|Origin|
|------|-----------|------|
|date| (yyyy-mm-dd) Sowing date | |
|location| G2F field location name | |
|rainfall| (mm) Daily rainfall | G2F or NASA |
|temp| (°C) Daily mean temperature | G2F or NASA |
|temp_min| (°C) Daily minimum temperature | G2F or NASA |
|temp_max| (°C) Daily maximum temperature | G2F or NASA |
|allsky_sfc_sw_dwn| (kW-hr/m^2/day) All Sky Insolation Incident on a Horizontal Surface| NASA |
|ps|(kPa) Surface Pressure| NASA |
|rh2m|(%) Relative Humidity at 2 Meters| NASA |
|t2mdew|(°C) Dew/Frost Point at 2 Meters| NASA |
|t2mwet|(°C) Wet Bulb Temperature at 2 Meters| NASA |
|ws2m| (m/s) Wind Speed at 2 Meters| NASA |
|dnr| (kW-hr/m^2/day) Monthly Direct Normal Radiation| NASA |
|dni| (kW-hr/m^2/day) Monthly Direct Normal Iradiation| NSRDB |
|GDD| Daily Growing degree days with base temperature of 10 celsius degrees|  |
|source_temp| Source of temperature information|  |
|source_rain| Source of rainfall information|  |

[Go back to main](https://github.com/QuantGen/G2F_RESOURCES)
