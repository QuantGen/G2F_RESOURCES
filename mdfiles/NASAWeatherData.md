The [Power project](https://power.larc.nasa.gov/) provides solar and meteorological data sets from NASA research for support of renewable energy, building energy efficiency and agricultural needs.
The script uses the "nasapower" package to download agroclimatic variables from The Power project.

It also calculates a theoretical evapotranspiration using the ClimMobTools package. To derive the crop factor (Kc) needed by growth stage, we used Tables 7 and 8 of the following documentation: http://www.fao.org/3/s2022e/s2022e07.htm

The resulting file is [NASAdaily.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/NASAdaily.csv), which has the following data dictionary:

|Column|Description|
|------|-----------|
|date| (yyyy-mm-dd) Sowing date |
|location| G2F field location name |
|rainfall| (mm) Daily rainfall |
|temp| (°C) Daily mean temperature |
|temp_min| (°C) Daily minimum temperature |
|temp_max| (°C) Daily maximum temperature |
|allsky_sfc_sw_dwn| (kW-hr/m^2/day) All Sky Insolation Incident on a Horizontal Surface|
|ps|(kPa) Surface Pressure|
|rh2m|(%) Relative Humidity at 2 Meters|
|t2mdew|(°C) Dew/Frost Point at 2 Meters|
|t2mwet|(°C) Wet Bulb Temperature at 2 Meters|
|ws2m| (m/s) Wind Speed at 2 Meters|
|dnr| (kW-hr/m^2/day) Monthly Direct Normal Radiation| 

[Go back to main](https://github.com/QuantGen/G2F_RESOURCES)

