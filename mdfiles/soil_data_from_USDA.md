The script gets data from the SSURGO database. This database contains information about soil as collected by the National Cooperative Soil Survey - USDA.
We offer a function to download the data and, in the case of multiple horizons for a particular location, to combine horizons by a weighted mean of standard horizons by depth (if the argument combineMultipleHorizons is set to TRUE).

Results are saved in [SoilData.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/SoilData.csv). Data dictionary for this file:

|Column|Description|
|------|-----------|
|location| G2F field location name |
|lat| (decimal degrees) latitude of the location, used to get soil data |
|lon| (decimal degrees) longitude of the location, used to get soil data |
|saverest| Time when the record was saved |
|muname| Mapunit name |
|compname| Component name |
|hz_dept| (cm) distance from the top to the upper boundary of the soil horizon|
|hz_depb| (cm) distance from the top to the base of the soil horizon|
|sieveno4_r| (%) soil fraction passing a number 4 sieve (4.7 mm square opening) |
|sieveno10_r| (%) soil fraction passing a number 10 sieve (2 mm square opening) |
|sieveno40_r| (%) soil fraction passing a number 40 sieve (0.42 mm square opening) |
|sieveno200_r| (%) soil fraction passing a number 200 sieve (0.074 mm square opening) |
|sandtotal_r| (%) mineral particles 0.05mm to 2.0mm in equivalent diameter |
|sandvc_r| (%) mineral particles 1.0mm to 2.0mm in equivalent diameter |
|sandco_r| (%) mineral particles 0.5mm to 1.0mm in equivalent diameter |
|sandmed_r| (%) mineral particles 0.25mm to 0.5mm in equivalent diameter |
|sandfine_r| (%) mineral particles 0.10mm to 0.25mm in equivalent diameter |
|sandvf_r| (%) mineral particles 0.05mm to 0.10mm in equivalent diameter |
|silttotal_r| (%) mineral particles 0.002mm to 0.05mm in equivalent diameter |
|siltco_r| (%) mineral particles 0.02mm to 0.05mm in equivalent diameter |
|siltfine_r| (%) mineral particles 0.002mm to 0.02mm in equivalent diameter |
|claytotal_r| (%) mineral particles less than 0.002mm in equivalent diameter |
|om_r| (%) weight of decomposed plant and animal residue expressed as a weight percentage of the less than 2mm of soil material |
|partdensity| (m/v) mass per unit of volume (not including pore space) of the solid soil particle either mineral or organic |
|ksat_r| amount of water that would move vertically through a unit area of saturated soil in unit time under unit hydraulic gradient |
|awc_r| amount of water that an increment of soil depth, inclusive of fragments, can store that is available to plants |
|wtenthbar_r| volumetric content of soil water retained at a tension of 1/10 bar (10 kPa), expressed as a percentage of the whole soil |
|wthirdbar_r| volumetric content of soil water retained at a tension of 1/3 bar (33 kPa), expressed as a percentage of the whole soil |
|wfifteenbar_r| volumetric content of soil water retained at a tension of 15 bars (1500 kPa), expressed as a percentage of the whole soil |
|wsatiated_r| estimated volumetric soil water content at or near zero bar tension, expressed as a percentage of the whole soil|
|kwfact| erodibility factor which quantifies the susceptibility of soil particles to detachment and movement by water. This factor is adjusted for the effect of rock fragments |
|caco3_r| quantity of Carbonate (CO3) in the soil expressed as CaCO3 and as a weight percentage of the less than 2 mm size fraction |
|gypsum_r| percent by weight of hydrated calcium sulfate in the less than 20 mm fraction of soil|
|sar_r| measure of the amount of Sodium (Na) relative to Calcium (Ca) and Magnesium (Mg) in the water extract from saturated soil paste |
|ec_r| electrical conductivity of an extract from saturated soil paste |
|cec7_r| amount of readily exchangeable cations that can be electrically adsorbed to negative charges in the soil, soil constituent, or other material, at pH 7.0, as estimated by the ammonium acetate method |
|ecec_r| sum of NH4OAc extractable bases plus KCl extractable aluminum |
|ph01mcacl2_r| negative logarithm to base of 10 or the hydrogen ion activity in the soil, using the 0.01M CaCl2 method, in a 1:2 soil:solution ratio |
|ptotal_r| total phosphorous content of the soil, measured after total dissolution of a size fraction of the soil material.  It is reported as a gravimetric percent oxide of the size fraction used |
|sumbases_r| sum of NH4OAc extractable bases (pH 7.0), reported on less than 2mm base |
|freeiron_r| secondary iron oxides such as geothite, hematite, ferrihydrite, lepidocrocite and maghemite. It is iron extracted by dithionite-citrate|
|extracid_r| soil exchangeable hydrogen ions that may become active by cation exchange |
|extral_r| aluminum extracted in 1 normal potassium chloride |
|pbray1_r| amount of phosphorous in the less than 2mm fraction, that is extractable using the Bray1 method |


