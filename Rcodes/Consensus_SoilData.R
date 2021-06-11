# Read datasets
g2fsoil <- read.csv('Data/OutputFiles/g2f_SoilData.csv')
usdasoil <- read.csv('Data/OutputFiles/USDA_SoilData.csv')


# Prepare G2F data before merging
g2fsoil$hz_depb <- g2fsoil$E.Depth * 2.5
g2fsoil <- g2fsoil[g2fsoil$Field.ID != 'ONH1',]
g2fsoil$Field.ID <- gsub('\\s', '', g2fsoil$Field.ID)
g2fsoil$Field.ID <- gsub('NEHDry', 'NEH2', g2fsoil$Field.ID)
g2fsoil$Field.ID <- gsub('W1H1', 'WIH1', g2fsoil$Field.ID)
g2fsoil$Field.ID <- gsub('W1H2', 'WIH2', g2fsoil$Field.ID)
g2fsoil <- g2fsoil[g2fsoil$Field.ID %in% unique(usdasoil$location),]
g2fsoil$year <- sapply(strsplit(g2fsoil$Date.Reported, '/'), function(x) x[3])
# Prepare USDA data
usdasoil$year_loc <- paste0(usdasoil$year, '_', usdasoil$location)


# Create Consensus data
res <- list()
for (year_loc in unique(usdasoil$year_loc)) {
  year <- unlist(strsplit(year_loc, '_'))[1]
  loc <- unlist(strsplit(year_loc, '_'))[2]
  usda_in <- usdasoil$year_loc == year_loc 
  g2f_in <- g2fsoil$Field.ID == loc & g2fsoil$year == year
  g2f_any <- rep(any(g2f_in), sum(usda_in))

  res[[length(res) + 1]] <- data.frame(
    'location' = loc,
    'year' = year,
    'dept' = paste(usdasoil$hz_dept,'-', usdasoil$hz_depb)[usda_in],
    'ph' = ifelse(g2f_any, g2fsoil$X1.1.Soil.pH[g2f_in], usdasoil$ph01mcacl2_r[usda_in]),
    'OM' = ifelse(g2f_any, g2fsoil$Organic.Matter.LOI..[g2f_in], usdasoil$om_r[usda_in]),
    'N' = ifelse(g2f_any, g2fsoil$Nitrate.N.ppm.N[g2f_in], NA),
    'K' = ifelse(g2f_any, g2fsoil$Potassium.ppm.K[g2f_in], NA),
    'S' = ifelse(g2f_any, g2fsoil$Sulfate.S.ppm.S[g2f_in], NA),
    'Ca' = ifelse(g2f_any, g2fsoil$Calcium.ppm.Ca[g2f_in], NA),
    'Mg' = ifelse(g2f_any, g2fsoil$Magnesium.ppm.Mg[g2f_in], NA),
    'Na' = ifelse(g2f_any, g2fsoil$Sodium.ppm.Na[g2f_in], NA),
    'CEC' = ifelse(g2f_any, g2fsoil$CEC.Sum.of.Cations.me.100g[g2f_in], usdasoil$cec7_r[usda_in]),
    'P' = ifelse(g2f_any, g2fsoil$Mehlich.P.III.ppm.P[g2f_in], usdasoil$ptotal_r[usda_in]),
    'sand' = ifelse(g2f_any, g2fsoil$X..Sand[g2f_in], usdasoil$sandtotal_r[usda_in]),
    'silt' = ifelse(g2f_any, g2fsoil$X..Silt[g2f_in], usdasoil$silttotal_r[usda_in]),
    'clay' = ifelse(g2f_any, g2fsoil$X..Clay[g2f_in], usdasoil$claytotal_r[usda_in]),
    'sandvc' = usdasoil$sandvc_r[usda_in],
    'sandco' = usdasoil$sandco_r[usda_in],
    'sandmed' = usdasoil$sandmed_r[usda_in],
    'sandfine' = usdasoil$sandfine_r[usda_in],
    'sandvf' = usdasoil$sandvf_r[usda_in],
    'siltco' = usdasoil$siltco_r[usda_in],
    'siltfine' = usdasoil$siltfine_r[usda_in],
    'ksat' = usdasoil$ksat_r[usda_in],
    'awc' = usdasoil$awc_r[usda_in],
    'wthirdbar' = usdasoil$wthirdbar_r[usda_in],
    'wfifteenbar' = usdasoil$wfifteenbar_r[usda_in],
    'wsatiated' = usdasoil$wsatiated_r[usda_in],
    'kwfact' = usdasoil$kwfact[usda_in],
    'caco3' = usdasoil$caco3_r[usda_in],
    'sar' = usdasoil$sar_r[usda_in],
    'ec' = usdasoil$ec_r[usda_in],
    'cec7' = usdasoil$cec7_r[usda_in])
}

res0 <- do.call(rbind, res)

# Save consensus data
write.csv(res0, file = 'Data/OutputFiles/SoilData.csv', quote = F, row.names = F)
