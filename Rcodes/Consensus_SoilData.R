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

# Prepare USDA data before merging
usdanew <- usdasoil[0,]

for (i in 1:nrow(g2fsoil)) {
  tmp <- usdasoil[usdasoil$hz_depb <= g2fsoil$hz_depb[i] & usdasoil$location == g2fsoil$Field.ID[i],]
  for (j in 1:ncol(tmp))
    usdanew[i,j] <- ifelse(class(tmp[,j]) == 'character', tmp[1,j], mean(tmp[,j], na.rm = T))
}

# Create Consensus data
soil_data <- data.frame('location' = g2fsoil$Field.ID,
                        'year' = sapply(strsplit(g2fsoil$Date.Reported, '/'), function(x)x[3]),
                        'dept' = g2fsoil$hz_depb,
                        'ph' = g2fsoil$X1.1.Soil.pH,
                        'OM' = g2fsoil$Organic.Matter.LOI..,
                        'N' = g2fsoil$Nitrate.N.ppm.N,
                        'K' = g2fsoil$Potassium.ppm.K,
                        'S' = g2fsoil$Sulfate.S.ppm.S,
                        'Ca' = g2fsoil$Calcium.ppm.Ca,
                        'Mg' = g2fsoil$Magnesium.ppm.Mg,
                        'Na' = g2fsoil$Sodium.ppm.Na,
                        'CEC' = g2fsoil$CEC.Sum.of.Cations.me.100g,
                        'P' = g2fsoil$Mehlich.P.III.ppm.P,
                        'sand' = g2fsoil$X..Sand,
                        'silt' = g2fsoil$X..Silt,
                        'clay' = g2fsoil$X..Clay,
                        'sandvc' = usdanew$sandvc_r,
                        'sandco' = usdanew$sandco_r,
                        'sandmed' = usdanew$sandmed_r,
                        'sandfine' = usdanew$sandfine_r,
                        'sandvf' = usdanew$sandvf_r,
                        'siltco' = usdanew$siltco_r,
                        'siltfine' = usdanew$siltfine_r,
                        'ksat' = usdanew$ksat_r,
                        'awc' = usdanew$awc_r,
                        'wthirdbar' = usdanew$wthirdbar_r,
                        'wfifteenbar' = usdanew$wfifteenbar_r,
                        'wsatiated' = usdanew$wsatiated_r,
                        'kwfact' = usdanew$kwfact,
                        'caco3' = usdanew$caco3_r,
                        'sar' = usdanew$sar_r,
                        'ec' = usdanew$ec_r,
                        'cec7' = usdanew$cec7_r)

# Save consensus data
write.csv(soil_data, file = 'Data/OutputFiles/SoilData.csv', quote = F, row.names = F)
