# Format G2F soil data
soil18 <- read.csv('Data/Metadata/g2f_2018_soil_data.csv')
soil19 <- read.csv('Data/Metadata/g2f_2019_soil_data.csv')
soil <- rbind(soil18, soil19)

# Remove grower name and date received
soil[, c(1,3)] <- NULL

# change colnames
colnames(soil) <- c('location','year','hz_depb','ph','buffer_ph',
                    'sol_salts','textn','om','nitrate_N','n','k','sulfate_s',
                    'ca','mg','na','cec','h_perc','k_perc','ca_perc','mg_perc',
                    'na_perc','p','sandtotal_r','silttotal_r','claytotal_r','texture')

# format year column
soil$year <- sapply(strsplit(soil$year, '/'), function(x)x[3])

write.csv(soil, file = 'Data/OutputFiles/g2f_soil.csv', quote = F, row.names = F)
