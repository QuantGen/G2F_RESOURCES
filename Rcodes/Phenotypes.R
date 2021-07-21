# Files that contain phenotype data
files <- list.files('Data/Phenotypes/')

# Read files
pData <- list()
for (filei in files) pData[[length(pData) + 1]] <- read.csv(paste0('Data/Phenotypes/', filei), header=T, stringsAsFactors=F, na.strings=c(''))

# lower case all column names
pData <- lapply(pData, function(x){colnames(x) <- tolower(colnames(x));x})

# Load function to rename columns
source('Tools/Functions.R')

# Rename columns
col_names <- data.frame(
  old_names = c('year', '(field.*location)', 'pedigree', 'source', 'replicate', '(plot.*area)', 'seed', '(date.*planted)', '(date.*harvest)',
                'anthesis', '(silking.*date)', '(stand.*plants)', '(grain.*moisture)', '(grain.*yield)'),
  new_names = c('year', 'location', 'pedigree', 'source', 'rep', 'plot_area', 'seed_number', 'date_plant', 'date_harvest', 'date_anthesis',
                'date_silking', 'plants_stand', 'grain_moisture', 'yield'))
for (i in 1:nrow(col_names)) pData <- rename(col_names$old_names[i], col_names$new_names[i], pData)

# merge datasets from different years
pheno <- do.call(rbind, lapply(pData, function(x) x[,col_names$new_names]))

# Remove empty rows
pheno <- pheno[rowSums(is.na(pheno)) != ncol(pheno),]
pheno <- pheno[!is.na(pheno$yield) & !pheno$yield == 'NA',]

# Remove Germany and Ontario, Canada
pheno <- pheno[!pheno$location == 'GEH1',]
pheno <- pheno[!pheno$location == 'ONH1',]
pheno <- pheno[!pheno$location == 'ONH2',]

# Rename locations
pheno$location[pheno$location == 'IAH1a'] <- 'IAH2'
pheno$location[pheno$location == 'IAH1b'] <- 'IAH3'
pheno$location[pheno$location == 'IAH1c'] <- 'IAH4'
pheno$location[pheno$location == 'TXH1-Dry'] <- 'TXH1'
pheno$location[pheno$location == 'TXH1-Early'] <- 'TXH1'
pheno$location[pheno$location == 'TXH1-Late'] <- 'TXH1'
pheno$location[pheno$location %in% c('TXH2', 'TXH3')] <- 'TXH1'

# Change yield measure to kg/ha
pheno$yield <- as.numeric(pheno$yield) * 62.77

# Fix date format
pheno$date_plant <- fix_date(pheno$date_plant)
pheno$date_harvest <- fix_date(pheno$date_harvest)
pheno$date_anthesis <- fix_date(pheno$date_anthesis)
pheno$date_silking <- fix_date(pheno$date_silking)

# Save file
write.csv(pheno, file = 'Data/OutputFiles/phenotypes.csv', quote = F, row.names = F)
