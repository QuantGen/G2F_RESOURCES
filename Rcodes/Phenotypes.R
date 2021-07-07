# Files that contain phenotype data
files <- c("data_phenotypes_2017.csv", "data_phenotypes_2018.csv", "data_phenotypes_2019.csv")

# Read files
pData <- list()
for (filei in files) pData[[length(pData) + 1]] <- read.csv(paste0('Data/Phenotypes/', filei), header=T, stringsAsFactors=F, na.strings=c(''))

# lower case all column names
pData <- lapply(pData, function(x){colnames(x) <- tolower(colnames(x));x})

# Load function to rename columns
source('Tools/Functions.R')

# Renaming columns
rep_matrix <- matrix(c('(field.*location)', 'location',
                     'RecId', NA,
                     'replicate', 'rep',
                     '(local.*check)', 'local_check',
                     '(Plot.*Length.*Field)', 'plot_length',
                     '(Alley.*Length)', 'alley_length',
                     '(Row.*Spacing)', 'row_spacing',
                     '(Plot.*Area)', 'plot_area',
                     '(Rows.*Plot)', 'rows',
                     'block', NA,
                     '(Packet.*Plot)', NA,
                     '(Kernels.*Packet)', NA,
                     'Seed', 'seed_number',
                     '(Date.*Planted)', 'date_plant',
                     '(Date.*Harvest)', 'date_harvest',
                     'Anthesis', 'date_anthesis',
                     '(Silking.*date)', 'date_silking',
                     'Pollen', NA,
                     '(Silk.*DAP)', NA,
                     '(Plant.*Height)', 'height_plant',
                     '(Ear.*Height)', 'height_plant',
                     '(Stand.*plants)', 'plants_stand',
                     '(Root.*Lodging)', 'plants_root_lodging',
                     '(Stalk.*Lodging)', 'plants_stalk_lodging',
                     '(grain.*moisture)', 'grain_moisture',
                     '(Test.*Weight)', 'test_weight',
                     '(Plot.*Weight)', 'plot_weight',
                     '(Grain.*Yield)', 'yield',
                     '(Plot.*Discarded)', 'discarded',
                     'comments', NA,
                     'additional', NA), ncol=2, byrow=T)
# Ignore warnings
for (i in 1:nrow(rep_matrix))
  pData <- rename_columns(rep_matrix[i,1], rep_matrix[i,2])

# merge datasets from different years
myCols <- names(which(table(unlist(lapply(pData, colnames))) == 3))
pheno <- do.call(rbind, lapply(pData, function(x) x[myCols]))

# Remove empty and discarded rows
pheno <- pheno[rowSums(is.na(pheno)) != ncol(pheno),]
pheno <- pheno[!pheno$discarded %in% c('Yes', 'yes'),]
pheno <- pheno[!pheno$location == 'GEH1',] # Remove Germany
pheno$discarded <- NULL

# Rename locations
pheno$location[pheno$location == 'NEH3'] <- 'NEH2'
pheno$location[pheno$location == 'NEH4'] <- 'NEH2'
pheno$location[pheno$location == 'TXH1-Dry'] <- 'TXH1'
pheno$location[pheno$location == 'TXH1-Early'] <- 'TXH1'
pheno$location[pheno$location == 'TXH1-Late'] <- 'TXH1'
pheno$location[pheno$location %in% c('TXH2', 'TXH3')] <- 'TXH1'
pheno$location[pheno$location == 'ONH1'] <- 'ONH2'
pheno$location[pheno$location == 'NEH1'] <- 'NEH2'

# Change yield measure to kg/ha (ignore warning)
pheno$yield <- as.numeric(pheno$yield) * 62.77

# Change date format
for (i in grep('date', colnames(pheno)))
  pheno[,i] <- as.Date(strptime(pheno[,i], "%m/%d/%y"))


write.csv(pheno, file = 'Data/OutputFiles/phenotypes.csv', quote = F, row.names = F)
