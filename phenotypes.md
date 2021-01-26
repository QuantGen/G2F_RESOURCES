## Phenotypes

The following scripts use data from the GxE initiative of the G2F project available through the following site:

 - [G2F site](https://www.genomes2fields.org/resources/)

Trials follow an incompleted block design where a set of replicated hybrids are in complete blocks with two repetitions and a set of un-replicated hybrids assigned with stratification to one of the two replicates. Stratification ensured nearly equal proportion of families in each replicate. Hybrids were grouped by families that were randomized on each replicate.

**Reading and merging data from multiple years**

```r
files <- c("data_phenotypes_2017.csv", "data_phenotypes_2018.csv", "data_phenotypes_2019.csv")

# Reading from multiple years
pData <- list()
for (filei in files){
  pData[[length(pData) + 1]] <- read.csv(filei, header=T, stringsAsFactors=F, na.strings=c(''))
}

# lower case all column names
pData <- lapply(pData, function(x){colnames(x) <- tolower(colnames(x));x})

# Load function to rename columns
source('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Code/Functions.R')

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
                     'Seed', NA,
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
for (i in 1:nrow(rep_matrix))
  pData <- rename_columns(rep_matrix[i,1], rep_matrix[i,2])

# merge datasets from different years
myCols <- names(which(table(unlist(lapply(pData, colnames))) == 3))
pheno <- do.call(rbind, lapply(pData, function(x) x[myCols]))

# Remove empty and discarded rows
pheno <- pheno[rowSums(is.na(pheno)) != ncol(pheno),]
pheno <- pheno[!pheno$discarded %in% c('Yes', 'yes'),]
pheno <- pheno[!pheno$location == 'GEH1',] # Remove Germany

# Rename locations
pheno$location[pheno$location == 'NEH3'] <- 'NEH2'
pheno$location[pheno$location == 'NEH4'] <- 'NEH2'
pheno$location[pheno$location == 'TXH1-Dry'] <- 'TXH1'
pheno$location[pheno$location == 'TXH1-Early'] <- 'TXH1'
pheno$location[pheno$location == 'TXH1-Late'] <- 'TXH1'
pheno$location[pheno$location == 'TXH3'] <- 'TXH1'
pheno$location[pheno$location == 'ONH1'] <- 'ONH2'
pheno$location[pheno$location == 'NEH1'] <- 'NEH2'

write.csv(pheno, file = '../OutputFiles/phenotypes.csv', quote = F, row.names = F)
```
The resulting file is [phenotypes.csv](https://github.com/QuantGen/G2F_RESOURCES/blob/main/Data/OutputFiles/phenotypes.csv)

The resulting file has the following columns:

|Column|Description|
|------|-----------|
|alley_length| (feet) Length of alley|
|date_anthesis| (mm/dd/yy) Date when 50% of plants exhibit another exertion on more than half of the main tassel spike |
|date_harvest| (mm/dd/yy) Date the plot was harvested |
|date_plant| (mm/dd/yy) Sowing date |
|date_silking| (mm/dd/yy) Silking date |
|grain_moisture| (%) Grain water content at harvest |
|local_check| Name of the check used at each location |
|location| G2F field location name |
|pass| (integer) Designation of field pass. Combination of range and pass form coordinate grid system describing plot location within the field |
|pedigree| Pedigree or hybrid name. Commercial hybrid name in the case of local checks |
|plants_root_lodging| (integer) Number of plants that show root lodging per plot |
|plants_stalk_lodging| (integer) Number of plants bloken between ground level and top ear node at harvest |
|plants_stand| (integer) Number of plants per plot at harvest |
|plot| (integer) Designation of individual experimental unit |
|plot_area| (square feet) Plot area |
|plot_length| (feet) Plot length at harvest |
|plot_weight| (lbs) Shelled grain weight per plot |
|range| (integer) Field range of the plot. Perpendicular to corn rows |
|rep| (integer) Replicate block |
|row_spacing| (inches) Space between rows |
|rows| (integer) Number of rows per plot |
|source| Code assigned by project identifying hybrid or commercial hybrid name in case of local check |
|test_weight| (lbs/bu) Shelled grain weight per bushel |
|year| (integer) year of harvest |
|yield| (bushels per acre at 15.5% moisture assuming 56 lbs per bushel) Grain yield using plot area without alley |





[main](https://github.com/QuantGen/G2F_RESOURCES)
