## Phenotypes

The following scripts use data from the GxE initiative of the G2F project available through the following site:

 - [G2F site](https://www.genomes2fields.org/resources/)
 
**Reading and mergining data from multiple years**
```r
pData <- list()
for (y in c('2017', '2018', '2019')){
  token <- switch(y, '2017'=token17, '2018'=token18, '2019'=token19) # Remove this before publish repository
  pData[[length(pData) + 1]] <- read.csv(paste0(dpath, y, fext, token), header=T, stringsAsFactors=F, na.strings=c(''))
}

# Add block column to 2019
pData[[3]]$block <- 1

# lower case all column names
pData <- lapply(pData, function(x){colnames(x) <- tolower(colnames(x));x})

# Load function to rename columns
ftoken <- '?token=ADHZTMISK6CIKMVRAHCQRTLAB3UWC' # Remove token
source(paste0('https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Code/Functions.R', ftoken))

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
for (i in 1:nrow(rep_matrix)) {
  pData <- rename_columns(rep_matrix[i,1], rep_matrix[i,2])
}

# merge datasets from different years
myCols <- names(which(table(unlist(lapply(pData, colnames))) == 3))
pheno <- do.call(rbind, lapply(pData, function(x) x[myCols]))
```


[main](https://github.com/QuantGen/G2F_RESOURCES)
