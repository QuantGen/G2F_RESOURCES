## Phenotypes

The following scripts use data from the GxE initiative of the G2F project available through the following links

 - [2017](https://datacommons.cyverse.org/browse/iplant/home/shared/commons_repo/curated/GenomesToFields_2014_2017_v1/G2F_Planting_Season_2017_v1/a._2017_hybrid_phenotypic_data/g2f_2017_hybrid_data_clean.csv)
 
**Reading the data from one year into R**

```r
token17 <- '?token=ADHZTMMBWS73JO7UGLXGXM3AAXNVM' # Remove this before publish repository
token18 <- '?token=ADHZTMKTJLN6L5PQ7EVWDWLAAXR3M' # Remove this before publish repository
token19 <- '?token=ADHZTMKIQ4HEEGYN7CRWURTAAXSEI' # Remove this before publish repository

dpath <- 'https://raw.githubusercontent.com/QuantGen/G2F_RESOURCES/main/Data/Phenotypes/data_phenotypes_'
fyear <- 2017
fext <- '.csv'
Data <- read.csv(paste0(dpath, fyear, fext, token17), header=T) # Remove this before publish repository
```

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

# Function to rename columns
rename_columns <- function(existing_colname, new_colname){
  lapply(pData, function(x){
    x_col <- grep(existing_colname, colnames(x), ignore.case=T)
    if(is.na(new_colname) & length(x_col) == 1) { x[,x_col] <- NULL } else {
      if(length(x_col) == 1) colnames(x)[x_col] <- new_colname }
    if(length(x_col) > 1) stop(paste('Two columns matched existing_colname:', paste(colnames(x)[x_col], collapse = ' and ')))
    if(length(x_col) < 1) warning('no column matched existing_colname in at least one dataset.')
    return(x)
  })
}

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
pData2 <- do.call(rbind, lapply(pData, function(x) x[myCols]))
```


[main](https://github.com/QuantGen/G2F_RESOURCES)
