# Function to rename columns

rename_columns <- function(existing_colname, new_colname, mydata = pData){
  lapply(mydata, function(x){
    x_col <- grep(existing_colname, colnames(x), ignore.case=T)
    if(is.na(new_colname) & length(x_col) == 1) { x[,x_col] <- NULL } else {
      if(length(x_col) == 1) colnames(x)[x_col] <- new_colname }
    if(length(x_col) > 1) stop(paste('Two columns matched existing_colname:', paste(colnames(x)[x_col], collapse = ' and ')))
    if(length(x_col) < 1) warning('no column matched existing_colname in at least one dataset.')
    return(x)
  })
}

#####

# Calculate daily rainfall from a dataset with columns 'valid' and 'rainfall'

calculate_daily <- function(x) {
  require(tidyverse)
  options(dplyr.summarise.inform = FALSE) # added
  if (!all(c('rainfall', 'valid') %in% colnames(x))) stop('columns named (rainfall) and (valid) are needed.')
  precip <- x %>% 
    filter(!is.na(rainfall)) %>% 
    select(valid, rainfall) %>% 
    mutate(date = with_tz(as.POSIXct(valid, tz='UCT'),
                          "America/New_York"),
           date = date-dst(date)*3600,
           year = year(date),
           month = month(date),
           day = day(date),
           hour = hour(date),
           minute = minute(date)) %>% 
    group_by(year, month, day, hour) 
  
  modalminute <- precip %>%
    arrange(desc(minute), .by_group=TRUE) %>% 
    mutate(maxminute = minute[which.max(rainfall)]) %>% 
    {which.max(tabulate(.$maxminute))}
  
  prec <- precip %>% 
    filter(minute <= modalminute) %>% 
    summarize(hourlyprecip = max(rainfall, na.rm=TRUE)) %>% 
    group_by(year, month, day) %>% 
    summarize('rainfall' = sum(hourlyprecip, na.rm=TRUE))
  prec <- as.data.frame(prec)
  prec$date <- date(ISOdate(prec$year, prec$month, prec$day))
  prec[,5:4]
}

