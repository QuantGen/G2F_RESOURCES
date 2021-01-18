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


