complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  file_list <- list.files(directory, full.names = TRUE)
  
  dat <- data.frame()
  ids <- c()
  nobs <- c()
  
  for (i in id) {
    dat <- read.csv(file_list[i])
    ids <- c(ids,as.numeric(dat[1,]$ID))
    n <- complete.cases(dat$sulfate) & complete.cases(dat$nitrate)
    nobs <- c(nobs,as.numeric(length(n[n==TRUE])))
  }
  
  result_dat <- data.frame(ids,nobs)
  
  colnames(result_dat) <- c("id","nobs")
  
  return(result_dat)
}