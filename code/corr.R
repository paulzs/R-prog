corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  file_list <- list.files(directory, full.names = TRUE)
  
  c = numeric(length = 0)
  
  for(i in 1:length(file_list)){
    dat <- read.csv(file_list[i])
    s <- complete.cases(dat$sulfate) 
    n <- complete.cases(dat$nitrate)
    ss <- dat$sulfate 
    ns <- dat$nitrate
    b <- s & n
    ss <- ss[b]
    ns <- ns[b]
    ss <- ss[!is.na(ss)]
    ns <- ns[!is.na(ss)]
    if(length(b[b==TRUE]) > threshold){
      c = c(c,cor(ns, ss))
    }
    else{
      next
    }
  }
  
  return(c)
  
}