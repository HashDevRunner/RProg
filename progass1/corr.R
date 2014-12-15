source("pollutantmean.R")
source("complete.R")

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  index <- 1
  id <- 1:332
  corr <- numeric()
  for(i in id){
    fname <- format_filename(i)
    fpath <- paste0(directory,'/',fname)
    
    df <- read.csv(file = fpath,
                   header = TRUE, 
                   sep = ",",
                   na.strings = c("NA",""),
    )
    df.good <- complete.cases(df)
    df.complete <- df[df.good,]
    
    
    complete_cases <- complete(directory,i)[,2]
    if( complete_cases >= threshold ){
      corr[index] <- cor(df.complete$sulfate,df.complete$nitrate, use = "na.or.complete")
      index <- index + 1
    }
  }
  
  if(length(corr) > 0){
    corr
  }else{
    corr[index] = 0
  }
  
}