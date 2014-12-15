source("pollutantmean.R")

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
  index <- 1
  ID <- 1
  nobs <- 1
  for(i in id){
    fname <- format_filename(i)
    fpath <- paste0(directory,'/',fname)
    
    df <- read.csv(file = fpath,
                   header = TRUE, 
                   sep = ",",
                   na.strings = c("NA",""),
    )
    
    #build a logical vector that has NA values
    df.good <- complete.cases(df)
    
    df.complete <- df[df.good,]
    if(nrow(df.complete) <= 0) next
    
    freq <- cbind(table(df.complete$ID))
    for(j in 1:nrow(freq)){
      ID[index] <- as.numeric(dimnames(freq)[[1]][j])
      nobs[index] <- freq[j]
      index <- index + 1
    }
  }
  
  data.frame(ID,nobs)

}