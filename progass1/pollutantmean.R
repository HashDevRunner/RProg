format_filename <- function( id = 1){

  if( nchar(id) ==  1){
    fname <- paste0("00",id,".csv")
  }else if( nchar(id) == 2){
    fname <- paste0("0",id,".csv")
  }else{
    fname <- paste0(id,".csv")
  }
  fname
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  if(!(pollutant == "sulfate" || pollutant == "nitrate")) return(NA)  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  index <- integer(1)
  calc_mean <- numeric()
  for(i in id){
    fname <- format_filename(i)
    fpath <- paste0(directory,'/',fname)
    
    df <- read.csv(file = fpath,
                    header = TRUE, 
                    sep = ",",
                    na.strings = c("NA",""),
                    )
    
    #build a logical vector that has NA values
    df.good <- is.na(df[,pollutant])

    #now create numerical vector for calculation of mean
    df.pollutant <- df[!df.good,pollutant]
    if(length(df.pollutant) <= 0) next

    #build a big list of values
    for( j in 1:length(df.pollutant)){ 
      calc_mean[index] <- df.pollutant[j]
      index <- index + 1
    }
  }
  mean_return <- mean(calc_mean)
}