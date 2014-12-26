
hosp_outcomes <- read.csv("outcome-of-care-measures.csv",
                          colClasses = "character",
                          na.strings = c("Not Available"))


hospitals <- read.csv("hospital-data.csv",
                      colClasses = "character")

valid_outcomes <- c("heart attack",
                    "heart failure",
                    "pneumonia")

outcome_vector <- c(2,7,11,17,23)
hosp_outcomes[,11] <- as.numeric(hosp_outcomes[,11])
hosp_outcomes[,17] <- as.numeric(hosp_outcomes[,17])
hosp_outcomes[,23] <- as.numeric(hosp_outcomes[,23])
# hosp_30day_mortrate <- as.numeric(hosp_outcomes[,11])
# hist(hosp_30day_mortrate)

validate_state <- function(state){
  #validating state
  if(!state %in% hosp_outcomes[,7]) stop("invalid state")
}

validate_outcome <- function(outcome){
  if(!outcome %in% valid_outcomes) stop("invalid outcome")
  which( valid_outcomes == outcome ) #return index number
}