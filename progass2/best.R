source('progassign3.R')


hospitals <- read.csv("hospital-data.csv",
                      colClasses = "character")

valid_outcomes <- c("heart attack",
                    "heart failture",
                    "pneumonia")

outcome_vector <- c(2,7,11,17,23)
hosp_outcomes[,11] <- as.numeric(hosp_outcomes[,11])
hosp_outcomes[,17] <- as.numeric(hosp_outcomes[,17])
hosp_outcomes[,23] <- as.numeric(hosp_outcomes[,23])

best <- function(state, outcome){
  
  outcome <- tolower(outcome)
  state <- toupper(state)

  #validating outcome
  if(!outcome %in% valid_outcomes) stop("invalid outcome")
  
  #validating state
  if(!state %in% hosp_outcomes[,7]) stop("invalid state")
  
  if(outcome == valid_outcomes[1]){
    good <- complete.cases(hosp_outcomes[c(2,7,11)])
    result = hosp_outcomes[good & hosp_outcomes$State == state,c(2,7,11)]
    result = result[order(result[3],decreasing = FALSE),][1,1]
  }else if( outcome == valid_outcomes[2]){
    good <- complete.cases(hosp_outcomes[c(2,7,17)])
    result = hosp_outcomes[good & hosp_outcomes$State == state,c(2,7,17)]
    result = result[order(result[3],decreasing = FALSE),][1,1]
  }else if( outcome == valid_outcomes[3]){
    good <- complete.cases(hosp_outcomes[c(2,7,23)])
    result = hosp_outcomes[good & hosp_outcomes$State == state,c(2,7,23)]
    result = result[order(result[3],decreasing = FALSE),][1,1]
  }
  
  result
}