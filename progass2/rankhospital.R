source("progassign3.R")
source("best.R")

num_possible <- c("best","worst")
rankhospital <- function(state,outcome,num){
  
  state <- toupper(state)
  outcome <- tolower(outcome)
  if(class(num) == "character" & (!num %in% num_possible)) stop("invalid num")

  #validating outcome
  outcome_idx <- validate_outcome(outcome)
  #get field mapped field number
  field_num <- if(outcome_idx == 1){
    11  #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }else if(outcome_idx == 2){
    17  #"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }else if(outcome_idx == 3){
    23  #"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  #validating state
  validate_state(state)

  ##process
  good <- complete.cases(hosp_outcomes[c(2,7,field_num)])
  result <- hosp_outcomes[good & hosp_outcomes$State == state,c(2,7,field_num)]
  result <- result[order(result[3],result[2],decreasing = FALSE),]
  
  if(num == "best"){ 
    num <- 1
  }else if( num == "worst" ){
    num <- nrow(result)
  }
  
  print(result[num,"Hospital.Name"])
  result[num,"Hospital.Name"]
}