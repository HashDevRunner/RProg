source("progassign3.R")
source("best.R")

num_possible <- c("best","worst")

rankall <- function( outcome, num ){

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
  
  ##process
  good <- complete.cases(hosp_outcomes[c(2,7,field_num)])
  hospitals <- hosp_outcomes[good,c(2,7,field_num)]
  names(hospitals)[3] <- c("MortRate")

  sp <- split(hospitals,hospitals$State)
  res <- sapply(sp, function(x, num) {
    # Order by Deaths and then HospitalName
     x = x[order(x$MortRate, x$Hospital.Name),]

    # Return
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital.Name[1])
      }
      else if(num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  },num)
  
  data.frame(hospital=unlist(res), state=names(res))

}