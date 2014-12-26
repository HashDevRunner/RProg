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
  require(plyr)
  good <- complete.cases(hosp_outcomes[c(2,7,field_num)])
  hospitals <- hosp_outcomes[good,c(2,7,field_num)]
  hospitals <- arrange(hospitals,desc(hospitals[3]),
                       hospitals$State,
                       hospitals$Hospital.Name)

  rank_hosp <- c()
  u_hosp <- unique(hospitals$State)
  for( st in unique(hospitals$State) ){
    st_hosp <- hospitals[hospitals$State == st,]
    st_hosp$StateRank <- c(1:nrow(st_hosp))
    rank_hosp <- rbind(rank_hosp,st_hosp)
  }
  
  if(num == "best"){ 
    num <- 1
  }
  
  result_final <- rank_hosp[rank_hosp$StateRank == num,c("Hospital.Name","State")]
  names(result_final) <- c("Hospital","State")
  arrange(result_final,result_final$State)
}