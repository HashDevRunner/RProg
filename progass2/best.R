source('progassign3.R')

best <- function(state, outcome){
  
  outcome <- tolower(outcome)
  state <- toupper(state)

  #validating outcome
  validate_outcome(outcome)
  
  #validate state
  validate_state(state)
  
  if(outcome == valid_outcomes[1]){ #HEART ATTACK
    good <- complete.cases(hosp_outcomes[c(2,7,11)])
    result = hosp_outcomes[good & hosp_outcomes$State == state,c(2,7,11)]
    result = result[order(result[3],decreasing = FALSE),][1,1]
  }else if( outcome == valid_outcomes[2]){  #HEART FAILURE
    good <- complete.cases(hosp_outcomes[c(2,7,17)])
    result = hosp_outcomes[good & hosp_outcomes$State == state,c(2,7,17)]
    result = result[order(result[3],decreasing = FALSE),][1,1]
  }else if( outcome == valid_outcomes[3]){  #PNEUMONIA
    good <- complete.cases(hosp_outcomes[c(2,7,23)])
    result = hosp_outcomes[good & hosp_outcomes$State == state,c(2,7,23)]
    result = result[order(result[3],decreasing = FALSE),][1,1]
  }
  
  result
}