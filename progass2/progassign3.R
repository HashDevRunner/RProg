
hosp_outcomes <- read.csv("outcome-of-care-measures.csv",
                          colClasses = "character",
                          na.strings = c("Not Available"))


# hosp_30day_mortrate <- as.numeric(hosp_outcomes[,11])
# hist(hosp_30day_mortrate)