best <- function(state, outcome) {
        #Load dplyr-package
        library(tidyverse)
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
        data <- select(data, 
                       Hospital.Name, 
                       State, 
                       "heart attack" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                       "heart failure" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                       "pneumonia" =Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        ## Check that state and outcome are valid
        if(!state %in% data$State) {
                stop("invalid state")   
        }
        
        if(!outcome %in% colnames(data)){
                stop("invalid outcome")   
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        else {
                State_filter <- filter(data, State == state)
                outcome_select <- select(State_filter, Hospital.Name, outcome)
                final <- outcome_select[order(outcome_select[outcome]),]
                final[1,1]
        }
       
}


#> source("best.R")
#> best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
#> best("TX", "heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"
#> best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"
#> best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
#> best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
#> best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome
#>