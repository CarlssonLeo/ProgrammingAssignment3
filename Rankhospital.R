rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        else {
                State_filter <- filter(data, State == state)
                outcome_select <- select(State_filter, Hospital.Name, outcome)
                outcome_select <- outcome_select[complete.cases(outcome_select),]
                final <- arrange(outcome_select,(!!as.symbol(outcome)), Hospital.Name)
              
                if(num == "best"){
                        final[1,1]
                }
                
                if(num == "worst"){
                        final[nrow(final),1]
                }
        
                else{
                        final[num,1]
                }
        }
}

#> source("rankhospital.R")
#> rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"
#> rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"
#> rankhospital("MN", "heart attack", 5000)
#[1] NA