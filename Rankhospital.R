rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}

#> source("rankhospital.R")
#> rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"
#> rankhospital("MD", "heart attack", "worst")
#3
#[1] "HARFORD MEMORIAL HOSPITAL"
#> rankhospital("MN", "heart attack", 5000)
#[1] NA