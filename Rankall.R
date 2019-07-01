rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
        data <- select(data, 
                       Hospital = Hospital.Name, 
                       State, 
                       "heart attack" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                       "heart failure" = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                       "pneumonia" =Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        rank_frame <- data.frame()
        state_names <- unique(data$State)
        
        ## Check that state and outcome are valid
        if(!state %in% data$State) {
                stop("invalid state")   
        }
        
        if(!outcome %in% colnames(data)){
                stop("invalid outcome")   
        }
        ## For each state, find the hospital of the given rank
        for(i in 1:length(unique(data$State))) {
                
                State_filter <- filter(data, State == state_names[i])
                outcome_select <- select(State_filter, Hospital, outcome, State)
                #outcome_select <- outcome_select[complete.cases(outcome_select),]
                final <- arrange(outcome_select,(!!as.symbol(outcome)), Hospital)
                
                if(num == "best"){
                        rank_frame <- rbind(rank_frame, final[1,])
                }
                
                if(num == "worst"){
                        rank_frame <- rbind(rank_frame, final[nrow(final),])
                }
                
                if(num > nrow(final)){
                        rank_frame <- rbind(rank_frame, final[num,])
                }
                
                else{
                        rank_frame <- rbind(rank_frame, final[num,])
                }
        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        select(rank_frame, State, Hospital) %>% arrange(State)
}

#> source("rankall.R")
#> head(rankall("heart attack", 20), 10)
#hospital state
#AK <NA> AK
#AL D W MCMILLAN MEMORIAL HOSPITAL AL
#AR ARKANSAS METHODIST MEDICAL CENTER AR
#4
#AZ JOHN C LINCOLN DEER VALLEY HOSPITAL AZ
#CA SHERMAN OAKS HOSPITAL CA
#CO SKY RIDGE MEDICAL CENTER CO
#CT MIDSTATE MEDICAL CENTER CT
#DC <NA> DC
#DE <NA> DE
#FL SOUTH FLORIDA BAPTIST HOSPITAL FL
#> tail(rankall("pneumonia", "worst"), 3)
#hospital state
#WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC WI
#WV PLATEAU MEDICAL CENTER WV
#WY NORTH BIG HORN HOSPITAL DISTRICT WY
#> tail(rankall("heart failure"), 10)
#hospital state
#TN WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL TN
#TX FORT DUNCAN MEDICAL CENTER TX
#UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER UT
#VA SENTARA POTOMAC HOSPITAL VA
#VI GOV JUAN F LUIS HOSPITAL & MEDICAL CTR VI
#VT SPRINGFIELD HOSPITAL VT
#WA HARBORVIEW MEDICAL CENTER WA
#WI AURORA ST LUKES MEDICAL CENTER WI
#WV FAIRMONT GENERAL HOSPITAL WV
#WY CHEYENNE VA MEDICAL CENTER WY