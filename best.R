best <- function(state, outcome) {
    ## Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    by_state <- split(outcomes,f=outcomes$State)
    
    ## Check that state and outcome are valid
    pos_states <- unique(outcomes$State)
    pos_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!(state %in% pos_states)){
        stop("invalid state")
    }
    if(!(outcome %in% pos_outcomes)){
        stop("invalid outcome")
    }
    current_state <- by_state[[state]][order(by_state[[state]]$Hospital.Name),]
    
    a <- strsplit(outcome, " ")[[1]]
    a <- paste0(toupper(substr(a, 1, 1)), tolower(substring(a, 2)))
    outcome_con <- paste("Hospital.30.Day.Death..Mortality..Rates.from"
                         ,paste(a,collapse = "."),sep = ".")
    
    current_state <- current_state[!(current_state[[outcome_con]]=="Not Available"),]
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    current_state$Hospital.Name[which.min(current_state[[outcome_con]])]
}