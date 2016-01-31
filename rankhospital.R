rankhospital <- function(state, outcome, num = "best") {
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
    if(is.numeric(num) & num>nrow(current_state)){
        return(NA)
    } else if(!is.numeric(num) & !(num == "best" | num == "worst" )){
        stop("num must be an interger or 'best' or 'worst'")
    }
    current_state <- current_state[order(as.numeric(current_state[[outcome_con]])),]
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if(num=="best"){
        current_state$Hospital.Name[which.min(current_state[[outcome_con]])]
    } else if(num=="worst"){
        current_state$Hospital.Name[which.max(current_state[[outcome_con]])]
    } else if(is.numeric(num)){
        current_state$Hospital.Name[num]
    }
    
}