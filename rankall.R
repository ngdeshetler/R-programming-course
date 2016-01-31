rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomes <-
        read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    pos_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% pos_outcomes)) {
        stop("invalid outcome")
    }
    
    a <- strsplit(outcome, " ")[[1]]
    a <- paste0(toupper(substr(a, 1, 1)), tolower(substring(a, 2)))
    outcome_con <-
        paste("Hospital.30.Day.Death..Mortality..Rates.from"
              ,paste(a,collapse = "."),sep = ".")
    
    if (!is.numeric(num) & !(num == "best" | num == "worst")) {
        stop("num must be an interger or 'best' or 'worst'")
    }
    
    ## Split data by state, alphebetize by hospital name and remove missing cases
    by_state <- split(outcomes,f = outcomes$State)
    pos_states <- names(by_state)
    by_state <- lapply(by_state, function(x) {x[order(x$Hospital.Name),]})
    by_state <- lapply(by_state, function(x,outcome_con){x[!(x[[outcome_con]]== "Not Available"),]},outcome_con)
    
    ## For each state, find the hospital of the given rank
    # Sort by outcome
    by_state <- lapply(by_state,function(x,outcome_con){x[order(as.numeric(x[[outcome_con]])),]},outcome_con)
    # Function to find rank based on "num"
    find_rank <-
        function(current_state,num = num,outcome_con = outcome_con) {
            if (num == "best") {
                current_state$Hospital.Name[which.min(current_state[[outcome_con]])]
            } else if (num == "worst") {
                current_state$Hospital.Name[which.max(current_state[[outcome_con]])]
            } else if (is.numeric(num)) {
                current_state$Hospital.Name[num]
            }
        }
    # apply find_rank to all states
    hospital <-
        sapply(by_state, find_rank,num = num,outcome_con = outcome_con)
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    state=pos_states
    data.frame(hospital,state,row.names = pos_states)
    
}