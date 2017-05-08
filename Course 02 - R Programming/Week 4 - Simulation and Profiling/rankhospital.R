##############################################
# 3. Ranking hospitals by outcome in a state #
##############################################
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv",
                     colClasses = "character")
    
    ## Check that state and outcome are valid
    state.validity <- FALSE
    outcome.validity <- FALSE
    outcome.col <- NULL # Default column index
    
    state.factor <- levels(factor(data$State)) # State abbreviations
    if (state %in% state.factor) {
        state.validity <- TRUE
        state.data <- subset(data, data$State == state) # State data
    } else {
        stop("invalid state")
    }
    if(outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        outcome.validity <- TRUE
        if(outcome == "heart attack") {outcome.col <- 11}
        if(outcome == "heart failure") {outcome.col <- 17}
        if(outcome == "pneumonia") {outcome.col <- 23}
    } else {
        stop("invalid outcome")
    }
    
    # Remove NAs
    state.data.clean <-
        state.data[state.data[, outcome.col] != "Not Available", ]
    # Sort by rate (outcome) first and then alphabetically
    rank <- state.data.clean[
        order(
            as.numeric(state.data.clean[, outcome.col]), # by outcome...
            state.data.clean[, "Hospital.Name"]), # ... and alphabetically
        "Hospital.Name"] # Return column #2 (Hospital.Name)
    
    ## Return hospital name in that state with the given rank 30-day death rate
    if(num == "best") {return(rank[1])}
    else if(num == "worst") {return(rank[length(rank)])}
    else if(num <= length(rank) && num >= 1) {return(rank[num])}
    else {return(NA)}
}
