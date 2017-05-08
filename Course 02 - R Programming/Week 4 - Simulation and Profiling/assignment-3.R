#######################################################
# 1. Plot the 30-day mortality rates for heart attack #
#######################################################
remove(list = ls())

# Read the outcome data
outcome <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv",
                    colClasses = "character")
# Some exploration
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)
str(outcome)

# Histogram of the 30-day death rates from heart attack (col #11)
outcome[, 11] <- as.numeric(outcome[, 11]) # Coercing the column to be numeric
hist(outcome[, 11])



###########################################
# 2. Finding the best hospital in a state #
###########################################
remove(list = ls())
best <- function(state, outcome) {
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
    
    ## Return hospital name in that state with lowest 30-day death rate
    return(rank[1])
}



##############################################
# 3. Ranking hospitals by outcome in a state #
##############################################
remove(list = ls())
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



#####################################
# 4. Ranking hospitals in all states #
#####################################
remove(list = ls())
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv",
                     colClasses = "character")
    
    ## Check that state and outcome are valid
    if(outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        if(outcome == "heart attack") {outcome.col <- 11}
        else if(outcome == "heart failure") {outcome.col <- 17}
        else {outcome.col <- 23}
        data <- subset(data, select = c("Hospital.Name",
                                        "State",
                                        names(data)[outcome.col]))
    } else {
        stop("invalid outcome")
    }
    
    # Remove NAs
    data.clean <-
        data[data[, 3] != "Not Available", ]
    # Sort by rate (outcome) first and then alphabetically by state and hospital
    ordered.rank <- as.data.frame(data.clean[
        order(
            data.clean[, "State"], # ... and alphabetically by state...
            as.numeric(data.clean[, 3]), # by outcome...
            data.clean[, "Hospital.Name"]), # ... and hospital
        c(-3)]) # Return columns #1 (Hospital.Name) and #2 (State)
    
    rank <- as.data.frame( matrix(ncol = 2) )
    names(rank) <- c("hospital", "state")
    index = 1
    if(num == "best") {
        for(state in levels(as.factor(ordered.rank$State))) {
            position <- 1
            rank[index, ] <-
                ordered.rank[ordered.rank$State == state, ][position, ]
            index <- index + 1
        }
    }
    else if(num == "worst") {
        for(state in levels(as.factor(ordered.rank$State))) {
            position <- nrow(ordered.rank[ordered.rank$State == state, ])
            rank[index, ] <-
                ordered.rank[ordered.rank$State == state, ][position, ]
            index <- index + 1
        }
    }
    else {
        for(state in levels(as.factor(ordered.rank$State))) {
            position <- num
            rank[index, ] <-
                ordered.rank[ordered.rank$State == state, ][position, ]
            index <- index + 1
        }
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    return(rank)
}
