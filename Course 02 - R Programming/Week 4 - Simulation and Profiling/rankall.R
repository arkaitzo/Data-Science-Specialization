#####################################
# 4.Ranking hospitals in all states #
#####################################
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
