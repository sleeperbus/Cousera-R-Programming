best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- data[, c(2, 7, 11, 17, 23)]
    data[, 3] <- as.numeric(data[, 3])
    data[, 4] <- as.numeric(data[, 4])
    data[, 5] <- as.numeric(data[, 5])
    names(data) <- c("Hospital.Name", "State", "heart attack", "heart failure",
                     "pneumonia")
    outcomes <- c("heart attack", "heart failure", "pneumonia")
          
    ## Check that state and outcome are valid
    states <- unique(data$State)
    if (!any(states == state)) stop("invalid state")
    if (!any(outcomes == outcome)) stop("invalid outcome") 
    
    ## Return hospital name in that state with lowest 30-day death rate
    data <- data[data$State == state, ]
    win <- data[with(data, order(data[, outcome], data[, "Hospital.Name"])), 1][1]
    win
}