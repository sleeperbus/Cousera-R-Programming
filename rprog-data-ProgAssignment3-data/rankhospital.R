rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Return hospital name in that state with the given rank 30-day death rate
    data.state <- data[data$State == state, ]
    complete <- data.state[complete.cases(
        data.state[, c("Hospital.Name", outcome)]), c("Hospital.Name", outcome)]
    result <- complete[with(complete, 
                  order(complete[, outcome], complete[, "Hospital.Name"])),  ]
    if (num == "best")
        ret <- result[1, 1]
    else if (num == "worst")
        ret <- result[nrow(result), 1]
    else {
        if (num > nrow(result))
            ret <- c(NA)
        else
            ret <- result[num, 1]
    } 
    return(ret)
}