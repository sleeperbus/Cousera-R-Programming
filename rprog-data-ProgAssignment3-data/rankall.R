rankall <- function(outcome, num = "best") {
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- data[, c(2, 7, 11, 17, 23)]
    data[, 3] <- as.numeric(data[, 3])
    data[, 4] <- as.numeric(data[, 4])
    data[, 5] <- as.numeric(data[, 5])
    names(data) <- c("hospital", "state", "heart attack", "heart failure",
                     "pneumonia")
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    states <- unique(data$state)
    
    ## Check that outcome is valid
    if (!any(outcomes == outcome)) stop("invalid outcome") 
    
    result <- do.call("rbind", lapply(states, function(s) {
        data.state <- data[data$state == s, ]
        data.state <- data.state[!is.na(data.state[outcome]), ]
        data.state <- data.state[num, c("hospital", "state", outcome)]
    }));
#    result <- result[with(result, order(state)), ]
}