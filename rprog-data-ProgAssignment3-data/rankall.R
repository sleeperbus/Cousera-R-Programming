rankall <- function(outcome, num = "best") {
    ## Read outcome data
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#    data <- read.csv("outcome-of-care-measures.csv")
    data <- data[, c(2, 7, 11, 17, 23)]
    data[, 3] <- as.numeric(data[, 3])
    data[, 4] <- as.numeric(data[, 4])
    data[, 5] <- as.numeric(data[, 5])
    names(data) <- c("hospital", "state", "heart attack", "heart failure",
                     "pneumonia")
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    states <- unique(data$state)
    states <- sort(states)
    
    ## Check that outcome is valid
    if (!any(outcomes == outcome)) stop("invalid outcome") 
    
    result <- do.call("rbind", lapply(states, function(s) {
        r <- data[data$state == s, c("hospital", "state", outcome)]
        r <- r[!is.na(r[, outcome]),]
        r <- r[with(r, order(r[, outcome], r[, "hospital"])), ]  
        
        rows <- nrow(r)
        if (num == "best") n <- 1
        else if (num == "worst") n <- rows
        else n <- num
        
        if (n > rows) result <- c(NA, s)
        else result <- r[n, c("hospital", "state")]
#        else result <- r[n, c("hospital", "state", outcome)]
        result
        
    }));

    return(result)
}

