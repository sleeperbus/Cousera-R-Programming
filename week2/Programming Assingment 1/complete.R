complete <- function(directory, id = 1:332) {
    files <- sprintf("%03d.csv", id)
    files <- file.path(directory, files)
    
    dataCount <- function(file) {
        data <- read.csv(file)
        count <- sum(complete.cases(data))
        count
    }
    counts <- do.call("rbind", lapply(files, dataCount))
    data <- data.frame(id, counts)
    data
}
