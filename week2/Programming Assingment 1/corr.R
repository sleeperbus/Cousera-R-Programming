corr <- function(directory, threshold = 0) {
    readData <- function(id) {
        fileName <- sprintf("%03d.csv", id)
        fileName <- file.path(directory, fileName)
        data <- read.csv(fileName)
        data
    }
    
    singleCor <- function(id) {
        data <- readData(id)
        val <- cor(data[2], data[3], use = "complete.obs")
        val 
    }
    
    objs <- complete(directory, 1:332)    
    ids <- objs[which(objs[2] > threshold), 1]
    data <- do.call("list", lapply(ids, singleCor))
    data
}