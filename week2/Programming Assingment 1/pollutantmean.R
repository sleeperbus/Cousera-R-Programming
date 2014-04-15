pollutantmean <- function(directory, pollutant, id = 1:332) {
    temp <- id
    temp <- sprintf("%03d.csv", temp)
    files <- file.path(directory, temp)
    data <- do.call("rbind", lapply(files, read.csv))
    m <- round(unlist(lapply(data[pollutant], mean, na.rm = T)), digit = 3)
    m
}