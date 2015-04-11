CombineZoosToDataframe <- function(...) {
    # Return the data.frame of series of objects "zoo" 
    # Args:
    #   ...: objects of class zoo
    # Returns:
    #   data.frame, which is combined objects "zoo"
    
    # Check args for validity
    list.args <- list(...)
    stopifnot(sapply(list.args, is.zoo))
    
    # Bind to data.frame
    series <- merge.zoo(..., index, all = T)
    series <- cbind(time(series), as.data.frame(series))
    series <- data.frame(series, row.names = NULL)
    colnames(series)[1] <- "date"
    return(series)
    
}
