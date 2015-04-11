CleanSeries.zoo <- function(...) {
    # Return the data.frame of series without NA's. copmlete.cases is used 
    # Args:
    #   ...: objects of class zoo
    # Returns:
    #   data.frame of series without NA's
    
    # Check args for validity
    list.args <- list(...)
    stopifnot(sapply(list.args, is.zoo))
    
    series <- CombineZoosToDataframe(...)
    return(series[complete.cases(series), ])
}

CleanSeries.data.frame <- function(series) {
    # Return the data.frame of series without NA's. copmlete.cases is used 
    # Args:
    #   series: data.frame with series
    # Returns:
    #   data.frame of series without NA's
    return(series[complete.cases(series), ])
}