#' Clean the series
#'
#' Return the data.frame of series without NA's. copmlete.cases is used
#' 
#' @param ... objects of class zoo
#' @return data.frame of series without NA's
CleanSeries.zoo <- function(...) {
    # Check args for validity
    list.args <- list(...)
    stopifnot(sapply(list.args, is.zoo))
    
    series <- CombineZoosToDataframe(...)
    message(paste(as.character(sum(!complete.cases(series))), "observations",
                  "were deleted"))
    return(series[complete.cases(series), ])
}

#' Clean the series
#'
#' Return the data.frame of series without NA's. copmlete.cases is used
#' 
#' @param series data.frame with series
#' @return data.frame of series without NA's
CleanSeries.data.frame <- function(series) {
    message(paste(as.character(sum(!complete.cases(series))), "observations",
                  "were deleted"))
    return(series[complete.cases(series), ])
}