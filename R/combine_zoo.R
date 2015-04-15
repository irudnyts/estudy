#' Merge zoo's to the data.frame
#'
#' Return the data.frame of series of objects "zoo"
#' 
#' @param ... objects of class zoo
#' @return data.frame, which is combined objects "zoo"
#' @examples
#' library("tseries"); library("zoo")
#' ALV.DE <- get.hist.quote(instrument = "ALV.DE", start = "2000-01-01",
#'                          end = "2014-12-31", quote = "Open",
#'                          provider = "yahoo", retclass = "zoo")
#' CS.PA <- get.hist.quote(instrument = "CS.PA", start = "2000-01-01",
#'                          end = "2014-12-31", quote = "Open",
#'                          provider = "yahoo", retclass = "zoo")
#' CombineZoosToDataframe(ALV.DE, CS.PA)
CombineZoosToDataframe <- function(...) {
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
