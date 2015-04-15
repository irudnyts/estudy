# S3 methodology for dispathing functions
GetRates <- function(prices) UseMethod(generic = "GetRates", object = prices)

#' Calculate rates of return for given series of prices.
#'
#' Return the series of rates of return for given series of prices.
#' 
#' S3 generics, which dispatches for data.frames, zoo's and regular vector.
#' 
#' @param prices objects of class zoo, containing prices of the stock
#' @return object of class zoo, which comtains rates of return
#' @examples
#' library("tseries"); library("zoo")
#' ALV.DE <- get.hist.quote(instrument = "ALV.DE", start = "2000-01-01",
#'                          end = "2014-12-31", quote = "Open",
#'                          provider = "yahoo", retclass = "zoo")
#' GetRates(ALC.DE)
GetRates.zoo <- function(prices) {
    # check for validity, arg.match()
    rates <- zoo(coredata(prices)[2:length(prices)] / 
                     coredata(prices)[1:(length(prices) - 1)] - 1,
                 time(prices)[1:(length(prices) - 1)])
    return(rates)
}

#' Calculate rates of return for given series of prices.
#'
#' Return the series of rates of return for given series of prices.
#' 
#' S3 generics, which dispatches for data.frames, zoo's and regular vector.
#' For data.frmae the first column will be treated as "date" column and will 
#' be excluded from computation.
#' 
#' @param prices data.frame, in which first column is date, and the rest columns
#'   are prices. By default first column is dates.
#' @return data.frame with respective rates of returns
#' @examples
#' library("tseries"); library("zoo")
#' ALV.DE <- get.hist.quote(instrument = "ALV.DE", start = "2000-01-01",
#'                          end = "2014-12-31", quote = "Open",
#'                          provider = "yahoo", retclass = "zoo")
#' CS.PA <- get.hist.quote(instrument = "CS.PA", start = "2000-01-01",
#'                          end = "2014-12-31", quote = "Open",
#'                          provider = "yahoo", retclass = "zoo")
#' prices <- CombineZoosToDataframe(ALV.DE, CS.PA)
#' GetRates(prices)
GetRates.data.frame <- function(prices) {
    rates <- cbind(prices[1:(nrow(prices) - 1), 1],
                   prices[2:nrow(prices), -1] / 
                       prices[1:(nrow(prices) - 1), -1] - 1)
    colnames(rates) <- colnames(prices)
    return(rates)
}

#' Calculate rates of return for given series of prices.
#'
#' Return the series of rates of return for given series of prices.
#' 
#' S3 generics, which dispatches for data.frames, zoo's and regular vector.
#' 
#' @param prices numeric vector, containing prices of the stock
#' @return numeric vector of rates of return
#' @examples
#' GetRates(1:10)

GetRates.numeric <- function(prices) {
    rates <- prices[2:length(prices)] / prices[1:(length(prices) - 1)] - 1
    return(rates)
}