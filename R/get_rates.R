# GetRatesOpenPrices family ---------------------------------------------------- 

# S3 methodology for dispathing function GetRatesOpenPrices
GetRatesOpenPrices <- function(prices) UseMethod(generic = "GetRatesOpenPrices",
                                                 object = prices)

#' Calculate the rates of return for given series of open prices.
#'
#' Return the series of the rates of return for given series of open prices.
#' 
#' S3 generics, which dispatches for data.frames, zoo's and regular vector.
#' 
#' @param prices objects of class zoo, containing open prices of the stock
#' @return object of class zoo, which comtains the rates of return
#' @examples
#' library("tseries"); library("zoo")
#' ALV.DE <- get.hist.quote(instrument = "ALV.DE", start = "2000-01-01",
#'                          end = "2014-12-31", quote = "Open",
#'                          provider = "yahoo", retclass = "zoo")
#' GetRatesOpenPrices(ALC.DE)
GetRatesOpenPrices.zoo <- function(prices) {
    # check for validity?
    rates <- zoo(coredata(prices)[2:length(prices)] / 
                     coredata(prices)[1:(length(prices) - 1)] - 1,
                 time(prices)[1:(length(prices) - 1)])
    return(rates)
}

#' Calculate the rates of return for given series of open prices.
#'
#' Return the series of the rates of return for given series of open prices.
#' 
#' S3 generics, which dispatches for data.frames, zoo's and regular vector.
#' For data.frmae the first column will be treated as "date" column and will 
#' be excluded from computation.
#' 
#' @param prices data.frame, in which first column is date, and the rest columns
#'   are open prices. By default first column is dates.
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
#' GetRatesOpenPrices(prices)
GetRatesOpenPrices.data.frame <- function(prices) {
    rates <- cbind(prices[1:(nrow(prices) - 1), 1],
                   prices[2:nrow(prices), -1] / 
                       prices[1:(nrow(prices) - 1), -1] - 1)
    colnames(rates) <- colnames(prices)
    return(rates)
}

# GetRatesClosePrices family --------------------------------------------------- 

# S3 methodology for dispathing function GetRatesClosePrices
GetRatesClosePrices <- function(prices) UseMethod(
                                                generic = "GetRatesClosePrices",
                                                object = prices)

#' Calculate the rates of return for given series of close prices.
#'
#' Return the series of the rates of return for given series of close prices.
#' 
#' S3 generics, which dispatches for data.frames, zoo's and regular vector.
#' 
#' @param prices objects of class zoo, containing close prices of the stock
#' @return object of class zoo, which comtains the rates of return
#' @examples
#' library("tseries"); library("zoo")
#' ALV.DE <- get.hist.quote(instrument = "ALV.DE", start = "2000-01-01",
#'                          end = "2014-12-31", quote = "Close",
#'                          provider = "yahoo", retclass = "zoo")
#' GetRatesClosePrices(ALC.DE)
GetRatesClosePrices.zoo <- function(prices) {
    # check for validity?
    rates <- zoo(coredata(prices)[2:length(prices)] / 
                     coredata(prices)[1:(length(prices) - 1)] - 1,
                 time(prices)[2:length(prices)])
    return(rates)
}

#' Calculate the rates of return for given series of close prices.
#'
#' Return the series of the rates of return for given series of close prices.
#' 
#' S3 generics, which dispatches for data.frames, zoo's and regular vector.
#' For data.frmae the first column will be treated as "date" column and will 
#' be excluded from computation.
#' 
#' @param prices data.frame, in which first column is date, and the rest columns
#'   are close prices. By default first column is dates.
#' @return data.frame with respective rates of returns
#' @examples
#' library("tseries"); library("zoo")
#' ALV.DE <- get.hist.quote(instrument = "ALV.DE", start = "2000-01-01",
#'                          end = "2014-12-31", quote = "Close",
#'                          provider = "yahoo", retclass = "zoo")
#' CS.PA <- get.hist.quote(instrument = "CS.PA", start = "2000-01-01",
#'                          end = "2014-12-31", quote = "Close",
#'                          provider = "yahoo", retclass = "zoo")
#' prices <- CombineZoosToDataframe(ALV.DE, CS.PA)
#' GetRatesClosePrices(prices)
GetRatesClosePrices.data.frame <- function(prices) {
    rates <- cbind(prices[2:nrow(prices), 1],
                   prices[2:nrow(prices), -1] / 
                       prices[1:(nrow(prices) - 1), -1] - 1)
    colnames(rates) <- colnames(prices)
    return(rates)
}


# GetRates for vectors ---------------------------------------------------------

#' Calculate the rates of return for vector of prices.
#'
#' Return the vector of the rates of return of given prices' vector.
#' 
#' @param prices numeric vector, containing prices of the stock
#' @return numeric vector of the rates of return
#' @examples
#' GetRates(1:10)
GetRates <- function(prices) {
    rates <- prices[2:length(prices)] / prices[1:(length(prices) - 1)] - 1
    return(rates)
}