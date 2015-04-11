# S3 methodology for dispathing functions
GetRates <- function(prices) UseMethod(generic = "GetRates", object = prices)

GetRates.zoo <- function(prices) {
    # Return series of rates of return for given prices
    # Args:
    #   prices: objects of class zoo, containing prices of the stock
    # Returns:
    #   object of class zoo, which comtains rates of return
    
    # check for validity, arg.match()
    rates <- zoo(coredata(prices)[2:length(prices)] / 
                     coredata(prices)[1:(length(prices) - 1)] - 1,
                 time(prices)[1:(length(prices) - 1)])
    return(rates)
}

GetRates.data.frame <- function(prices) {
    # Return series of rates of return for given prices
    # Args:
    #   prices: data.frame, in which first column is date, and the rest columns
    #           are prices. By default first column is dates.
    # Returns:
    #   data.frame with respective rates of returns
    
    # check for validity, arg.match()
    rates <- cbind(prices[1:(nrow(prices) - 1), 1],
                   prices[2:nrow(prices), -1] / 
                       prices[1:(nrow(prices) - 1), -1] - 1)
    colnames(rates) <- colnames(prices)
    return(rates)
}

GetRates.numeric <- function(prices) {
    # Return series of rates of return for given prices
    # Args:
    #   prices: numeric vector, containing prices of the stock
    # Returns:
    #   numeric vector of rates of return
    
    rates <- prices[2:length(prices)] / prices[1:(length(prices) - 1)] - 1
    return(rates)
}