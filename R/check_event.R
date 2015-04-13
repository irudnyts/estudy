#' Sum of vector elements.
#' 
#' \code{sum} returns the sum of all the values present in its arguments.
#' 
#' This is a generic function: methods can be defined for it directly or via the
#' \code{\link{Summary}} group generic. For this to work properly, the arguments
#' \code{...} should be unnamed, and dispatch is on the first argument.
GetNextDate <- function(date, set) {
    # Return the closest date in set, which is greater then argument date. 
    # Args:
    #   date: the date, for which the closest will be choosen
    #   set: the vector, which contains dates for search
    # Returns:
    #   the closest date from set
    
    # check arguments for validity
    stopifnot(inherits(date, "Date"), inherits(set, "Date"))
    set <- sort(set)
    if(set[length(set)] < date) {
        stop("set does not contain dates greater then date")
    }
    # perform the search
    i <- 1
    while(set[i] < date) {
        i <- i + 1
    }
    set[i]   
}



CheckEvent.df <- function(rates, index.ticker = character(), event.date,
                          w.b = numeric(),
                          w.a = numeric(),
                          delta = numeric()) {
    # Check the event date on significance for given market index and companies.
    #
    # Args:
    #   rates: data.frame containing daily prices for companies and index, as 
    #          well as column with dates and name "date"
    #   index.ticker: character, the ticker of the index, must be a column name
    #                 of data data.frame.
    #   event.date: Date, specifying event date, which should be tested.
    #   w.b: numeric, parameter of time frame (window), specifying number of
    #        days before event.
    #   w.b: numeric, parameter of time frame (window), specifying number of
    #        days after event.
    #   delta: numeric, length of estimation period.
    #
    # Returns:
    #   data.frame with marked dates and test statistics
    
    # check all arguments for validity
    
    if(length(which(colnames(rates) %in% index.ticker)) != 1) {
        stop("The data.frame retes does not contain such an index as
             index.ticker.")
    }
    if(length(which(rates[ , "date"] %in% event.date)) != 1) {
        warning("The data.frame rates does not contain date event.date or
                contain multiple dates. The event date to the closest following
                will be taken instead.")
        # browser()
        event.date <- GetNextDate(event.date, rates[ , "date"])
    }
    if(!is.numeric(w.b) || !is.numeric(w.a) || !is.numeric(delta)) {
        stop("One of parameters w.a, w.b, or delta is not numeric.")
    }
    if(which(rates[ , "date"] %in% event.date) - w.b - delta < 1) {
        stop("Estimation period is out of boundary of d.f. rates.")
    }
    if(which(rates[ , "date"] %in% event.date) + w.a > nrow(rates)) {
        stop("Event window period is out of boundary of d.f. rates.")
    }
    
    
    # row number of event.date
    t.e <- which(rates[ , "date"] %in% event.date) 
    # column numbers for index, date and companies columns 
    col.index <- which(colnames(rates) %in% index.ticker)
    col.date <- which(colnames(rates) %in% "date")
    col.companies <- (1:ncol(rates))[c(-col.index, -col.date)]
    
    # data.frame, which stores abnormals
    abnormals <-
        data.frame(date
                   = rates[(t.e - w.b - delta):(t.e + w.a), col.date],
                   stringsAsFactors = F)
    
    for(col.company in col.companies) {
        # for all companies perform OLS
        estimated.parameters <- lm(
            rates[(t.e - w.b - delta):(t.e - w.b - 1), col.company] ~
                rates[(t.e - w.b - delta):(t.e - w.b - 1), col.index])
        company.abnormal <- data.frame(
            rates[(t.e - w.b - delta):(t.e + w.a), col.company] - 
                estimated.parameters$coefficients[[1]] - 
                estimated.parameters$coefficients[[2]] * 
                rates[(t.e - w.b - delta):(t.e + w.a), col.index])
        # actually we do not need names in abnormals, so we can avoid creating
        # new variable and directly use cbind
        colnames(company.abnormal) <- colnames(rates)[col.company]
        abnormals <- cbind(abnormals, company.abnormal)
    }
    # calculate means for all dates
    # can through an error (if in rowMeans # of columns == 1)
    if(length(col.companies) > 1) {
        abnormals.means <- rowMeans(abnormals[, -1])
    } else if(length(col.companies) == 1) {
        abnormals.means <- abnormals[, -1]
    }
    # calculate var
    abnormals.sd <- sqrt(var(abnormals.means[1:delta]))
    # nice result data.frame
    result <- data.frame(date =
                             abnormals[(delta + 1):(delta + w.a + w.b + 1), 1],
                         statistics = numeric(w.a + w.b + 1),
                         significance = character(w.a + w.b + 1),
                         perc.negative = numeric(w.a + w.b + 1),
                         stringsAsFactors = F)
    for(i in 1:nrow(result)) {
        statistics <- abnormals.means[delta + i] / abnormals.sd # extra variable
        # for fast code
        result[i, 2] <- statistics
        result[i, 3] <- if(abs(statistics) > qnorm(1 - 0.01/2)) { 
            "***"
        } else if(abs(statistics) > qnorm(1 - 0.05/2)) {
            "**"
        } else if(abs(statistics) > qnorm(1 - 0.10/2)) {
            "*"  
        } else {
            ""
        }
        result[i, 4] <- (sum(abnormals[delta + i, -1] < 0) /
                             (ncol(abnormals) - 1)) * 100
        
    }
    return(result)
    }

CheckEvent.zoo <- function(..., index, event.date, w.b = numeric(),
                           w.a = numeric(), delta = numeric()) {
    # Check the event date on significance for given market index and companies.
    # Well-combined with packages tseries and yahoo finance data.
    # Note: zoo package should be loaded.
    #
    # Args:
    #   ...: objects of class zoo, containing rates of numerous companies. 
    #   index: object of class zoo, containing index rates.
    #   event.date: Date, specifying event date, which should be tested.
    #   w.b: numeric, parameter of time frame (window), specifying number of
    #        days before event.
    #   w.b: numeric, parameter of time frame (window), specifying number of
    #        days after event.
    #   delta: numeric, length of estimation period.
    #
    # Returns:
    #   data.frame with marked dates and test statistics
    
    # require("zoo") should be avoided in package
    
    # merging all zoo objects
    rates.zoo <- merge.zoo(..., index, all = T)
    # creat data.frame, which will be passed to CheckEvent.df
    rates <- cbind(time(rates.zoo), as.data.frame(rates.zoo))
    rates <- data.frame(rates, row.names = NULL)
    colnames(rates)[1] <- "date"
    CleanSeries.data.frame(rates)
    
    CheckEvent.df(rates = rates[complete.cases(rates) ,],
                  index.ticker = colnames(rates)[length(rates)],
                  event.date = event.date, w.b = w.b, w.a = w.a, delta = delta)
}