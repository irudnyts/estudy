#' Return clothest to \code{date} date in the \code{set}
#'
#' Return the closest date in \code{set}, which is greater then argument date.
#' 
#' The \code{date} should not be greater then the largest date in \code{set}. 
#' 
#' @param date the single-value vector of class Date, for which the closest will
#'     be choosen
#' @param set the vector of the class Date, which contains dates for search 
#' @return the closest date from \code{set}
#' @examples
#' GetNextDate(as.Date("01.05.2005","%d.%m.%Y"), 
#'     c(as.Date("01.01.2005","%d.%m.%Y"), as.Date("01.08.2005","%d.%m.%Y"), 
#'     as.Date("01.03.2005","%d.%m.%Y")))
GetNextDate <- function(date, set) {
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

#' Check the event date of significance.
#'
#' Check the event date on significance for given market index and companies.
#' 
#' Data.frame rates should contain the column with exact name \code{date}, which 
#' sould contain the dates, as well as column with name \code{index.ticker}
#' 
#' @param rates data.frame, which contains daily rates of return for companies
#'   and index, as well as column with dates and name "date"
#' @param index.ticker character, the ticker of the index, must be a column name
#'   of data data.frame.
#' @param event.date date, specifying event date, which should be tested.
#' @param w.b numeric, parameter of time frame (window), specifying number of 
#'   days before event.
#' @param w.a numeric, parameter of time frame (window), specifying number of
#'   days after event.
#' @param delta numeric, length of estimation period.
#' @return data.frame with marked dates and test statistics
#' @examples
#' data(rates)
#' CheckEvent.df(rates = rates, index.ticker = "SXW1E", 
#'               event.date = as.Date("11.09.2001","%d.%m.%Y"), w.b = 10,
#'               w.a = 10, delta = 110)
CheckEvent.df <- function(rates, index.ticker = character(), event.date,
                          w.b = numeric(),
                          w.a = numeric(),
                          delta = numeric()) {
    # check all arguments for validity    
    if(length(which(colnames(rates) %in% index.ticker)) != 1) {
        stop("The data.frame retes does not contain such an index as
             index.ticker.")
    }
    if(length(which(rates[ , "date"] %in% event.date)) != 1) {
        warning("The data.frame rates does not contain date event.date or
                contain multiple dates. The event date to the closest following
                will be taken instead.")
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
    # number of companies, without date and index column
    N <- ncol(rates) - 2
    N.sqrt <- sqrt(N)
    
    # data.frame, which stores abnormals
    abnormals <-
        data.frame(date
                   = rates[(t.e - w.b - delta):(t.e + w.a), col.date],
                   stringsAsFactors = F)
    ####
    series <- data.frame(date
                         = rates[(t.e - w.b - delta):(t.e + w.a), col.date],
                         stringsAsFactors = F)
    ####
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
        ####
        company.predicter <- data.frame(estimated.parameters$coefficients[[1]] + 
            estimated.parameters$coefficients[[2]] * 
            rates[(t.e - w.b - delta):(t.e + w.a), col.index])
        company.observed <- data.frame(
            rates[(t.e - w.b - delta):(t.e + w.a), col.company])
        company.all <- cbind(company.predicter, company.observed)
        colnames(company.all) <- c(paste(colnames(rates)[col.company], ".pred",
                                         sep = ""),
                                   paste(colnames(rates)[col.company], ".obs",
                                         sep = ""))
        series <- cbind(series, company.all)        
        #####
        
        
        
        
        
        
        # actually we do not need names in abnormals, so we can avoid creating
        # new variable and directly use cbind
        colnames(company.abnormal) <- colnames(rates)[col.company]
        abnormals <- cbind(abnormals, company.abnormal)
    }
    # calculate means for all dates
    # can through an error (if in rowMeans # of columns == 1)
    if(length(col.companies) > 1) {
        abnormals.means <- rowMeans(abnormals[, -1])
        abnormals.sd <- sqrt(apply(abnormals[, -1], 1, var))
    } else if(length(col.companies) == 1) {
        abnormals.means <- abnormals[, -1]
    }
    
    # calculate var
    abnormals.crude.sd <- sqrt(var(abnormals.means[1:delta]))
    # nice result data.frame
    browser()
    result <- data.frame(date =
                             abnormals[(delta + 1):(delta + w.a + w.b + 1), 1],
                         w.day = 
                             weekdays(abnormals[(delta + 1):
                                                (delta + w.a + w.b + 1), 1]),
                         t.stat = numeric(w.a + w.b + 1),
                         t.signif = character(w.a + w.b + 1),
                         c.stat = numeric(w.a + w.b + 1),
                         c.signif = character(w.a + w.b + 1),
                         perc.negative = numeric(w.a + w.b + 1),
                         stringsAsFactors = F)
    qnorm.001 <- qnorm(1 - 0.01/2)
    qnorm.005 <- qnorm(1 - 0.05/2)
    qnorm.01 <- qnorm(1 - 0.10/2)
    qt.001 <- qt(1 - 0.01/2, df = N - 1)
    qt.005 <- qt(1 - 0.05/2, df = N - 1)
    qt.01 <- qt(1 - 0.1/2, df = N - 1)
    for(i in 1:nrow(result)) {
        t.stat <- abnormals.means[delta + i] / abnormals.sd[delta + i] * N.sqrt
        c.stat <- abnormals.means[delta + i] / abnormals.crude.sd
        t.stat.abs <- abs(t.stat)
        c.stat.abs <- abs(c.stat)
        result[i, 2] <- t.stat
        result[i, 3] <- if(t.stat.abs > qt.001) { 
            "***"
        } else if(t.stat.abs > qt.005) {
            "**"
        } else if(t.stat.abs > qt.01) {
            "*"  
        } else {
            ""
        }
        result[i, 4] <- c.stat
        result[i, 5] <- if(c.stat.abs > qnorm.001) { 
            "***"
        } else if(c.stat.abs > qnorm.005) {
            "**"
        } else if(c.stat.abs > qnorm.01) {
            "*"  
        } else {
            ""
        }
        result[i, 6] <- (sum(abnormals[delta + i, -1] < 0) /
                             (ncol(abnormals) - 1)) * 100
        
    }
    return(list(abnormals, abnormals.means, result, series))
}

#' Check the event date of significance.
#'
#' Check the event date on significance for given market index and companies.
#' 
#' Well-combined with packages tseries and yahoo finance data. Note: zoo package
#' should be loaded.
#' 
#' @param ... objects of class zoo, containing daily rates of return of numerous
#'   companies.
#' @param index object of class zoo, containing index rates.
#' @param event.date date, specifying event date, which should be tested.
#' @param w.b numeric, parameter of time frame (window), specifying number of 
#'   days before event.
#' @param w.a numeric, parameter of time frame (window), specifying number of
#'   days after event.
#' @param delta numeric, length of estimation period.
#' @return data.frame with marked dates and test statistics
#' @examples
#' library("tseries"); library("zoo")
#' date(rates)
#' ALV.DE <- get.hist.quote(instrument = "ALV.DE", start = "2000-01-01",
#'                          end = "2014-12-31", quote = "Open",
#'                          provider = "yahoo", retclass = "zoo")
#' CS.PA <- get.hist.quote(instrument = "CS.PA", start = "2000-01-01",
#'                          end = "2014-12-31", quote = "Open",
#'                          provider = "yahoo", retclass = "zoo")
#' CheckEvent.zoo(ALV.DE, CS.PA, index = zoo(rates[, "SXW1E"], rates[, "date"]), 
#'               event.date = as.Date("11.09.2001","%d.%m.%Y"), w.b = 10,
#'               w.a = 10, delta = 110)
CheckEvent.zoo <- function(..., index, event.date, w.b = numeric(),
                           w.a = numeric(), delta = numeric()) {    
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