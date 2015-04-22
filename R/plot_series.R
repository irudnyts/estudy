#' Plot series with hatched weekends
#'
#' Plot series with hatched in gray weekends
#' 
#' \code{series} should be data.frame with column "date" 
#' 
#' @param series data.frame, with two columns, one is "date".
#' @param ... parameters, passed to plot(...)
#' @return NULL
#' @examples
#' PlotSeries(rates[, c(1, 2)])
PlotSeries <- function(series, ...) {
    plot(series, ...)
    for(i in 1:length(seq(from = as.Date(series[1, "date"]),
                          to = as.Date(series[nrow(series), "date"]),
                          by = 1))) {
        date <- as.Date(series[1, "date"]) + i
        if(weekdays(date) == "Friday") {
            rect(xleft = date, ybottom = par("usr")[3], xright = date + 3,
                 ytop = par("usr")[4], density = 20, angle = 45,
                 border = NULL, col = "gray")
        }
    }
}