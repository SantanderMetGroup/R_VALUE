#' @title Annual cycle statistics
#' @description Computes several annual cycle statistics
#' @template templateIndexParams
#' @template templateDates
#' @param type Character string indicating the statistic. Currently implemented options are \code{"min"},
#' \code{"max"}, \code{"amp"} for amplitude, and \code{"relamp"} for relative amplitude (in \%).
#' @return A float number with the corresponding statistis.
#' @author J. Bedia
#' @export

index.annual.cycle <- function(ts, dates, type = c("min", "max", "amp", "relamp")) {
      type <- match.arg(type, choices = c("min", "max", "amp", "relamp"))
      yrs <- as.POSIXlt(dates)$year
      switch(type, 
             "min" = mean(tapply(ts, yrs, min, na.rm = TRUE)),
             "max" = mean(tapply(ts, yrs, max, na.rm = TRUE)),
             "amp" = mean(tapply(ts, yrs, max, na.rm = TRUE)) - mean(tapply(ts, yrs, min, na.rm = TRUE)),
             "relamp" = (mean(tapply(ts, yrs, max, na.rm = TRUE)) - mean(tapply(ts, yrs, min, na.rm = TRUE)))*100 / mean(tapply(ts, yrs, mean, na.rm = TRUE))
      )
}
