#' @title Annual cycle statistics
#' @description Computes several annual cycle statistics
#' @template templateIndexParams
#' @template templateDates
#' @param type Character string indicating the statistic. Currently implemented options are \code{"min"},
#' \code{"max"}, \code{"amp"} for amplitude, and \code{"relamp"} for relative amplitude (in \%).
#' @return A float number with the corresponding statistis.
#' @author Sven Kotlarski, S. Herrera, J. Bedia, D. San Martin
#' @export

index.annual.cycle <- function(ts, dates, type = c("min", "max", "amp", "relamp")) {
      type <- match.arg(type, choices = c("min", "max", "amp", "relamp"))
      # date format yyyy-mm-dd hh:mm:ss is assumed
      # this speeds up the POSIXlt function which uses the slow strptime function
      # providing a format and timezone to POSIXlt helps but this approach is faster
      doy <- substr(dates,6,10)
      ref <- tapply(ts, INDEX = doy, FUN = mean, na.rm = TRUE)
      switch(type, 
             "min" = min(ref, na.rm = TRUE),
             "max" = max(ref, na.rm = TRUE),
             "amp" = max(ref, na.rm = TRUE) - min(ref, na.rm = TRUE),
             "relamp" = (max(ref, na.rm = TRUE) - min(ref, na.rm = TRUE))*100 / mean(ref, na.rm = TRUE)
      )
}
