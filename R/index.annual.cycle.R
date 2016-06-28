#' @title Annual cycle statistics
#' @description Computes several annual cycle statistics
#' @template templateIndexParams
#' @template templateDates
#' @param type Character string indicating the statistic. Currently implemented options are \code{"min"},
#' \code{"max"}, \code{"amp"} for amplitude, \code{"relamp"} for relative amplitude (in \%),
#' \code{"phase"} for amplitude of the filtered time series and \code{"asymmetry"} to calculate the
#' difference in days between maximum and minimum of the filtered cycle (see Details for the last two types).
#' @details \strong{Time series filter}
#' 
#' The \code{type = "phase"} and \code{"asymmetry"} options apply a circular moving average, with a window width of 31 days
#'  and centered on lag-0, and then return the day of the year (julian day [1-366]) reaching either the maximum or the number 
#' of days between maximum and minimum respectively.
#' 
#' @return A float number with the corresponding statistics.
#' @author Sven Kotlarski, S. Herrera, J. Bedia, D. San Martin, D. Maraun
#' @export
#' @importFrom stats filter

index.annual.cycle <- function(ts, dates, type = c("phase","min", "max", "amp", "relamp", "asymmetry")) {
      type <- match.arg(type, choices = c("phase", "min", "max", "amp", "relamp", "asymmetry"))
      doy <- substr(dates, 6, 10)
      ref <- tapply(ts, INDEX = doy, FUN = mean, na.rm = TRUE)
      if (length(ref) >= 31) {
            if (type == "phase" | type == "asymmetry") {
                  ref <- filter(ref, 1/31 * rep(1, 31), method = "convolution", sides = 2, circular = TRUE)
                  if (type == "asymmetry") nmonth <- length(unique(substr(dates,6,7)))
            }
            out <- switch(type,
                   "min" = min(ref, na.rm = TRUE),
                   "max" = max(ref, na.rm = TRUE),
                   "amp" = max(ref, na.rm = TRUE) - min(ref, na.rm = TRUE),
                   "relamp" = (max(ref, na.rm = TRUE) - min(ref, na.rm = TRUE)) * 100 / mean(ref, na.rm = TRUE),
                   "phase" = which.max(ref),
                   "asymmetry" = ((abs(which.max(ref) - which.min(ref)) * -1) %% 365) / nmonth
            )
      } else {
            out <- NA
      }
      return(out)
}


      


