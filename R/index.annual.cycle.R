#' @title Annual cycle statistics
#' @description Computes several annual cycle statistics
#' @template templateIndexParams
#' @template templateDates
#' @param type Character string indicating the statistic. Currently implemented options are \code{"min"},
#' \code{"max"}, \code{"amp"} for amplitude, \code{"relamp"} for relative amplitude (in \%),
#' \code{"phase"} for amplitude of the filtered time series and \code{"asymmetry"} to calculate the
#' difference in days between maximum and minimum of the filtered cycle (see Details for the last two types).
#' @param window.width Integer number indicating the width, in days, of the window used for
#'  moving average computation of the reference daily climatology. Default to 61 days. See details.
#' 
#' @details \strong{Time series filter}
#' 
#' A (circular) moving average daily climatology is calculated for each data series, considering 
#' a user-defined window width centered around lag 0.
#' 
#' @return A float number with the corresponding statistics.
#' @author Sven Kotlarski, S. Herrera, J. Bedia, D. San Martin, D. Maraun
#' @export
#' @importFrom stats filter

index.annual.cycle <- function(ts,
                               dates,
                               type = c("phase","min", "max", "amp", "relamp", "asymmetry"),
                               window.width = 61) {
    type <- match.arg(type, choices = c("phase", "min", "max", "amp", "relamp", "asymmetry"))
    doy <- substr(dates, 6, 10)
    ref <- tapply(ts, INDEX = doy, FUN = mean, na.rm = TRUE)
    if (length(ref) >= window.width) {
        ref <- filter(ref, 1/window.width * rep(1, window.width), method = "convolution", sides = 2, circular = TRUE)
        out <- switch(type,
                      "min" = min(ref, na.rm = TRUE),
                      "max" = max(ref, na.rm = TRUE),
                      "amp" = max(ref, na.rm = TRUE) - min(ref, na.rm = TRUE),
                      "relamp" = (max(ref, na.rm = TRUE) - min(ref, na.rm = TRUE)) * 100 / mean(ref, na.rm = TRUE),
                      "phase" = which.max(ref),
                      "asymmetry" = ((which.max(ref) - which.min(ref)) %% 365) / 365
                      )
    } else {
        out <- NA
    }
    return(out)
}


      


