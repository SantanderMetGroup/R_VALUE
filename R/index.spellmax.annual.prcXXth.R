#' @title Annual spell length percentiles
#' @description Computes a user-defined percentile for the duration of spells (above/below) a given threshold, on an annual basis.
#' @template templateIndexParams
#' @template templateDates
#' @param threshold A float number defining the threshold considered. Default to 1 
#' @param condition Inequality operator to be applied considering the given threshold.
#'  \code{"GT"} = greater than the value of \code{threshold}, \code{"GE"} = greater or equal,
#'   \code{"LT"} = lower than, \code{"LE"} = lower or equal than
#' @param prob A float number in the range [0,1] defining the probability of the quantile to be calculated.
#'  Default to median (\code{prob = 0.5}).
#' @return A float number with the corresponding percentile.
#' @author J. Bedia, J.M. Gutierrez
#' @details The function requires the date information in order to ensure that spells
#'  are computed on consecutive records.
#' @export

index.spellmax.annual.prcXXth <- function(ts, dates, threshold = 1, condition = c("GT", "GE", "LT", "LE"), prob = .5) {
      condition <- match.arg(condition, choices = c("GT", "GE", "LT", "LE"))
      ineq <- switch(condition,
                     "GT" = ">",
                     "GE" = ">=",
                     "LT" = "<",
                     "LE" = "<=")
      # date format yyyy-mm-dd hh:mm:ss is assumed
      # this speeds up the POSIXlt function which uses the slow strptime function
      # providing a format and timezone to POSIXlt helps but this approach is faster
      yrs <- substr(dates,1,4)
      dates <- as.Date(dates)
      annual.values <- sapply(1:length(unique(yrs)), function(x) {
            ind <- which(yrs == unique(yrs)[x]) 
            disc <- diff(dates[ind])
            aux <- ts[ind]
            ind2 <- if (any(disc > 1)) {# Index of discontinuities in the data series
                  c(0, which(disc > 1))
            } else {
                  c(0, length(aux))
            }
            spell.list <- lapply(2:length(ind2), function(y) {
                  aux <- ts[(ind2[y - 1] + 1):ind2[y]]
                  rle.obj <- eval(parse(text = paste("rle(aux", ineq, "threshold)")))
                  rle.obj$lengths[rle.obj$values == TRUE]
            })
            max(do.call("c", spell.list), na.rm = TRUE)
      })
      quantile(annual.values, probs = prob, type = 7, na.rm = TRUE)
}
