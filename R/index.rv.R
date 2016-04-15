#' @title Return Value 
#' @description Function to compute return values
#' @author Neyko Neykov, S. Herrera, J. Bedia
#' @param ts  A vector containing the data
#' @param prob  Return period considered (in years). Default to 20.
#' @param annual.index Logical. If set to TRUE, a vector defining the years for aggregation is passed by \code{\link{wrapperFUN}}.
#' Otherwise no temporal aggregation of the input time series is performed.
#' @param tail A character indicating wheter GEV is being fit for maxima (\code{tail="right"}) or minima (\code{tail="left"}).
#' @return Return value 
#' @export
#' @importFrom evd fgev


index.rv <- function(ts, prob = 20, annual.index = TRUE, tail) {
      tail <- match.arg(tail, c("right", "left"))
      meanObj <- NA
      if (tail == "right") {
            fun <- "max"
            prob <- 1/prob
      } else {
            fun <- "min"
            prob <- 1 - 1/prob
      }
      x <- tapply(ts, INDEX = annual.index, FUN = fun, na.rm = TRUE)
      if (any(is.finite(x))) {
            estim <- fgev(x[which(is.finite(x))], prob = prob, std.err = FALSE)
            meanObj <- estim$param[1]
      }
      return(meanObj)
}
