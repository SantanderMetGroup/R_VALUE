#' @title Return Value upper tail
#' @description Function to compute the 1/prob return value (right tail). 
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts  A vector containing the data
#' @param prob  Return period considered. Default to 20.
#' @param INDEX A Vector defining the aggregation periods (passed to \code{tapply}). No aggregation is performed by default.
#' @return Return value for the upper tail
#' @export
#' @importFrom evd fgev


index.rv.ub <- function(ts, prob = 20, INDEX = 1:length(ts)) {
      meanObj <- NA
      max.x <- tapply(ts, INDEX = INDEX, FUN = max, na.rm = TRUE)
      if (any(is.finite(max.x))) {
            estim <- fgev(max.x[which(is.finite(max.x))], prob = 1/prob, std.err = FALSE)
            meanObj <- estim$param[1]
      }
      return(meanObj)
}

