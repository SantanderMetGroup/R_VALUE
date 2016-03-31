#' @title Return Value lower tail
#' @description Function to compute the 1/prob return value (left tail). 
#' @author Neyko Neykov, S. Herrera, J. Bedia
#' @param ts  A vector containing the data
#' @param prob  Return period considered. Default to 20.
#' @param annual.index Logical. If set to TRUE, a vector defining the years for aggregation is passed by \code{\link{wrapperFUN}}.
#' Otherwise no temporal aggregation of the input time series is performed.
#' @return Return value for the lower tail
#' @export
#' @importFrom evd fgev


index.rv.lb <- function(ts, prob = 20, annual.index = TRUE) {
      meanObj <- NA
      max.x <- tapply(ts, INDEX = annual.index, FUN = max, na.rm = TRUE)
      if (any(is.finite(max.x))) {
            estim <- fgev(max.x[which(is.finite(max.x))], prob = 1 - 1/prob, std.err = FALSE)
            meanObj <- estim$param[1]
      }
      return(meanObj)
}
