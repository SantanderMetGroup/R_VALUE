#' @title Return Value
#' @description Function to compute the 1/prob return value (left/right tail). 
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts  A vector containing the data
#' @param prob  Return period considered
#' @param INDEX A Vector defining the aggregation periods (passed to \code{tapply}). No aggregation is performed by default.
#' @return Return values for the upper/lower tails
#' @export
#' @importFrom evd fgev
#' @examples \dontrun{
#' # 20-years Return value of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' index.rv(obs, prob = 20)
#' }

index.rv <- function(ts, prob, INDEX = 1:length(ts)) {
      meanObj <- c(NA, NA)
      max.x <- tapply(ts, INDEX = INDEX, FUN = max, na.rm = TRUE)
      if (any(is.finite(max.x))) {
            estim <- fgev(max.x[which(is.finite(max.x))], prob = 1/prob, std.err = FALSE)
            meanObj1 <- estim$param[1]
            estim <- fgev(max.x[which(is.finite(max.x))], prob = 1 - 1/prob, std.err = FALSE)
            meanObj2 <- estim$param[1]
            meanObj <- c(meanObj1,meanObj2)
      }
      return(meanObj)
}

