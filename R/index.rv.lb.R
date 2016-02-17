#' @title Return Value lower tail
#' @description Function to compute the 1/prob return value (left tail). 
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts  A vector containing the data
#' @param prob  Return period considered. Default to 20.
#' @param INDEX A Vector defining the aggregation periods (passed to \code{tapply}). No aggregation is performed by default.
#' @return Return value for the lower tail
#' @export
#' @importFrom evd fgev
#' @examples \dontrun{
#' # 20-years Return value of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' index.rv.lb(obs, prob = 20)
#' }

index.rv.lb <- function(ts, prob = 20, INDEX = 1:length(ts)) {
      meanObj <- NA
      max.x <- tapply(ts, INDEX = INDEX, FUN = max, na.rm = TRUE)
      if (any(is.finite(max.x))) {
            estim <- fgev(max.x[which(is.finite(max.x))], prob = 1 - 1/prob, std.err = FALSE)
            meanObj <- estim$param[1]
      }
      return(meanObj)
}
