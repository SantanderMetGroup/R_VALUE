#' @title Sample quantiles
#' @description Function to compute the sample quantiles for the given probabilities. 
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg}, J. Bedia, D. San-Mart\'in, S. Herrera
#' 
#' @param ts A vector containing the data
#' @param prob A float number in the range [0,1] defining the probability of the quantile to be calculated.
#' @param threshold Optional parameter. Remove data below the threshold value.
#'                  Included to consider the case of precipitation. NULL by default.
#' @return A float number corresponding to the quantile.
#' @export
#' @examples \dontrun{
#' # 98th quantile of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' # 98th percentile
#' prob <- 0.98
#' # Default 
#' index.prcXXth(obs)
#' }

index.prcXXth <- function(ts, prob = seq(0, 1, 0.25), threshold = NULL) {
      if (!is.null(threshold)) ts[ts < threshold] <- NA
      quantile(ts, probs = prob, type = 7, na.rm = TRUE)
}
