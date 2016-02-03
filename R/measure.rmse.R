#' @title Root mean square error
#' @description Function to compute the root mean square error between the observed and predicted time series.
#' @author Daniel San-Mart\'in \email{daniel@@predictia.es}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param obs A vector of observations
#' @param prd A vector of predictions 
#' @return A float number corresponding to the root mean square error between the predicted and observed series.
#' @export
#' @examples \dontrun{
#' # Root mean square error of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' prd <- precipIberiaECA$predictions$Data[,1]
#' measure.rmse(obs, prd)
#' }

measure.rmse <- function(obs, prd) {
      if (length(obs) <= 1) {
            stop("Observed time series is needed")
      }
      if (length(prd) <= 1) {
            stop("Predicted time series is needed")
      }
      sqrt(index.mean((prd - obs) ** 2))
}
