#' @title Mean absolute error
#' @description Function to compute the mean absolute difference between the observed and predicted time series.
#' @author Daniel San-Mart\'in \email{daniel@@predictia.es}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param obs A vector of observations
#' @param prd A vector of predictions 
#' @return A float number corresponding to the mean absolute difference between the predicted and observed series.
#' @export
#' @examples \dontrun{
#' # Mean absolute error of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' prd <- precipIberiaECA$predictions$Data[,1]
#' measure.mae(obs,prd)
#' }

measure.mae <- function(obs, prd){
      if (length(obs) <= 1) {
            stop("Observed time series is needed")
      }
      if (length(prd) <= 1) {
            stop("Predicted time series is needed")
      }
      index.mean(abs(prd - obs))
}
