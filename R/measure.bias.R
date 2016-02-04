#' @title Bias
#' @description Function to compute the difference between the observed and predicted means. 
#' The measure depends on the index mean.
#' @author Daniel San-Mart\'in \email{daniel@@predictia.es}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param obs Mean value for the observed series or a vector with the observed time series. 
#' @param prd Mean value for the predicted series or a vector with the predicted time series. 
#' @return A float number corresponding to the bias.
#' @export
#' @examples \dontrun{
#' # Bias Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' prd <- precipIberiaECA$predictions$Data[,1]
#' # Considering the time series:
#' measure.bias(obs,prd)
#' # Considering the index associated with the bias (mean):
#' obs <- index.mean(obs)
#' prd <- index.mean(prd)
#' measure.bias(obs,prd)
#' }


measure.bias <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL) {
      indexPrd - indexObs
}
