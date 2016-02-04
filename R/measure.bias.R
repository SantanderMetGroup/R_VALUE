#' @title Bias
#' @description Function to compute the difference between the observed and predicted means. 
#' The measure depends on the index mean.
#' @author Daniel San-Mart\'in \email{daniel@@predictia.es}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param indexObs index computed from the observations
#' @param indexPrd index computed from the predictions
#' @param obs Mean value for the observed series or a vector with the observed time series. 
#' @param prd Mean value for the predicted series or a vector with the predicted time series. 
#' @return A float number corresponding to the bias.
#' @export

measure.bias <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL) {
      indexPrd - indexObs
}
