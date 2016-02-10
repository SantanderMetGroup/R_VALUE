#' @title Bias
#' @description Function to compute the difference between the observed and predicted means. 
#' The measure depends on the index mean.
#' @author Daniel San-Mart\'in \email{daniel@@predictia.es}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @template templateMeasureParams
#' @return A float number corresponding to the bias.
#' @export

measure.bias <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL) {
      indexPrd - indexObs
}