#' @title Correlation
#' @description Function to compute the temporal correlation between the observed and predicted time series. 
#' @author Daniel San-Mart\'in \email{daniel@@predictia.es}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param indexObs index computed from the observations
#' @param indexPrd index computed from the predictions
#' @param obs Mean value for the observed series or a vector with the observed time series. 
#' @param prd Mean value for the predicted series or a vector with the predicted time series. 
#' @param method Type of correlation applied. Options: "pearson", "kendall" and "spearman".
#' @return A float number corresponding to the correlation coefficient of choice between the predicted and observed series.
#' @export


measure.cor <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, method = c("pearson", "kendall", "spearman")) {
      if (length(obs) <= 1) {
            stop("Observed time series is needed")
      }
      if (length(prd) <= 1) {
            stop("Predicted time series is needed")
      }
      cor(obs, prd, use = "pairwise.complete.obs", method = method)
}
