#' @title Correlation
#' @description Function to compute the temporal correlation between the observed and predicted time series. 
#' @template templateMeasureParams
#' @param method Type of correlation applied. Options: \code{c("pearson","kendall","spearman")}.
#' @return A float number corresponding to the correlation coefficient of choice between the predicted and observed series.
#' @author J. Bedia, D. San-Martin, S. Herrera
#' @export


measure.cor <- function(indexObs = NULL, indexPrd = NULL, obs, prd,
                        method = c("pearson", "kendall", "spearman")) {
      if (length(obs) <= 1) {
            stop("Observed time series is needed")
      }
      if (length(prd) <= 1) {
            stop("Predicted time series is needed")
      }
      cor(obs, prd, use = "pairwise.complete.obs", method = method)
}
