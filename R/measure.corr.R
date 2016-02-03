#' @title Correlation
#' @description Function to compute the temporal correlation between the observed and predicted time series. 
#' @author Daniel San-Mart\'in \email{daniel@@predictia.es}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param obs Vector with the observed time series. 
#' @param prd Vector with the predicted time series. 
#' @param method Type of correlation applied. Options: "pearson", "kendall" and "spearman".
#' @param ... optional parameters
#' @return A float number corresponding to the correlation coefficient of choice between the predicted and observed series.
#' @export
#' @examples \dontrun{
#' # Correlation Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' prd <- precipIberiaECA$predictions$Data[,1]
#' measure.cor(obs, prd, method = "spearman")
#' }

measure.cor <- function(obs, prd, method = c("pearson", "kendall", "spearman")) {
      if (length(obs) <= 1) {
            stop("Observed time series is needed")
      }
      if (length(prd) <= 1) {
            stop("Predicted time series is needed")
      }
      cor(obs, prd, use = "pairwise.complete.obs", method = method)
}
