#' @title Correlation
#' @description Function to compute the temporal correlation between the observed and predicted time series. 
#' @template templateMeasureParams
#' @param method Type of correlation applied. Options: \code{c("pearson","kendall","spearman")}.
#' @param deseason Default to \code{NULL}, and ignored. Only used if removal of the seasonal cycle is to be done.
#'  In this case, this is an integer number indicating the width, in days, of the window used for moving average computation
#'   of the reference daily climatology. This argument is passed to \code{\link{deseason.VALUE}} via \code{\link{wrapperFUN}}.
#' @return A float number corresponding to the correlation coefficient of choice between the predicted and observed series.
#' @author J. Bedia, D. San-Martin, S. Herrera
#' @export


measure.cor <- function(indexObs = NULL, indexPrd = NULL, obs, prd,
                        method = c("pearson", "kendall", "spearman"),
                        deseason = NULL) {
      if (length(obs) <= 1) {
            stop("Observed time series is needed")
      }
      if (length(prd) <= 1) {
            stop("Predicted time series is needed")
      }
      cor(obs, prd, use = "pairwise.complete.obs", method = method)
}
