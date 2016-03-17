#' @title Amount from events above threshold
#' @description Function to compute the amount from events above a predefined threshold.
#' @author Neyko Neykov, J. Bedia, D. San-Martin, S. Herrera
#' @param ts A vector containing the input time series
#' @param threshold A float number defining the threshold considered. Default to 1.
#' @return A float number corresponding to the amount fallen the days above the input threshold.
#' @export

index.amountFreqGT <- function(ts, threshold = 1) {
      sum(ts[ts > threshold], na.rm = TRUE)
}
