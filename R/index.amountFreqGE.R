#' @title Amount from events above threshold
#' @description Function to compute the amount from events above or equal a predefined threshold.
#' @author Neyko Neykov, J. Bedia, D. San-Martin, S. Herrera
#' @param ts A vector containing the input time series
#' @param threshold A float number defining the threshold considered. Default to 1.
#' @return A float number corresponding to the amount fallen the days above or equal the input threshold.
#' @export

index.amountFreqGE <- function(ts, threshold = 1) {
      sum(ts[ts >= threshold], na.rm = TRUE)
}
