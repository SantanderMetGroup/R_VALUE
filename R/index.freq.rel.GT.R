#' @title Relative frequency above the threshold
#' @description Function to compute the proportion of days above a predefined threshold.
#' @author Douglas Maraun, J. Bedia, D. San-Martin, S. Herrera
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1.
#' @return A float number corresponding to the proportion of days above the defined threshold of the input.
#' @export

index.freq.rel.GT <- function(ts, threshold = 1) {
      sum(ts > threshold, na.rm = TRUE)/length(ts)
}
