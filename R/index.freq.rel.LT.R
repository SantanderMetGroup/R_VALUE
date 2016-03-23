#' @title Relative frequency below the threshold
#' @description Function to compute the relative amount of days below a predefined threshold. 
#' @author Douglas Maraun, J. Bedia, D. San-Martin, S. Herrera
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1.
#' @return A float number corresponding to the proportion of days below the defined threshold of the input.
#' @export

index.freq.rel.LT <- function(ts, threshold = 1) {
      sum(ts < threshold, na.rm = TRUE)/length(ts)
}
