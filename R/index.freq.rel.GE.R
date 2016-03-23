#' @title Realtive fequency above or equal the threshold
#' @description Function to compute the proportion of days above or equal a predefined threshold mean index. Missing values are ignored.
#' @author Douglas Maraun, J. Bedia, D. San-Martin, S. Herrera
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1.
#' @return A float number corresponding to the proportion of days above or equal the defined threshold of the input.
#' @export

index.freq.rel.GE <- function(ts, threshold = 1) {
      sum(ts >= threshold, na.rm = TRUE)/length(ts)
}
