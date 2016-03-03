#' @title Frequency above or equal the threshold
#' @description Function to compute the amount of days above or equal a predefined threshold mean index. Missing values are ignored.
#' @author Dougals Maraun \email{dmaraun@@geomar.de}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1.
#' @return A float number corresponding to the amount of days above or equal the defined threshold of the input.
#' @export

index.freqGE <- function(ts, threshold = 1) {
      sum(ts >= threshold, na.rm = TRUE)
}
