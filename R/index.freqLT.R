#' @title Frequency below the threshold
#' @description Function to compute the amount of days below a predefined threshold. 
#' @author Douglas Maraun \email{dmaraun@@geomar.de}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1.
#' @return A float number corresponding to the amount of days below the defined threshold of the input.
#' @export

index.freqLT <- function(ts, threshold = 1) {
      sum(ts < threshold, na.rm = TRUE)
}
