#' @title Frequency lower or equal the threshold
#' @description Function to compute the amount of days below or equal of a predefined threshold mean index. 
#' @author Douglas Maraun \email{dmaraun@@geomar.de}
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1 
#' @return A float number corresponding to the amount of days below or equal the defined threshold of the input.
#' @export

index.freqLE <- function(ts, threshold = 1) {
      sum(ts <= threshold, na.rm = TRUE)
}

