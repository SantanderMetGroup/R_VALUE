#' @title Relative frequency lower or equal the threshold
#' @description Function to compute the proportion of days below or equal of a predefined threshold mean index. 
#' @author Douglas Maraun 
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1 
#' @return A float number corresponding to the proportion of days below or equal the defined threshold of the input.
#' @export

index.freq.rel.LE <- function(ts, threshold = 1) {
      sum(ts <= threshold, na.rm = TRUE)/length(ts)
}

