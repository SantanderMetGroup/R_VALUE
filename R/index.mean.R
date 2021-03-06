#' @title Arithmetic mean 
#' @description Function to compute the arithmetic mean index.
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @return A float number corresponding to the mean of the input.
#' @export


index.mean <- function(ts) {
      mean(ts, na.rm = TRUE)
}
