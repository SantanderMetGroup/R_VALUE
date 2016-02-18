#' @title Skewness
#' @description Function to compute the skewness index.
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @return A float number corresponding to the skewness index.
#' @export

index.skew <- function(ts) {
      mean.x <- index.mean(ts)
      std.x <- index.sd(ts)
      auxObj <- (ts - mean.x) / std.x
      index.mean(auxObj ^ 3)
}
