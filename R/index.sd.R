#' @title Standard deviation
#' @description Function to compute the (quasi) standard deviation index.
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @return A float number corresponding to the standard deviation of the input sample.
#' @export

index.sd <- function(ts) {
      sd(ts, na.rm = TRUE)
}

