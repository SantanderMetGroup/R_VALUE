#' @title Arithmetic mean 
#' @description Function to compute the arithmetic mean index.
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @return A float number corresponding to the mean of the input.
#' @export
#' @examples \dontrun{
#' # Precipitation mean of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' index.mean(obs)
#' }

index.mean <- function(ts) {
      mean(ts, na.rm = TRUE)
}
