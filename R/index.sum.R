#' @title Sum
#' @description Function to compute the sum index.
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @return A float number corresponding to the sum of the input.
#' @export
#' @examples \dontrun{
#' # Precipitation amount of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' index.sum(obs)
#' }

index.sum <- function(ts) {
      sum(ts, na.rm = TRUE)
}
