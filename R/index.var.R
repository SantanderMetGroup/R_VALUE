#' @title Variance
#' @description Function to compute the variance index. 
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @return A float number corresponding to the variance of the input.
#' @export
#' @examples \dontrun{
#' # Precipitation variance of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' index.var(obs)
#' }

index.var <- function(ts) {
      var(ts, na.rm = TRUE)
}
