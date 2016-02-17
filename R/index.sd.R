#' @title Standard deviation
#' @description Function to compute the standard deviation index.
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @return A float number corresponding to the standard deviation of the input sample.
#' @export
#' @examples \dontrun{
#' # Precipitation standard deviation of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' index.sd(obs)
#' }

# Standard deviation:
index.sd <- function(ts) {
      sd(ts, na.rm = TRUE)
}

