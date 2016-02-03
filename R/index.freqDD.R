#' @title Dry-dry probability
#' @description Function to compute the dry-dry probability index. 
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1.
#' @return A float number corresponding to the dry-dry transition probability.
#' @export

#' @examples \dontrun{
#' # Dry-dry transition probability of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' # Default precipitation threshold of 1 mm
#' index.freqDD(obs)
#' }

index.freqDD <- function(ts, threshold = 1) {
      indToday <- 1:(length(ts) - 1)
      indTomorrow <- 2:length(ts)
      mean((ts[indToday] < threshold)*(ts[indTomorrow] < threshold), na.rm = TRUE)
}
