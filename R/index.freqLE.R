#' @title Frequency lower or equal the threshold
#' @description Function to compute the amount of days below or equal of a predefined threshold mean index. 
#' @author Douglas Maraun \email{dmaraun@@geomar.de}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the data
#' @param threshold A float number defining the threshold considered. Default to 1 
#' @return A float number corresponding to the amount of days below or equal the defined threshold of the input.
#' @export
#' @examples \dontrun{
#' # Dry days frequency of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' index.freqLE(obs, threshold = 0.5)
#' }

index.freqLE <- function(ts, threshold = 1) {
      sum(ts <= threshold, na.rm = TRUE)
}

