#' @title Amount from events above threshold
#' @description Function to compute the amount from events above a predefined threshold.
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the input time series
#' @param threshold A float number defining the threshold considered. Default to 1.
#' @return A float number corresponding to the amount of days above the defined threshold of the input.
#' @export
#' @examples \dontrun{
#' # Precipitation amounts in Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' # Above default threshold (1 mm)
#' index.amountFreqGT(obs)
#' # Above threshold of 10 mm
#' index.amountFreqGT(obs, 10)
#' }

index.amountFreqGT <- function(ts, threshold = 1){
      ts[ts <= threshold] <- NA
      sum(ts, na.rm = TRUE)
}
