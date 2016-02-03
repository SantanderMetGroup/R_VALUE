#' @title Autocorrelation function
#' @description Function to compute the estimated autocorrelation function at pre-defined lags.
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @param ts A vector containing the time series
#' @param lag.max Maximum lag considered for acf calculation. Default \code{lag.max = 3}.
#' @return A vector with the estimated autocorrelation values.
#' @export
#' @examples \dontrun{
#' # Autocorrelation of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' plot(index.acf(obs, lag.max = 10), type = "h")
#' }


# Autocorrelation:
index.acf <- function(ts, lag.max = 3){
      meanObj <- acf(ts, na.action = na.pass,
                     plot = FALSE,
                     lag.max = lag.max,
                     type = "correlation",
                     demean = TRUE)
      meanObj$acf[2:(lag.max+1)]
}
