#' @title Autocorrelation function
#' @description Function to compute the estimated autocorrelation function at pre-defined lags.
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @template templateIndexParams
#' @param lag.max Maximum lag considered for acf calculation. Default \code{lag.max = 1}.
#' @return A scalar with the estimated autocorrelation for that lag.
#' @export
#' @examples \dontrun{
#' # Autocorrelation of Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' index.acf(obs, lag.max = 2)
#' }

index.acf <- function(ts, lag.max = 1){
      meanObj <- acf(ts, na.action = na.pass,
                     plot = FALSE,
                     lag.max = lag.max,
                     type = "correlation",
                     demean = TRUE)
      tail(meanObj$acf,1)
}
