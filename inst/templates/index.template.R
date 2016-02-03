#' @title index.NameOfTheIndex
#' @description Function to compute NameOfTheIndex index.
#' @param ts A time series of the target variable
#' @param dates Calendar dates corresponding to the time series records
#' @param ... Further parameters particular to the index definition, if any (e.g., threshold, probability, etc.)
#' @return Either a scalar or a vector depending on the index definition
#' @details  It is assumed that the input time series do not have any missing values (these are filtered previously).
#'  Date formatting corresponds to a character string or POSIXct defining the calendar dates, of the form
#'  \code{"%Y-%m-%d %H:%M:%S"}. See \code{\link{srtptime}} for further details on date formatting. 
#' @author corrresponding author \email{author@@email.com}, author2, ...
#' @export
#' @examples \dontrun{
#' # Observed precipitation in Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' index.nameOfTheIndex(obs)
#' }

index.NameOfTheIndex <- function(ts, dates, ...) {
      # Include the code needed to estimate the index
      return(index)
}



