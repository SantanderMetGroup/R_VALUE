#' @title measure.NameOfTheMeasure
#' @description Function to compute the NameOfTheMeasure. The measure depends on the index nameOfTheIndex.
#' @param obs Index value for the observed series or vector with the observed time series. 
#' @param pred Index value for the predicted series or vector with the predicted time series. 
#' @param ... Additional parameters required by a particular measure
#' @details Some measures are computed directly from the original time series (e.g. temporal correlation),
#' whereas others are computed upon previouly computed indices (e.g. mean bias).
#' @return A scaler or a vector corresponding to the measure value(s).
#' @export
#' @importFrom package function # OPTIONAL IN CASE OF PACKAGE DEPENDENCIES. Remove otherwise.
#' @family validation
#' @author corresponding author \email{author@@email.com}, author2, ...
#' @examples \dontrun{
#' # Measure Braganca:
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,1]
#' prd <- precipIberiaECA$observations$Data[,2]
#' obsMean <- nameMeasure.value(obs,prd)
#' str(obsMean)
#' }

# Measure:
nameMeasure.value <- function(obs, pred, ...){
      # Include the code needed to estimate the measure
      return(measure)
}
