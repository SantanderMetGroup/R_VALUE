#' @title measure.NameOfTheMeasure
#' @description Function to compute the NameOfTheMeasure. The measure depends on the index nameOfTheIndex.
#' @param indexObs index computed from the observations
#' @param indexPrd index computed from the predictions
#' @param obs A vector of observations
#' @param prd A vector of predictions 
#' @param ... Additional parameters required by a particular measure
#' @details Some measures are computed directly from the original time series (e.g. temporal correlation),
#' whereas others are computed upon previouly computed indices (e.g. mean bias). 
#' Thus, both the indices and the original time series are required as input (thay can be NULL if not used).
#' @return Either a scalar or a vector corresponding to the measure value(s).
#' @export
#' @importFrom package function # OPTIONAL IN CASE OF PACKAGE DEPENDENCIES. Remove otherwise.
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
nameMeasure.value <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, ...){
      # Include the code needed to estimate the measure
      return(measure)
}
