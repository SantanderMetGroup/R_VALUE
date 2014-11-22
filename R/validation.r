#' @title Validation
#' 
#' @description Calculation of the indicators and scores defined for each variable in the VALUE Action Cost.
#' 
#' @param obs
#' @param prd
#' 
#' @return A validation object.
#' 
#' @export
#'  
#' @family validation
#'
#'@examples
#' #Example predictions
#' obs.dataset <- file.path(find.package("R.VALUE"), "example-observations.zip")
#' obs <- loadValueStations(obs.dataset, "tmin", season = 6:8, years = 2001)
#' # Loading deterministic predictions
#' pred.file1 <- file.path(find.package("R.VALUE"), "example-prediction.txt")
#' pred <- loadValuePredictions(obs, pred.file1)
#' str(pred$Data) # 2D array
#' # Loading stochastic predictions (several realizations)
#' pred.file2 <- file.path(find.package("R.VALUE"), "example-prediction-multimember.zip")
#' pred2 <- loadValuePredictions(obs, pred.file2)
#' str(pred2$Data) # 3D array with 'member' dimension
#'

validation <- function(obs, prd, lag.max = 3, lowVarPeriod = 1, Nbins = 100) {
      if ((any(grepl(obs$Variable$varName,c("tas","mean temperature","tmean"))))){
            validation <- validationTemp(obs, prd, warm.threshold = 25, cold.threshold = 15, lag.max = lag.max, lowVarPeriod = lowVarPeriod, Nbins = Nbins)
      }
      if ((any(grepl(obs$Variable$varName,c("tasmax","maximum temperature","tmax"))))){
            validation <- validationTemp(obs, prd, warm.threshold = 30, cold.threshold = 0, lag.max = lag.max, lowVarPeriod = lowVarPeriod, Nbins = Nbins)
      }
      if ((any(grepl(obs$Variable$varName,c("tasmin","minimum temperature","tmin"))))){
            validation <- validationTemp(obs, prd, warm.threshold = 20, cold.threshold = 0, lag.max = lag.max, lowVarPeriod = lowVarPeriod, Nbins = Nbins)
      }
      if (any(grepl(obs$Variable$varName,c("pr","tp","precipitation","precip")))){
            validation <- validationPrecip(obs, prd, pr.threshold = 1, r10.threshold = 10, lag.max = lag.max, lowVarPeriod = lowVarPeriod, Nbins = Nbins)
      }
      if (any(grepl(obs$Variable$varName,c("wss","wind","windspeed")))){
            validation <- validationWind(obs, prd, upper.threshold = 20, lower.threshold = 1, lag.max = lag.max, lowVarPeriod = lowVarPeriod, Nbins = Nbins)
      }
      if (any(grepl(obs$Variable$varName,c("rss","radiation","sunshine","rds")))){
            validation <- validationRad(obs, prd, upper.threshold = 450, lower.threshold = 120, lag.max = lag.max, lowVarPeriod = lowVarPeriod, Nbins = Nbins)
      }
      return(validation)
}
