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
#' valDetPrediction <- validation(obs, pred, lag.max = 3, lowVarPeriod = 1, Nbins = 100, prob = 1/20)
#' # Loading stochastic predictions (several realizations)
#' pred.file2 <- file.path(find.package("R.VALUE"), "example-prediction-multimember.zip")
#' pred2 <- loadValuePredictions(obs, pred.file2)
#' str(pred2$Data) # 3D array with 'member' dimension
#' valStoPrediction <- validation(obs, pred2, lag.max = 3, lowVarPeriod = 1, Nbins = 100, prob = 1/20)
#'

validation <- function(obs, prd, lag.max = 3, lowVarPeriod = 1, Nbins = 100, prob = 1/20) {
  dimObs <- dim(obs$Data)
  obs.time.index <- grep("^time$", attr(obs$Data, "dimensions"))
  obs.station.index <- grep("^station$", attr(obs$Data, "dimensions"))
  dimPrd <- dim(prd$Data)
  prd.time.index <- grep("^time$", attr(prd$Data, "dimensions"))
  prd.station.index <- grep("^station$", attr(prd$Data, "dimensions"))
  prd.member.index <- grep("^member$", attr(prd$Data, "dimensions"))
  if ((any(grepl(obs$Variable$varName,c("tas","mean temperature","tmean"))))){
    upper.threshold <- 25
    lower.threshold <- 15
    if (length(prd.member.index)==0){
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],1,45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1, score = c("obsMean","obsVar","obsSkewness","obsT25","obsT15","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT25","prdT15","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
    }else{
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],dim(prd$Data)[prd.member.index],45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dim(prd$Data)[prd.member.index], score = c("obsMean","obsVar","obsSkewness","obsT25","obsT15","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT25","prdT15","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
    }
  }
  if ((any(grepl(obs$Variable$varName,c("tasmax","maximum temperature","tmax"))))){
    upper.threshold <- 30
    lower.threshold <- 0
    if (length(prd.member.index)==0){
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],1,45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1, score = c("obsMean","obsVar","obsSkewness","obsT30","obsT00","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT30","prdT00","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
    }else{
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],dim(prd$Data)[prd.member.index],45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dim(prd$Data)[prd.member.index], score = c("obsMean","obsVar","obsSkewness","obsT30","obsT00","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT30","prdT00","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
    }
  }
  if ((any(grepl(obs$Variable$varName,c("tasmin","minimum temperature","tmin"))))){
    upper.threshold <- 20
    lower.threshold <- 0
    if (length(prd.member.index)==0){
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],1,45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1, score = c("obsMean","obsVar","obsSkewness","obsT20","obsT00","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT20","prdT00","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
    }else{
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],dim(prd$Data)[prd.member.index],45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dim(prd$Data)[prd.member.index], score = c("obsMean","obsVar","obsSkewness","obsT20","obsT00","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT20","prdT00","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
    }
  }
  if (any(grepl(obs$Variable$varName,c("wss","wind","windspeed")))){
    upper.threshold <- 20
    lower.threshold <- 1
    if (length(prd.member.index)==0){
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],1,45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1, score = c("obsMean","obsVar","obsSkewness","obsW20","obsW1","obsW98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsHighSpell50","obsHighSpell90","obsMaxHighSpell","obsSlowSpell50","obsSlowSpell90","obsMaxSlowSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdW20","prdW1","prdW98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdHighSpell50","prdHighSpell90","prdMaxHighSpell","prdSlowSpell50","prdSlowSpell90","prdMaxSlowSpell","prdProVarLowFreq")))
    }else{
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],dim(prd$Data)[prd.member.index],45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dim(prd$Data)[prd.member.index], score = c("obsMean","obsVar","obsSkewness","obsW20","obsW1","obsW98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsHighSpell50","obsHighSpell90","obsMaxHighSpell","obsSlowSpell50","obsSlowSpell90","obsMaxSlowSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdW20","prdW1","prdW98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdHighSpell50","prdHighSpell90","prdMaxHighSpell","prdSlowSpell50","prdSlowSpell90","prdMaxSlowSpell","prdProVarLowFreq")))
    }
  }
  if (any(grepl(obs$Variable$varName,c("rss","radiation","sunshine","rds")))){
    upper.threshold <- 450
    lower.threshold <- 120
    if (length(prd.member.index)==0){
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],1,45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1, score = c("obsMean","obsVar","obsSkewness","obsW20","obsW1","obsW98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsHighSpell50","obsHighSpell90","obsMaxHighSpell","obsSlowSpell50","obsSlowSpell90","obsMaxSlowSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdW20","prdW1","prdW98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdHighSpell50","prdHighSpell90","prdMaxHighSpell","prdSlowSpell50","prdSlowSpell90","prdMaxSlowSpell","prdProVarLowFreq")))
    }else{
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],dim(prd$Data)[prd.member.index],45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dim(prd$Data)[prd.member.index], score = c("obsMean","obsVar","obsSkewness","obsW20","obsW1","obsW98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsHighSpell50","obsHighSpell90","obsMaxHighSpell","obsSlowSpell50","obsSlowSpell90","obsMaxSlowSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdW20","prdW1","prdW98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdHighSpell50","prdHighSpell90","prdMaxHighSpell","prdSlowSpell50","prdSlowSpell90","prdMaxSlowSpell","prdProVarLowFreq")))
    }
  }
  if ((any(grepl(obs$Variable$varName,c("tas","mean temperature","tmean","tasmax","maximum temperature","tmax","tasmin","minimum temperature","tmin","wss","wind","windspeed","rss","radiation","sunshine","rds"))))){
    validation[,,,1] <- getMean(obs)
    validation[,,,2] <- getVar(obs)
    validation[,,,3] <- getSkew(obs)
    validation[,,,4] <- getFreqGT(obs, upper.threshold)
    validation[,,,5] <- getFreqLT(obs, lower.threshold)
    validation[,,,6] <- get98th(obs)
    validation[,,,7:9] <- getACF(obs, lag.max)
    validation[,,,10:11] <- getReturnValue(obs, prob)
    validation[,,,12:15] <- getAnnualCicle(obs)
    validation[,,,16:18] <- getGTsld(obs, upper.threshold)
    validation[,,,19:21] <- getLTsld(obs, lower.threshold)
    validation[,,,22] <- getVarLF(obs, lowVarPeriod)
    validation[,,,23] <- getCM(obs, prd, Nbins = Nbins)
    validation[,,,24] <- getMean(prd)
    validation[,,,25] <- getVar(prd)
    validation[,,,26] <- getSkew(prd)
    validation[,,,27] <- getFreqGT(prd, upper.threshold)
    validation[,,,28] <- getFreqLT(prd, lower.threshold)
    validation[,,,29] <- get98th(prd)
    validation[,,,30:32] <- getACF(prd, lag.max)
    validation[,,,33:34] <- getReturnValue(prd, prob)
    validation[,,,35:38] <- getAnnualCicle(prd)
    validation[,,,39:41] <- getGTsld(prd, upper.threshold)
    validation[,,,42:44] <- getLTsld(prd, lower.threshold)
    validation[,,,45] <- getVarLF(prd, lowVarPeriod)
  }
  if (any(grepl(obs$Variable$varName,c("pr","tp","precipitation","precip")))){
    pr.threshold <- 1
    r10.threshold <- 10
#     validation <- validationPrecip(obs, prd, pr.threshold = 1, r10.threshold = 10, lag.max = lag.max, lowVarPeriod = lowVarPeriod, Nbins = Nbins)
    if (length(prd.member.index)==0){
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],1,55), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1, score = c("obsMean","obsVar","obsSkewness","obsR01","obsR10","obsR10p","obsR98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsWWProb","obsWDProb","obsDWProb","obsDDProb","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWetSpell50","obsWetSpell90","obsMaxWetSpell","obsDrySpell50","obsDrySpell90","obsMaxDrySpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdR01","prdR10","prdR10p","prdR98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdWWProb","prdWDProb","prdDWProb","prdDDProb","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWetSpell50","prdWetSpell90","prdMaxWetSpell","prdDrySpell50","prdDrySpell90","prdDryWetSpell","prdProVarLowFreq")))
    }else{
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],dim(prd$Data)[prd.member.index],55), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dim(prd$Data)[prd.member.index], score = c("obsMean","obsVar","obsSkewness","obsR01","obsR10","obsR10p","obsR98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsWWProb","obsWDProb","obsDWProb","obsDDProb","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWetSpell50","obsWetSpell90","obsMaxWetSpell","obsDrySpell50","obsDrySpell90","obsMaxDrySpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdR01","prdR10","prdR10p","prdR98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdWWProb","prdWDProb","prdDWProb","prdDDProb","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWetSpell50","prdWetSpell90","prdMaxWetSpell","prdDrySpell50","prdDrySpell90","prdDryWetSpell","prdProVarLowFreq")))
    }
    validation[,,,1] <- getMean(obs)
    validation[,,,2] <- getVar(obs)
    validation[,,,3] <- getSkew(obs)
    validation[,,,4] <- getFreqGET(obs, pr.threshold)
    validation[,,,5] <- getFreqGT(obs, r10.threshold)
    validation[,,,6] <- getAmountFreqGT(obs, r10.threshold)
    validation[,,,7] <- getWet98th(obs, pr.threshold)
    validation[,,,8:10] <- getACF(obs, lag.max)
    validation[,,,11:12] <- getReturnValue(obs, prob)
    validation[,,,13] <- getFreqWW(obs, pr.threshold)
    validation[,,,14] <- getFreqWD(obs, pr.threshold)
    validation[,,,15] <- getFreqDW(obs, pr.threshold)
    validation[,,,16] <- getFreqDD(obs, pr.threshold)
    validation[,,,17:20] <- getAnnualCicle(obs)
    validation[,,,21:26] <- getWDsld(obs, pr.threshold)
    validation[,,,27] <- getVarLF(obs, lowVarPeriod)
    validation[,,,28] <- getCM(obs, prd, Nbins = Nbins)
    validation[,,,29] <- getMean(prd)
    validation[,,,30] <- getVar(prd)
    validation[,,,31] <- getSkew(prd)
    validation[,,,32] <- getFreqGET(prd, pr.threshold)
    validation[,,,33] <- getFreqGT(prd, r10.threshold)
    validation[,,,34] <- getAmountFreqGT(prd, r10.threshold)
    validation[,,,35] <- getWet98th(prd, pr.threshold)
    validation[,,,36:38] <- getACF(prd, lag.max)
    validation[,,,39:40] <- getReturnValue(prd, prob)
    validation[,,,41] <- getFreqWW(prd, pr.threshold)
    validation[,,,42] <- getFreqWD(prd, pr.threshold)
    validation[,,,43] <- getFreqDW(prd, pr.threshold)
    validation[,,,44] <- getFreqDD(prd, pr.threshold)
    validation[,,,45:48] <- getAnnualCicle(prd)
    validation[,,,49:54] <- getWDsld(prd, pr.threshold)
    validation[,,,55] <- getVarLF(prd, lowVarPeriod)
  }
  return(validation)
}
