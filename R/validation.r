#' @title Validation
#' 
#' @description Function to compute VALUE indices and scores for each variable.
#'
#' @param obs A field or station data containing the observed climate data
#' @param prd A field containing the simulated climate by the model
#' @param lag.max Optional. Maximum lags considered for the autocorrelation. Default = 3
#' @param lowVarPeriod Optional. Low pass filter. Default = 1
#' @param Nbins Optional. Number of intervals considered in the calculation of the Cramer Von Misses. Default = 100
#' @param prob Optional. Inverse of the return period. Default = 1/20 corresponding with the 20 years return value.
#' 
#' @return A validation object. A 4-D array (season, station, member, index) with the indices and scores obtained for each season (DJF, MAM, JJA, SON and Annual), location and member.
#' 
#' @export
#'  
#' @family validation
#'
#' @examples
#' # Precipitation
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations
#' prd <- precipIberiaECA$predictions
#' valObject <- validation(obs, prd, lag.max = 3, lowVarPeriod = 1, Nbins = 100, prob = 1/20) 
#' # Maximum Temperature
#' data(tasmaxIberiaECA)
#' obs <- tasmaxIberiaECA$observations
#' prd <- tasmaxIberiaECA$predictions
#' valObject <- validation(obs, prd, lag.max = 3, lowVarPeriod = 1, Nbins = 100, prob = 1/20)

validation <- function(obs, prd, lag.max = 3, lowVarPeriod = 1, Nbins = 100, prob = 1/20) {
  vectorialDates <- getVectorialDates(obs)
  yo <- vectorialDates[,1]
  mo <- vectorialDates[,2]
  so <- vectorialDates[,3]
  yoS <- unique(yo)
  dimObs <- dim(obs$Data)
  obs.time.index <- grep("^time$", attr(obs$Data, "dimensions"))
  obs.station.index <- grep("^station$", attr(obs$Data, "dimensions"))
  dimPrd <- dim(prd$Data)
  prd.time.index <- grep("^time$", attr(prd$Data, "dimensions"))
  prd.station.index <- grep("^station$", attr(prd$Data, "dimensions"))
  prd.member.index <- grep("^member$", attr(prd$Data, "dimensions"))
  if (length(prd.member.index)==0){
    dimValidation <- c(5,dim(obs$Data)[obs.station.index],1,1)
    marginValidation <- c(prd.station.index)
  }else{
    dimValidation <- c(5,dim(obs$Data)[obs.station.index],dim(prd$Data)[prd.member.index],1)
    marginValidation <- c(prd.station.index, prd.member.index)
  }
  if ((any(grepl(obs$Variable$varName,c("tas","mean temperature","tmean"))))){
    upper.threshold <- 25
    lower.threshold <- 15
    dimValidation[4] <- 45
    validation <- array(data = NA, dim = dimValidation, dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dimValidation[3], score = c("obsMean","obsVar","obsSkewness","obsT25","obsT15","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT25","prdT15","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
  }
  if ((any(grepl(obs$Variable$varName,c("tasmax","maximum temperature","tmax"))))){
    upper.threshold <- 30
    lower.threshold <- 0
    dimValidation[4] <- 45
    validation <- array(data = NA, dim = dimValidation, dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dimValidation[3], score = c("obsMean","obsVar","obsSkewness","obsT30","obsT00","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT30","prdT00","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
  }
  if ((any(grepl(obs$Variable$varName,c("tasmin","minimum temperature","tmin"))))){
    upper.threshold <- 20
    lower.threshold <- 0
    dimValidation[4] <- 45
    validation <- array(data = NA, dim = dimValidation, dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dimValidation[3], score = c("obsMean","obsVar","obsSkewness","obsT20","obsT00","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT20","prdT00","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
  }
  if (any(grepl(obs$Variable$varName,c("wss","wind","windspeed")))){
    upper.threshold <- 20
    lower.threshold <- 1
    dimValidation[4] <- 45
    validation <- array(data = NA, dim = dimValidation, dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dimValidation[3], score = c("obsMean","obsVar","obsSkewness","obsW20","obsW1","obsW98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsHighSpell50","obsHighSpell90","obsMaxHighSpell","obsSlowSpell50","obsSlowSpell90","obsMaxSlowSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdW20","prdW1","prdW98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdHighSpell50","prdHighSpell90","prdMaxHighSpell","prdSlowSpell50","prdSlowSpell90","prdMaxSlowSpell","prdProVarLowFreq")))
  }
  if (any(grepl(obs$Variable$varName,c("rss","radiation","sunshine","rds")))){
    upper.threshold <- 450
    lower.threshold <- 120
    dimValidation[4] <- 45
    validation <- array(data = NA, dim = dimValidation, dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dimValidation[3], score = c("obsMean","obsVar","obsSkewness","obsW20","obsW1","obsW98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsHighSpell50","obsHighSpell90","obsMaxHighSpell","obsSlowSpell50","obsSlowSpell90","obsMaxSlowSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdW20","prdW1","prdW98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdHighSpell50","prdHighSpell90","prdMaxHighSpell","prdSlowSpell50","prdSlowSpell90","prdMaxSlowSpell","prdProVarLowFreq")))
  }
  if (any(grepl(obs$Variable$varName,c("pr","tp","precipitation","precip")))){
    pr.threshold <- 1
    r10.threshold <- 10
    dimValidation[4] <- 55
    validation <- array(data = NA, dim = dimValidation, dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:dimValidation[3], score = c("obsMean","obsVar","obsSkewness","obsR01","obsR10","obsR10p","obsR98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsWWProb","obsWDProb","obsDWProb","obsDDProb","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWetSpell50","obsWetSpell90","obsMaxWetSpell","obsDrySpell50","obsDrySpell90","obsMaxDrySpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdR01","prdR10","prdR10p","prdR98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdWWProb","prdWDProb","prdDWProb","prdDDProb","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWetSpell50","prdWetSpell90","prdMaxWetSpell","prdDrySpell50","prdDrySpell90","prdDryWetSpell","prdProVarLowFreq")))
  }
  if ((any(grepl(obs$Variable$varName,c("tas","mean temperature","tmean","tasmax","maximum temperature","tmax","tasmin","minimum temperature","tmin","wss","wind","windspeed","rss","radiation","sunshine","rds"))))){
    validation[1,,,1] <- getMean(obs$Data, obs.station.index)
    validation[1,,,2] <- getVar(obs$Data, obs.station.index)
    validation[1,,,3] <- getSkew(obs$Data, obs.station.index)
    aux <- getFreqGT(obs$Data, upper.threshold, MARGIN = obs.station.index)
    validation[1,,,4] <- aux/length(yoS)
    aux <- getFreqLT(obs$Data, lower.threshold, MARGIN = obs.station.index)
    validation[1,,,5] <- aux/length(yoS)
    validation[1,,,6] <- get98th(obs$Data, obs.station.index)
    validation[1,,,7:9] <- getACF(obs$Data, lag.max)
    validation[1,,,10:11] <- getReturnValue(obs$Data, prob, INDEX = yo)
    validation[1,,,12:15] <- getAnnualCicle(obs$Data, INDEX = mo)
    validation[1,,,16:18] <- getGTsld(obs$Data, upper.threshold, INDEX = yo)
    validation[1,,,19:21] <- getLTsld(obs$Data, lower.threshold, INDEX = yo)
    validation[1,,,22] <- getVarLF(obs$Data, lowVarPeriod, INDEX = yo)
    validation[1,,,24] <- getMean(prd$Data, marginValidation)
    validation[1,,,25] <- getVar(prd$Data, marginValidation)
    validation[1,,,26] <- getSkew(prd$Data, marginValidation)
    aux <- getFreqGT(prd$Data, upper.threshold, MARGIN = marginValidation)
    validation[1,,,27] <- aux/length(yoS)
    aux <- getFreqLT(prd$Data, upper.threshold, MARGIN = marginValidation)
    validation[1,,,28] <- aux/length(yoS)
    validation[1,,,29] <- get98th(prd$Data, marginValidation)
    if (length(prd.member.index)==0){
      validation[1,,,23] <- getCM(obs$Data, prd$Data, Nbins = Nbins)
      validation[1,,,30:32] <- getACF(prd$Data, lag.max)
      validation[1,,,33:34] <- getReturnValue(prd$Data, prob, INDEX = yo)
      validation[1,,,35:38] <- getAnnualCicle(prd$Data, INDEX = mo)
      validation[1,,,39:41] <- getGTsld(prd$Data, upper.threshold, INDEX = yo)
      validation[1,,,42:44] <- getLTsld(prd$Data, lower.threshold, INDEX = yo)
      validation[1,,,45] <- getVarLF(prd$Data, lowVarPeriod, INDEX = yo)
    }else{
      for (m in 1:dimValidation[3]){
        indPrdMember <- rep(list(bquote()), length(dimPrd))
        for (d in 1:length(dimPrd)){
          indPrdMember[[d]] <- 1:dimPrd[d]
        }
        indPrdMember[[prd.member.index]] <- m
        callPrdMember <- as.call(c(list(as.name("["),quote(prd$Data)), indPrdMember))
        validation[1,,m,23] <- getCM(eval(callObs), eval(callPrdMember), Nbins = Nbins)
        validation[1,,m,30:32] <- getACF(eval(callPrdMember), lag.max)
        validation[1,,m,33:34] <- getReturnValue(eval(callPrdMember), prob, INDEX = yo)
        validation[1,,m,35:38] <- getAnnualCicle(eval(callPrdMember), INDEX = mo)
        validation[1,,m,39:41] <- getGTsld(eval(callPrdMember), upper.threshold, INDEX = yo)
        validation[1,,m,42:44] <- getLTsld(eval(callPrdMember), lower.threshold, INDEX = yo)
        validation[1,,m,45] <- getVarLF(eval(callPrdMember), lowVarPeriod, INDEX = yo)
      }
    }
    for (s in 1:4){
      indSeason <- which(so == s)
      if (length(indSeason)>0){
        yoSS <- yo[indSeason]
        indObs <- rep(list(bquote()), length(dimObs))
        for (d in 1:length(dimObs)){
          indObs[[d]] <- 1:dimObs[d]
        }
        indObs[[obs.time.index]] <- indSeason
        callObs <- as.call(c(list(as.name("["),quote(obs$Data)), indObs))
        validation[s+1,,,1] <- getMean(eval(callObs), obs.station.index)
        validation[s+1,,,2] <- getVar(eval(callObs), obs.station.index)
        validation[s+1,,,3] <- getSkew(eval(callObs), obs.station.index)
        aux <- getFreqGT(eval(callObs), upper.threshold, MARGIN = obs.station.index)
        validation[s+1,,,4] <- aux/length(yoS)
        aux <- getFreqLT(eval(callObs), lower.threshold, MARGIN = obs.station.index)
        validation[s+1,,,5] <- aux/length(yoS)
        validation[s+1,,,6] <- get98th(eval(callObs), obs.station.index)
        validation[s+1,,,7:9] <- getACF(eval(callObs), lag.max)
        validation[s+1,,,10:11] <- getReturnValue(eval(callObs), prob, INDEX = yoSS)
        validation[s+1,,,16:18] <- getGTsld(eval(callObs), upper.threshold, INDEX = yoSS)
        validation[s+1,,,19:21] <- getLTsld(eval(callObs), lower.threshold, INDEX = yoSS)
        validation[s+1,,,22] <- getVarLF(eval(callObs), lowVarPeriod, INDEX = yoSS)
        indPrd <- rep(list(bquote()), length(dimPrd))
        for (d in 1:length(dimPrd)){
          indPrd[[d]] <- 1:dimPrd[d]
        }
        indPrd[[prd.time.index]] <- indSeason
        callPrd <- as.call(c(list(as.name("["),quote(prd$Data)), indPrd))
        validation[s+1,,,24] <- getMean(eval(callPrd), marginValidation)
        validation[s+1,,,25] <- getVar(eval(callPrd), marginValidation)
        validation[s+1,,,26] <- getSkew(eval(callPrd), marginValidation)
        aux <- getFreqGT(eval(callPrd), upper.threshold, MARGIN = marginValidation)
        validation[s+1,,,27] <- aux/length(yoS)
        aux <- getFreqLT(eval(callPrd), upper.threshold, MARGIN = marginValidation)
        validation[s+1,,,28] <- aux/length(yoS)
        validation[s+1,,,29] <- get98th(eval(callPrd), marginValidation)
        if (length(prd.member.index)==0){
          validation[s+1,,,23] <- getCM(eval(callObs), eval(callPrd), Nbins = Nbins)
          validation[s+1,,,30:32] <- getACF(eval(callPrd), lag.max)
          validation[s+1,,,33:34] <- getReturnValue(eval(callPrd), prob, INDEX = yoSS)
          validation[s+1,,,39:41] <- getGTsld(eval(callPrd), upper.threshold, INDEX = yoSS)
          validation[s+1,,,42:44] <- getLTsld(eval(callPrd), lower.threshold, INDEX = yoSS)
          validation[s+1,,,45] <- getVarLF(eval(callPrd), lowVarPeriod, INDEX = yoSS)
        }else{
          for (m in 1:dimValidation[3]){
            indPrdMember <- rep(list(bquote()), length(dimPrd))
            for (d in 1:length(dimPrd)){
              indPrdMember[[d]] <- 1:dimPrd[d]
            }
            indPrdMember[[prd.time.index]] <- indSeason
            indPrdMember[[prd.member.index]] <- m
            callPrdMember <- as.call(c(list(as.name("["),quote(prd$Data)), indPrdMember))
            validation[s+1,,m,23] <- getCM(eval(callObs), eval(callPrdMember), Nbins = Nbins)
            validation[s+1,,m,30:32] <- getACF(eval(callPrdMember), lag.max)
            validation[s+1,,m,33:34] <- getReturnValue(eval(callPrdMember), prob, INDEX = yoSS)
            validation[s+1,,m,39:41] <- getGTsld(eval(callPrdMember), upper.threshold, INDEX = yoSS)
            validation[s+1,,m,42:44] <- getLTsld(eval(callPrdMember), lower.threshold, INDEX = yoSS)
            validation[s+1,,m,45] <- getVarLF(eval(callPrdMember), lowVarPeriod, INDEX = yoSS)
          }
        }
      }
    }
  }
  if (any(grepl(obs$Variable$varName,c("pr","tp","precipitation","precip")))){
    validation[1,,,1] <- getMean(obs$Data, obs.station.index)
    validation[1,,,2] <- getVar(obs$Data, obs.station.index)
    validation[1,,,3] <- getSkew(obs$Data, obs.station.index)
    aux <- getFreqGET(obs$Data, pr.threshold, MARGIN = obs.station.index)
    validation[1,,,4] <- aux/length(yoS)
    aux <- getFreqGT(obs$Data, r10.threshold, MARGIN = obs.station.index)
    validation[1,,,5] <- aux/length(yoS)
    aux <- getAmountFreqGT(obs$Data, r10.threshold, MARGIN = obs.station.index)
    validation[1,,,6] <- aux/length(yoS)
    validation[1,,,7] <- getWet98th(obs$Data, pr.threshold, MARGIN = obs.station.index)
    validation[1,,,8:10] <- getACF(obs$Data, lag.max)
    validation[1,,,11:12] <- getReturnValue(obs$Data, prob, INDEX = yo)
    validation[1,,,13] <- getFreqWW(obs$Data, pr.threshold)
    validation[1,,,14] <- getFreqWD(obs$Data, pr.threshold)
    validation[1,,,15] <- getFreqDW(obs$Data, pr.threshold)
    validation[1,,,16] <- getFreqDD(obs$Data, pr.threshold)
    validation[1,,,17:20] <- getAnnualCicle(obs$Data, INDEX = mo)
    validation[1,,,21:26] <- getWDsld(obs$Data, pr.threshold, INDEX = yo)
    validation[1,,,27] <- getVarLF(obs$Data, lowVarPeriod, INDEX = yo)
    validation[1,,,29] <- getMean(prd$Data, marginValidation)
    validation[1,,,30] <- getVar(prd$Data, marginValidation)
    validation[1,,,31] <- getSkew(prd$Data, marginValidation)
    aux <- getFreqGET(prd$Data, pr.threshold, MARGIN = marginValidation)
    validation[1,,,32] <- aux/length(yoS)
    aux <- getFreqGT(prd$Data, r10.threshold, MARGIN = marginValidation)
    validation[1,,,33] <- aux/length(yoS)
    aux <- getAmountFreqGT(prd$Data, r10.threshold, MARGIN = marginValidation)
    validation[1,,,34] <- aux/length(yoS)
    validation[1,,,35] <- getWet98th(prd$Data, pr.threshold, MARGIN = marginValidation)
    if (length(prd.member.index)==0){
      validation[1,,,28] <- getCM(obs$Data, prd$Data, Nbins = Nbins)
      validation[1,,,36:38] <- getACF(prd$Data, lag.max)
      validation[1,,,39:40] <- getReturnValue(prd$Data, prob, INDEX = yo)
      validation[1,,,41] <- getFreqWW(prd$Data, pr.threshold)
      validation[1,,,42] <- getFreqWD(prd$Data, pr.threshold)
      validation[1,,,43] <- getFreqDW(prd$Data, pr.threshold)
      validation[1,,,44] <- getFreqDD(prd$Data, pr.threshold)
      validation[1,,,45:48] <- getAnnualCicle(prd$Data, INDEX = mo)
      validation[1,,,49:54] <- getWDsld(prd$Data, pr.threshold, INDEX = yo)
      validation[1,,,55] <- getVarLF(prd$Data, lowVarPeriod, INDEX = yo)
    }else{
      for (m in 1:dimValidation[3]){
        indPrdMember <- rep(list(bquote()), length(dimPrd))
        for (d in 1:length(dimPrd)){
          indPrdMember[[d]] <- 1:dimPrd[d]
        }
        indPrdMember[[prd.member.index]] <- m
        callPrdMember <- as.call(c(list(as.name("["),quote(prd$Data)), indPrdMember))
        validation[1,,m,28] <- getCM(eval(callObs), eval(callPrdMember), Nbins = Nbins)
        validation[1,,m,36:38] <- getACF(eval(callPrdMember), lag.max)
        validation[1,,m,39:40] <- getReturnValue(eval(callPrdMember), prob, INDEX = yo)
        validation[1,,m,41] <- getFreqWW(eval(callPrdMember), pr.threshold)
        validation[1,,m,42] <- getFreqWD(eval(callPrdMember), pr.threshold)
        validation[1,,m,43] <- getFreqDW(eval(callPrdMember), pr.threshold)
        validation[1,,m,44] <- getFreqDD(eval(callPrdMember), pr.threshold)
        validation[1,,m,45:48] <- getAnnualCicle(eval(callPrdMember), INDEX = mo)
        validation[1,,m,49:54] <- getWDsld(eval(callPrdMember), pr.threshold, INDEX = yo)
        validation[1,,m,55] <- getVarLF(eval(callPrdMember), lowVarPeriod, INDEX = yo)
      }
    }
    for (s in 1:4){
      indSeason <- which(so == s)
      if (length(indSeason)>0){
        yoSS <- yo[indSeason]
        yoSSS <- unique(yoSS)
        indObs <- rep(list(bquote()), length(dimObs))
        for (d in 1:length(dimObs)){
          indObs[[d]] <- 1:dimObs[d]
        }
        indObs[[obs.time.index]] <- indSeason
        callObs <- as.call(c(list(as.name("["),quote(obs$Data)), indObs))
        validation[s+1,,,1] <- getMean(eval(callObs), obs.station.index)
        validation[s+1,,,2] <- getVar(eval(callObs), obs.station.index)
        validation[s+1,,,3] <- getSkew(eval(callObs), obs.station.index)
        
        aux <- getFreqGET(eval(callObs), pr.threshold, MARGIN = obs.station.index)
        validation[s+1,,,4] <- aux/length(yoSSS)
        aux <- getFreqGT(eval(callObs), r10.threshold, MARGIN = obs.station.index)
        validation[s+1,,,5] <- aux/length(yoSSS)
        aux <- getAmountFreqGT(eval(callObs), r10.threshold, MARGIN = obs.station.index)
        validation[s+1,,,6] <- aux/length(yoSSS)
        validation[s+1,,,7] <- getWet98th(eval(callObs), pr.threshold, MARGIN = obs.station.index)
        validation[s+1,,,8:10] <- getACF(eval(callObs), lag.max)
        validation[s+1,,,11:12] <- getReturnValue(eval(callObs), prob, INDEX = yoSS)
        validation[s+1,,,13] <- getFreqWW(eval(callObs), pr.threshold)
        validation[s+1,,,14] <- getFreqWD(eval(callObs), pr.threshold)
        validation[s+1,,,15] <- getFreqDW(eval(callObs), pr.threshold)
        validation[s+1,,,16] <- getFreqDD(eval(callObs), pr.threshold)
        validation[s+1,,,21:26] <- getWDsld(eval(callObs), pr.threshold, INDEX = yoSS)
        validation[s+1,,,27] <- getVarLF(eval(callObs), lowVarPeriod, INDEX = yoSS)
        indPrd <- rep(list(bquote()), length(dimPrd))
        for (d in 1:length(dimPrd)){
          indPrd[[d]] <- 1:dimPrd[d]
        }
        indPrd[[prd.time.index]] <- indSeason
        callPrd <- as.call(c(list(as.name("["),quote(prd$Data)), indPrd))
        validation[s+1,,,29] <- getMean(eval(callPrd), marginValidation)
        validation[s+1,,,30] <- getVar(eval(callPrd), marginValidation)
        validation[s+1,,,31] <- getSkew(eval(callPrd), marginValidation)
        aux <- getFreqGET(eval(callPrd), pr.threshold, MARGIN = marginValidation)
        validation[s+1,,,32] <- aux/length(yoSSS)
        aux <- getFreqGT(eval(callPrd), r10.threshold, MARGIN = marginValidation)
        validation[s+1,,,33] <- aux/length(yoSSS)
        aux <- getAmountFreqGT(eval(callPrd), r10.threshold, MARGIN = marginValidation)
        validation[s+1,,,34] <- aux/length(yoSSS)
        validation[s+1,,,35] <- getWet98th(eval(callPrd), pr.threshold, MARGIN = marginValidation)
        if (length(prd.member.index)==0){
          validation[s+1,,,28] <- getCM(eval(callObs), eval(callPrd), Nbins = Nbins)
          validation[s+1,,,36:38] <- getACF(eval(callPrd), lag.max)
          validation[s+1,,,39:40] <- getReturnValue(eval(callPrd), prob, INDEX = yoSS)
          validation[s+1,,,41] <- getFreqWW(eval(callPrd), pr.threshold)
          validation[s+1,,,42] <- getFreqWD(eval(callPrd), pr.threshold)
          validation[s+1,,,43] <- getFreqDW(eval(callPrd), pr.threshold)
          validation[s+1,,,44] <- getFreqDD(eval(callPrd), pr.threshold)
          validation[s+1,,,49:54] <- getWDsld(eval(callPrd), pr.threshold, INDEX = yoSS)
          validation[s+1,,,55] <- getVarLF(eval(callPrd), lowVarPeriod, INDEX = yoSS)
        }else{
          for (m in 1:dimValidation[3]){
            indPrdMember <- rep(list(bquote()), length(dimPrd))
            for (d in 1:length(dimPrd)){
              indPrdMember[[d]] <- 1:dimPrd[d]
            }
            indPrdMember[[prd.time.index]] <- indSeason
            indPrdMember[[prd.member.index]] <- m
            callPrdMember <- as.call(c(list(as.name("["),quote(prd$Data)), indPrdMember))
            validation[s+1,,m,28] <- getCM(eval(callObs), eval(callPrdMember), Nbins = Nbins)
            validation[s+1,,m,36:38] <- getACF(eval(callPrdMember), lag.max)
            validation[s+1,,m,39:40] <- getReturnValue(eval(callPrdMember), prob, INDEX = yoSS)
            validation[s+1,,m,41] <- getFreqWW(eval(callPrdMember), pr.threshold)
            validation[s+1,,m,42] <- getFreqWD(eval(callPrdMember), pr.threshold)
            validation[s+1,,m,43] <- getFreqDW(eval(callPrdMember), pr.threshold)
            validation[s+1,,m,44] <- getFreqDD(eval(callPrdMember), pr.threshold)
            validation[s+1,,m,49:54] <- getWDsld(eval(callPrdMember), pr.threshold, INDEX = yoSS)
            validation[s+1,,m,55] <- getVarLF(eval(callPrdMember), lowVarPeriod, INDEX = yoSS)
          }
        }
      }
    }
  }
  return(validation)
}
