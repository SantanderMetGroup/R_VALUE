validationTemp <- function(obs, prd, warm.threshold = 25, cold.threshold = 15, lag.max = 3, lowVarPeriod = 1, Nbins = 100, prob = 1/20) {
  # Tmean: warm.threshold = 25, cold.threshold = 15
  # Tmin: warm.threshold = 20, cold.threshold = 0
  # Tmax: warm.threshold = 30, cold.threshold = 0
  
  dimObs <- dim(obs$Data)
  obs.time.index <- grep("^time$", attr(obs$Data, "dimensions"))
  obs.station.index <- grep("^station$", attr(obs$Data, "dimensions"))
  dimPrd <- dim(prd$Data)
  prd.time.index <- grep("^time$", attr(prd$Data, "dimensions"))
  prd.station.index <- grep("^station$", attr(prd$Data, "dimensions"))
  prd.member.index <- grep("^member$", attr(prd$Data, "dimensions"))
  if (length(prd.member.index)==0){
    validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],1,45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1, score = c("obsMean","obsVar","obsSkewness","obsT25","obsT15","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT25","prdT15","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
  }else{
    validation <- array(data = NA, dim = c(5,dim(obs$Data)[obs.station.index],dim(prd$Data)[prd.member.index],45), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1:length(prd.member.index), score = c("obsMean","obsVar","obsSkewness","obsT25","obsT15","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT25","prdT15","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
  }
  
  validation[,,,1] <- getMean(obs)
  validation[,,,2] <- getVar(obs)
  validation[,,,3] <- getSkew(obs)
  validation[,,,4] <- getFreqGT(obs, warm.threshold)
  validation[,,,5] <- getFreqLT(obs, cold.threshold)
  validation[,,,6] <- get98th(obs)
  validation[,,,7:9] <- getACF(obs, lag.max)
  validation[,,,10:11] <- getReturnValue(obs, prob)
  validation[,,,12:15] <- getAnnualCicle(obs)
  validation[,,,16:18] <- getGTsld(obs, warm.threshold)
  validation[,,,19:21] <- getLTsld(obs, cold.threshold)
  validation[,,,22] <- getVarLF(obs, lowVarPeriod)
  validation[,,,23] <- getCM(obs, prd, Nbins = Nbins)
  
  validation[,,,24] <- getMean(obs)
  validation[,,,25] <- getVar(obs)
  validation[,,,26] <- getSkew(obs)
  validation[,,,27] <- getFreqGT(obs, warm.threshold)
  validation[,,,28] <- getFreqLT(obs, cold.threshold)
  validation[,,,29] <- get98th(obs)
  validation[,,,30:32] <- getACF(obs, lag.max)
  validation[,,,33:34] <- getReturnValue(obs, prob)
  validation[,,,35:38] <- getAnnualCicle(obs)
  validation[,,,39:41] <- getGTsld(obs, warm.threshold)
  validation[,,,42:44] <- getLTsld(obs, cold.threshold)
  validation[,,,45] <- getVarLF(obs, lowVarPeriod)
  
  
  return(validation)
}
# End
