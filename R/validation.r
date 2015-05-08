#' @title Validation
#' 
#' @description Function to compute VALUE indices and scores for each variable.
#'
#' @author S. Herrera, J. Bedia, D. San-Martín
#' 
#' @param obs A field or station data containing the observed climate data
#' @param prd A field containing the simulated climate by the model
#' @param lag.max Optional. Maximum lags considered for the autocorrelation. Default = 3
#' @param lowVarPeriod Optional. Low pass filter. Default = 1
#' @param Nbins Optional. Number of intervals considered in the calculation of the Cramer Von Misses. Default = 100
#' @param prob Optional. Inverse of the return period. Default = 1/20 corresponding with the 20 years return value.
#' 
#' @return A validation object. A 4-D array (season, station, member, index) with the indices and scores obtained for each season (DJF, MAM, JJA, SON and Annual), location and just one member.
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
  obj <- getIntersect(obs,prd)
  obs <- obj$obs
  prd <- obj$prd
  vectorialDates <- getVectorialDates(obs)
  yo <- vectorialDates[,1]
  mo <- vectorialDates[,2]
  so <- vectorialDates[,3]
  obs.station.index <- grep("^station$", attr(obs$Data, "dimensions"))
  dimPrd <- dim(prd$Data)
  if(length(dimPrd)==2){
    # add dummy member dimension
    auxData = dim(prd$Data)
    auxData <- array(data = NA, dim = c(1,dimPrd))
    auxData[1,,] = prd$Data
    attr(auxData,"dimensions") = c("member",attr(prd$Data,"dimensions"))
    prd$Data = auxData
    remove(auxData)
  }
  prd.station.index <- grep("^station$", attr(prd$Data, "dimensions"))
  prd.member.index <- grep("^member$", attr(prd$Data, "dimensions"))
  
  dimValidation <- c(5,dim(obs$Data)[obs.station.index],1,1)
  
  season = c("Annual","DJF","MAM","JJA","SON")
  stations = dimnames(obs$xyCoords)[[1]]
  realizations = 1:dim(prd$Data)[prd.member.index]
  
  if ((any(grepl(obs$Variable$varName,c("tas","mean temperature","tmean"))))){
    score = c("obsMean","prdMean","Mean")
    score = c(score,"obsVar","prdVar","Var")
    score = c(score,"obsSkewness","prdSkewness","Skewness")
    score = c(score,"cmIndex")
    score = c(score,"r")
    score = c(score,"MAE")
    score = c(score,"obsACF1","obsACF2","obsACF3","prdACF1","prdACF2","prdACF3","ACF1","ACF2","ACF3")
    score = c(score,"obsT98p","prdT98p","T98p")
    score = c(score,"obsRV20lb","obsRV20ub","prdRV20lb","prdRV20ub","RV20lb","RV20ub")
    score = c(score,"obsT25","prdT25","T25")
    score = c(score,"obsT15","prdT15","T15")
    score = c(score,"obsWarmSpell50","obsColdSpell50","prdWarmSpell50","prdColdSpell50","WarmSpell50","ColdSpell50")
    score = c(score,"obsWarmSpell90","obsColdSpell90","prdWarmSpell90","prdColdSpell90","WarmSpell90","ColdSpell90")
    score = c(score,"obsMaxWarmSpell","obsMaxColdSpell","prdMaxWarmSpell","prdMaxColdSpell","MaxWarmSpell","MaxColdSpell")
    score = c(score,"obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","MinAC","MaxAC","AmpAC","RelAmpAC")
    score = c(score,"obsProVarLowFreq","prdProVarLowFreq","ProVarLowFreq")
    upper.threshold <- 25
    lower.threshold <- 15
  }
  if ((any(grepl(obs$Variable$varName,c("tasmax","maximum temperature","tmax"))))){
    score = c("obsMean","prdMean","Mean")
    score = c(score,"obsVar","prdVar","Var")
    score = c(score,"obsSkewness","prdSkewness","Skewness")
    score = c(score,"cmIndex")
    score = c(score,"r")
    score = c(score,"MAE")
    score = c(score,"obsACF1","obsACF2","obsACF3","prdACF1","prdACF2","prdACF3","ACF1","ACF2","ACF3")
    score = c(score,"obsT98p","prdT98p","T98p")
    score = c(score,"obsRV20lb","obsRV20ub","prdRV20lb","prdRV20ub","RV20lb","RV20ub")
    score = c(score,"obsT20","prdT20","T20")
    score = c(score,"obsT00","prdT00","T00")
    score = c(score,"obsWarmSpell50","obsColdSpell50","prdWarmSpell50","prdColdSpell50","WarmSpell50","ColdSpell50")
    score = c(score,"obsWarmSpell90","obsColdSpell90","prdWarmSpell90","prdColdSpell90","WarmSpell90","ColdSpell90")
    score = c(score,"obsMaxWarmSpell","obsMaxColdSpell","prdMaxWarmSpell","prdMaxColdSpell","MaxWarmSpell","MaxColdSpell")
    score = c(score,"obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","MinAC","MaxAC","AmpAC","RelAmpAC")
    score = c(score,"obsProVarLowFreq","prdProVarLowFreq","ProVarLowFreq")
    upper.threshold <- 30
    lower.threshold <- 0
  }
  if ((any(grepl(obs$Variable$varName,c("tasmin","minimum temperature","tmin"))))){
    score = c("obsMean","prdMean","Mean")
    score = c(score,"obsVar","prdVar","Var")
    score = c(score,"obsSkewness","prdSkewness","Skewness")
    score = c(score,"cmIndex")
    score = c(score,"r")
    score = c(score,"MAE")
    score = c(score,"obsACF1","obsACF2","obsACF3","prdACF1","prdACF2","prdACF3","ACF1","ACF2","ACF3")
    score = c(score,"obsT98p","prdT98p","T98p")
    score = c(score,"obsRV20lb","obsRV20ub","prdRV20lb","prdRV20ub","RV20lb","RV20ub")
    score = c(score,"obsT30","prdT30","T30")
    score = c(score,"obsT00","prdT00","T00")
    score = c(score,"obsWarmSpell50","obsColdSpell50","prdWarmSpell50","prdColdSpell50","WarmSpell50","ColdSpell50")
    score = c(score,"obsWarmSpell90","obsColdSpell90","prdWarmSpell90","prdColdSpell90","WarmSpell90","ColdSpell90")
    score = c(score,"obsMaxWarmSpell","obsMaxColdSpell","prdMaxWarmSpell","prdMaxColdSpell","MaxWarmSpell","MaxColdSpell")
    score = c(score,"obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","MinAC","MaxAC","AmpAC","RelAmpAC")
    score = c(score,"obsProVarLowFreq","prdProVarLowFreq","ProVarLowFreq")
    upper.threshold <- 20
    lower.threshold <- 0
  }
  if (any(grepl(obs$Variable$varName,c("pr","tp","precipitation","precip")))){
    score = c("obsMean","prdMean","Mean")
    score = c(score,"obsVar","prdVar","Var")
    score = c(score,"obsSkewness","prdSkewness","Skewness")
    score = c(score,"cmIndex")
    score = c(score,"r")
    score = c(score,"MAE")
    score = c(score,"obsACF1","obsACF2","obsACF3","prdACF1","prdACF2","prdACF3","ACF1","ACF2","ACF3")
    score = c(score,"obsR01","prdR01","R01")
    score = c(score,"obsT98p","prdT98p","T98p")
    score = c(score,"obsRV20lb","obsRV20ub","prdRV20lb","prdRV20ub","RV20lb","RV20ub")
    score = c(score,"obsR10p","prdR10p","R10p")
    score = c(score,"obsR10","prdR10","R10")
    score = c(score,"obsWetSpell50","obsDrySpell50","prdWetSpell50","prdDrySpell50","WetSpell50","DrySpell50")
    score = c(score,"obsWetSpell90","obsDrySpell90","prdWetSpell90","prdDrySpell90","WetSpell90","DrySpell90")
    score = c(score,"obsMaxWetSpell","obsMaxDrySpell","prdMaxWetSpell","prdMaxDrySpell","MaxWetSpell","MaxDrySpell")
    score = c(score,"obsWWProb","prdWWProb","WWProb")
    score = c(score,"obsDWProb","prdDWProb","DWProb")
    score = c(score,"obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","MinAC","MaxAC","AmpAC","RelAmpAC")
    score = c(score,"obsProVarLowFreq","prdProVarLowFreq","ProVarLowFreq")
    pr.threshold <- 1
    r10.threshold <- 10
  }
  
  dimValidation[4] <- length(score)
  validation <- array(data = NA, dim = dimValidation, dimnames = list(season = season, stations = stations, realizations = 1, score = score))
  for (s in 0:4){
    print(season[s+1])
    if(s==0){
      iTObs = 1:dim(obs$Data)[1]
    }else{
      iTObs = which(so == s)
    }
    prdAg = apply(prd$Data[,iTObs,,drop = F], c(2,3), mean, na.rm=TRUE)
    yoS <- yo[iTObs]
    moS <- mo[iTObs]
    yoU <- unique(yoS)
    for(sc in score){
      print(sc)
      scoAg <- array(data=NA,dim=c(length(realizations),length(stations)))
      if(sc=="obsMean"){
        # includes obsMean prdMean Mean
        validation[s+1,,,"obsMean"] <- getMean(obs$Data[iTObs,], obs.station.index)
        validation[s+1,,,"prdMean"] <- getMean(prdAg, c(2))
        validation[s+1,,,"Mean"] <- validation[s+1,,,"obsMean"] - validation[s+1,,,"prdMean"]
      }else if(sc=="obsVar"){
        # includes obsVar prdVar Var
        validation[s+1,,,"obsVar"] <- getVar(obs$Data[iTObs,], obs.station.index)
        scoAg <- getVar(prd$Data[,iTObs,,drop = F], c(1,3))
        validation[s+1,,,"prdVar"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
        validation[s+1,,,"Var"] <- validation[s+1,,,"obsVar"] / validation[s+1,,,"prdVar"]
      }else if(sc=="obsSkewness"){
        # includes obsSkewness prdSkewness Skewness
        validation[s+1,,,"obsSkewness"] <- getSkew(obs$Data[iTObs,], obs.station.index)
        for (m in 1:length(realizations)){
          scoAg[m,] <- getSkew(prd$Data[m,iTObs,], obs.station.index)
        }
        validation[s+1,,,"prdSkewness"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
        validation[s+1,,,"Skewness"] <- validation[s+1,,,"obsSkewness"] - validation[s+1,,,"prdSkewness"]
      }else if(sc=="cmIndex"){
        for (m in 1:length(realizations)){
          scoAg[m,] <- getCM(obs$Data[iTObs,], prd$Data[m,iTObs,], Nbins = Nbins)
        }
        validation[s+1,,,"cmIndex"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
      }else if(sc=="r"){
        validation[s+1,,,"r"] <- getCorrelation(obs$Data[iTObs,],prdAg,c(2))
      }else if(sc=="MAE"){
        for (m in 1:length(realizations)){
          scoAg[m,] <- getMae(obs$Data[iTObs,],prd$Data[m,iTObs,],c(2))
        }
        validation[s+1,,,"MAE"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
      }else if(sc=="obsACF1"){
        # includes obsACF1 obsACF2 obsACF3
        # includes prdACF1 prdACF2 prdACF3
        # includes ACF1 ACF2 ACF3
        if(s>0){
          validation[s+1,,,c("obsACF1","obsACF2","obsACF3")] <- getACF(obs$Data[iTObs,], lag.max)
          validation[s+1,,,c("prdACF1","prdACF2","prdACF3")] <- getACF(prdAg, lag.max)
          validation[s+1,,,c("ACF1","ACF2","ACF3")] <- validation[s+1,,,c("obsACF1","obsACF2","obsACF3")] - validation[s+1,,,c("prdACF1","prdACF2","prdACF3")]
        }
      }else if(sc=="obsT98p"){
        # includes obsT98p prdT98p T98p
        validation[s+1,,,"obsT98p"] <- get98th(obs$Data[iTObs,], obs.station.index)
        scoAg <- get98th(prd$Data[,iTObs,,drop = F], c(1,3))
        validation[s+1,,,"prdT98p"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
        validation[s+1,,,"T98p"] <- validation[s+1,,,"obsT98p"] - validation[s+1,,,"prdT98p"]
      }else if(sc=="obsRV20lb"){
        # includes obsRV20lb obsRV20ub
        # includes prdRV20lb prdRV20ub
        # includes RV20lb RV20ub
        validation[s+1,,,c("obsRV20lb","obsRV20ub")] <- getReturnValue(obs$Data[iTObs,], prob, INDEX = yoS)
        scoAg <- array(data=NA,dim=c(length(realizations),length(stations),2))
        for (m in 1:length(realizations)){
          scoAg[m,,] <- getReturnValue(prd$Data[m,iTObs,], prob, INDEX = yoS)[1,,1,]
        }
        validation[s+1,,,c("prdRV20lb","prdRV20ub")] <- apply(scoAg,c(2,3),mean,na.rm=TRUE)
        validation[s+1,,,c("RV20lb","RV20ub")] <- validation[s+1,,,c("obsRV20lb","obsRV20ub")] - validation[s+1,,,c("prdRV20lb","prdRV20ub")]
      }else if(sc=="obsT15"){
        # includes obsT15 prdT15 T15
        if(s==0){
          aux <- getFreqGT(obs$Data[iTObs,], 15, MARGIN = obs.station.index)
          validation[s+1,,,"obsT15"] <- aux/length(yoS)
          scoAg <- getFreqGT(prd$Data[,iTObs,,drop = F], 15, MARGIN = c(1,3))/length(yoU)
          validation[s+1,,,"prdT15"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
          validation[s+1,,,"T15"] <- validation[s+1,,,"obsT15"] - validation[s+1,,,"prdT15"]
        }
      }else if(sc=="obsT20"){
        # includes obsT20 prdT20 T20
        if(s==0){
          aux <- getFreqGT(obs$Data[iTObs,], 20, MARGIN = obs.station.index)
          validation[s+1,,,"obsT20"] <- aux/length(yoS)
          scoAg <- getFreqGT(prd$Data[,iTObs,,drop = F], 20, MARGIN = c(1,3))/length(yoU)
          validation[s+1,,,"prdT20"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
          validation[s+1,,,"T20"] <- validation[s+1,,,"obsT20"] - validation[s+1,,,"prdT20"]
        }
      }else if(sc=="obsT25"){
        # includes obsT25 prdT25 T25
        if(s==0){
          aux <- getFreqGT(obs$Data[iTObs,], 25, MARGIN = obs.station.index)
          validation[s+1,,,"obsT25"] <- aux/length(yoS)          
          scoAg <- getFreqGT(prd$Data[,iTObs,,drop = F], 25, MARGIN = c(1,3))/length(yoU)
          validation[s+1,,,"prdT25"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
          validation[s+1,,,"T25"] <- validation[s+1,,,"obsT25"] - validation[s+1,,,"prdT25"]
        }
      }else if(sc=="obsT30"){
        # includes obsT30 prdT30 T30
        if(s==0){
          aux <- getFreqGT(obs$Data[iTObs,], 30, MARGIN = obs.station.index)
          validation[s+1,,,"obsT30"] <- aux/length(yoS)          
          scoAg <- getFreqGT(prd$Data[,iTObs,,drop = F], 30, MARGIN = c(1,3))/length(yoU)
          validation[s+1,,,"prdT30"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
          validation[s+1,,,"T30"] <- validation[s+1,,,"obsT30"] - validation[s+1,,,"prdT30"]
        }
      }else if(sc=="obsT00"){
        # includes obsT00 prdT00 T00
        if(s==0){
          aux <- getFreqGT(obs$Data[iTObs,], 0, MARGIN = obs.station.index)
          validation[s+1,,,"obsT00"] <- aux/length(yoS)          
          scoAg <- getFreqGT(prd$Data[,iTObs,,drop = F], 0, MARGIN = c(1,3))/length(yoU)
          validation[s+1,,,"prdT00"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
          validation[s+1,,,"T00"] <- validation[s+1,,,"obsT00"] - validation[s+1,,,"T00"]
        }
      }else if(sc=="obsWarmSpell50"){
        # includes obsWarmSpell50 obsColdSpell50 prdWarmSpell50 prdColdSpell50 WarmSpell50 ColdSpell50
        # includes obsWarmSpell90 obsColdSpell90 prdWarmSpell90 prdColdSpell90 WarmSpell90 ColdSpell90
        # includes obsMaxColdSpell obsMaxWarmSpell prdMaxWarmSpell prdMaxColdSpell MaxWarmSpell MaxColdSpell
        validation[s+1,,,c("obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell")] <- getGTsld(obs$Data[iTObs,], upper.threshold, INDEX = yoS)
        validation[s+1,,,c("obsColdSpell50","obsColdSpell90","obsMaxColdSpell")] <- getLTsld(obs$Data[iTObs,], lower.threshold, INDEX = yoS)
        scoAg <- array(data=NA,dim=c(length(realizations),length(stations),3))
        for (m in 1:length(realizations)){
          scoAg[m,,] <- getGTsld(prd$Data[m,iTObs,], upper.threshold, INDEX = yoS)
        }
        validation[s+1,,,c("prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell")] <- apply(scoAg,c(2,3),mean,na.rm=TRUE)
        scoAg <- array(data=NA,dim=c(length(realizations),length(stations),3))
        for (m in 1:length(realizations)){
          scoAg[m,,] <- getLTsld(prd$Data[m,iTObs,], lower.threshold, INDEX = yoS)
        }
        validation[s+1,,,c("prdColdSpell50","prdColdSpell90","prdMaxColdSpell")] <- apply(scoAg,c(2,3),mean,na.rm=TRUE)
        if(s>0){
          validation[s+1,,,c("obsMaxWarmSpell","obsMaxColdSpell")] <- NA
          validation[s+1,,,c("prdMaxWarmSpell","prdMaxColdSpell")] <- NA
        }        
        validation[s+1,,,c("WarmSpell50","ColdSpell50")] <- validation[s+1,,,c("obsWarmSpell50","obsColdSpell50")] - validation[s+1,,,c("prdWarmSpell50","prdColdSpell50")]
        validation[s+1,,,c("WarmSpell90","ColdSpell90")] <- validation[s+1,,,c("obsWarmSpell90","obsColdSpell90")] - validation[s+1,,,c("prdWarmSpell90","prdColdSpell90")]
        validation[s+1,,,c("MaxWarmSpell","MaxColdSpell")] <- validation[s+1,,,c("obsMaxColdSpell","obsMaxWarmSpell")] - validation[s+1,,,c("prdMaxWarmSpell","prdMaxColdSpell")]
      }else if(sc=="obsMinAC"){
        # includes obsMinAC obsMaxAC obsAmpAC
        # includes prdMinAC prdMaxAC prdAmpAC
        # includes MinAC MaxAC AmpAC
        if(s==0){
          validation[s+1,,,c("obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC")] <- getAnnualCicle(obs$Data[iTObs,], INDEX = moS)
          scoAg <- array(data=NA,dim=c(length(realizations),length(stations),4))
          for (m in 1:length(realizations)){
            scoAg[m,,] <- getAnnualCicle(prd$Data[m,iTObs,], INDEX = moS)
          }
          validation[s+1,,,c("prdMinAC","prdMaxAC","prdAmpAC","RelAmpAC")] <- apply(scoAg,c(2,3),mean,na.rm=TRUE)
          validation[s+1,,,c("MinAC","MaxAC","AmpAC","RelAmpAC")] <- validation[s+1,,,c("obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC")] - validation[s+1,,,c("prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC")]
        }
      }else if(sc=="obsProVarLowFreq"){
        # includes obsProVarLowFreq prdProVarLowFreq ProVarLowFreq
        validation[s+1,,,"obsProVarLowFreq"] <- getVarLF(obs$Data[iTObs,], lowVarPeriod, INDEX = yoS)
        scoAg <- array(data=NA,dim=c(length(realizations),length(stations)))
        for (m in 1:length(realizations)){
          scoAg[m,] <- getVarLF(prd$Data[m,iTObs,], lowVarPeriod, INDEX = yoS)
        }
        validation[s+1,,,"prdProVarLowFreq"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
        validation[s+1,,,"ProVarLowFreq"] <- validation[s+1,,,"obsProVarLowFreq"] / validation[s+1,,,"prdProVarLowFreq"]
      }else if(sc=="obsR01"){
        # includes obsR01 prdR01 R01
        aux <- getFreqGET(obs$Data[iTObs,], pr.threshold, MARGIN = obs.station.index)
        validation[s+1,,,"obsR01"] <- aux/length(yoS)
        scoAg <- getFreqGET(prd$Data[,iTObs,,drop = F], pr.threshold, MARGIN = c(1,3))/length(yoU)
        validation[s+1,,,"prdR01"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
        validation[s+1,,,"R01"] <- validation[s+1,,,"obsR01"] - validation[s+1,,,"prdR01"]
      }else if(sc=="obsR10"){
        # includes obsR10 prdR10 R10
        aux <- getFreqGET(obs$Data[iTObs,], r10.threshold, MARGIN = obs.station.index)
        validation[s+1,,,"obsR10"] <- aux/length(yoS)
        scoAg <- getFreqGET(prd$Data[,iTObs,,drop = F], pr.threshold, MARGIN = c(1,3))/length(yoU)
        validation[s+1,,,"prdR10"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
        validation[s+1,,,"R10"] <- validation[s+1,,,"obsR10"] - validation[s+1,,,"prdR10"]
      }else if(sc=="obsR10p"){
        # includes obsR10p prdR10p R10p
        aux <- getAmountFreqGT(obs$Data[iTObs,], r10.threshold, MARGIN = obs.station.index)
        validation[s+1,,,"obsR10p"] <- aux/length(yoS)
        scoAg <- getAmountFreqGT(prd$Data[,iTObs,,drop = F], pr.threshold, MARGIN = c(1,3))/length(yoU)
        validation[s+1,,,"prdR10p"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
        validation[s+1,,,"R10p"] <- validation[s+1,,,"obsR10p"] - validation[s+1,,,"prdR10p"]
      }else if(sc=="obsWetSpell50"){
        # includes obsWetSpell50 obsDrySpell50 prdWetSpell50 prdDrySpell50 WetSpell50 DrySpell50
        # includes obsWetSpell90 obsDrySpell90 prdWetSpell90 prdDrySpell90 WetSpell90 DrySpell90
        # includes obsMaxDrySpell obsMaxWetSpell prdMaxWetSpell prdMaxDrySpell MaxWetSpell MaxDrySpell
        validation[s+1,,,c("obsWetSpell50","obsWetSpell90","obsMaxWetSpell","obsDrySpell50","obsDrySpell90","obsMaxDrySpell")] <- getWDsld(obs$Data[iTObs,], pr.threshold, INDEX = yoS)
        scoAg <- array(data=NA,dim=c(length(realizations),length(stations),6))
        for (m in 1:length(realizations)){
          scoAg[m,,] <- getWDsld(prd$Data[m,iTObs,], pr.threshold, INDEX = yoS)
        }
        validation[s+1,,,c("prdWetSpell50","prdWetSpell90","prdMaxWetSpell","prdDrySpell50","prdDrySpell90","prdMaxDrySpell")] <- apply(scoAg,c(2,3),mean,na.rm=TRUE)
        if(s>0){
          validation[s+1,,,c("obsMaxWetSpell","obsMaxDrySpell")] <- NA
          validation[s+1,,,c("prdMaxWetSpell","prdMaxDrySpell")] <- NA
        }        
        validation[s+1,,,c("WetSpell50","DrySpell50")] <- validation[s+1,,,c("obsWetSpell50","obsDrySpell50")] - validation[s+1,,,c("prdWetSpell50","prdDrySpell50")]
        validation[s+1,,,c("WetSpell90","DrySpell90")] <- validation[s+1,,,c("obsWetSpell90","obsDrySpell90")] - validation[s+1,,,c("prdWetSpell90","prdDrySpell90")]
        validation[s+1,,,c("MaxWetSpell","MaxDrySpell")] <- validation[s+1,,,c("obsMaxDrySpell","obsMaxWetSpell")] - validation[s+1,,,c("prdMaxWetSpell","prdMaxDrySpell")]
      }else if(sc=="obsWWProb"){
        # includes obsWWProb prdWWProb WWProb
        if(s>0){
          validation[s+1,,,"obsWWProb"] <- getFreqWW(obs$Data, pr.threshold)
          scoAg <- array(data=NA,dim=c(length(realizations),length(stations)))
          for (m in 1:length(realizations)){
            scoAg[m,] <- getFreqWW(prd$Data[,iTObs,], pr.threshold)
          }
          validation[s+1,,,"prdWWProb"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
          validation[s+1,,,"WWProb"] <- validation[s+1,,,"obsWWProb"] / validation[s+1,,,"prdWWProb"]
        }
      }else if(sc=="obsDWProb"){
        # includes obsDWProb prdDWProb DWProb
        if(s>0){
          validation[s+1,,,"obsDWProb"] <- getFreqDW(obs$Data, pr.threshold)
          scoAg <- array(data=NA,dim=c(length(realizations),length(stations)))
          for (m in 1:length(realizations)){
            scoAg[m,] <- getFreqDW(prd$Data[,iTObs,], pr.threshold)
          }
          validation[s+1,,,"prdDWProb"] <- apply(scoAg,c(2),mean,na.rm=TRUE)
          validation[s+1,,,"DWProb"] <- validation[s+1,,,"obsDWProb"] / validation[s+1,,,"prdDWProb"]
        }
      }
    }
  }
  return(validation)
}
