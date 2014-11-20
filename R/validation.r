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
