validationTmean <- function(obs, prd, warm.threshold = 25, cold.threshold = 15, lag.max = 3, lowVarPeriod = 1, Nbins = 100) {
      # Tmean: warm.threshold = 25, cold.threshold = 15
      # Tmin: warm.threshold = 20, cold.threshold = 0
      # Tmax: warm.threshold = 30, cold.threshold = 0
      
      validation <- array(data = NA, dim = c(5,dim(obs$Data)[2],1,69), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1, score = c("obsMean","obsVar","obsSkewness","obsT25","obsT15","obsT98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsMeanJan","obsMeanFeb","obsMeanMar","obsMeanApr","obsMeanMay","obsMeanJun","obsMeanJul","obsMeanAug","obsMeanSep","obsMeanOct","obsMeanNov","obsMeanDec","obsWarmSpell50","obsWarmSpell90","obsMaxWarmSpell","obsColdSpell50","obsColdSpell90","obsMaxColdSpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdT25","prdT15","prdT98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdMeanJan","prdMeanFeb","prdMeanMar","prdMeanApr","prdMeanMay","prdMeanJun","prdMeanJul","prdMeanAug","prdMeanSep","prdMeanOct","prdMeanNov","prdMeanDec","prdWarmSpell50","prdWarmSpell90","prdMaxWarmSpell","prdColdSpell50","prdColdSpell90","prdMaxColdSpell","prdProVarLowFreq")))
      
      date.vec <- as.POSIXlt(obs$Dates$start, tz = "GMT")
      yo <- date.vec$year
      mo <- date.vec$mon+1
      so <- mo
      so[which(!is.na(match(mo, c(12,1,2))))] <- 1
      so[which(!is.na(match(mo, c(3,4,5))))] <- 2
      so[which(!is.na(match(mo, c(6,7,8))))] <- 3
      so[which(!is.na(match(mo, c(9,10,11))))] <- 4
      
      contador <- 1
      # Mean:
      mean.x <- apply(obs$Data, MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- mean.x
      annualMeanObs <- apply(obs$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = so)
      validation[2:5,,1,contador] <- annualMeanObs
      
      contador <- contador+1
      # Variance:
      var.x <- apply(obs$Data, MARGIN = 2, FUN = var, na.rm = TRUE)
      validation[1,,1,contador] <- var.x
      annualMeanObs <- apply(obs$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = var, na.rm = TRUE)}, INDEX = so)
      validation[2:5,,1,contador] <- annualMeanObs
      
      contador <- contador+1
      # Skewness:
      annualMeanObs <- (obs$Data - mean.x) / sqrt(var.x)
      annualMeanObs <- apply(annualMeanObs**3, MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            meanS.x <- apply(obs$Data[indSeason,], MARGIN = 2, FUN = mean, na.rm = TRUE)
            stdS.x <- apply(obs$Data[indSeason,], MARGIN = 2, FUN = sd, na.rm = TRUE)
            annualMeanObs <- (obs$Data[indSeason,] - meanS.x) / stdS.x
            annualMeanObs <- apply(annualMeanObs**3, MARGIN = 2, FUN = mean, na.rm = TRUE)
            validation[s+1,,1,contador] <- annualMeanObs
      }
      
      contador <- contador+1
      # Warm-day frequency
      threshold <- warm.threshold
      annualMeanObs <- apply((obs$Data>threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
      validation[1,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            annualMeanObs <- apply((obs$Data[indSeason,]>threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
            validation[1+s,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      }
      
      contador <- contador+1
      # Cold-day frequency
      threshold <- cold.threshold
      annualMeanObs <- apply((obs$Data<threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
      validation[1,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            annualMeanObs <- apply((obs$Data[indSeason,]<threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
            validation[1+s,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      }
      
      contador <- contador+1
      # 98th percentile
      annualMeanObs <- apply(obs$Data, MARGIN = 2, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanObs <- apply(obs$Data[indSeason,], MARGIN = 2, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
            validation[1+s,,1,contador] <- annualMeanObs
      }
      
      contador <- contador+1
      # Autocorrelation lag 1, 2 and 3:
      annualMeanObs <- apply(obs$Data, MARGIN = 2, FUN = acf, na.action = na.pass, plot = FALSE, lag.max = lag.max)
      for (i in 1:dim(obs$Data)[2]){
            index <- which(annualMeanObs[[i]]$lag %in% c(1:lag.max))
            if (length(index)>0){
                  validation[1,i,1,contador+(0:(length(index)-1))] <- annualMeanObs[[i]]$acf[index]
            }
      }
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanObs <- apply(obs$Data[indSeason,], MARGIN = 2, FUN = acf, na.action = na.pass, plot = FALSE, lag.max = lag.max)
            for (i in 1:dim(obs$Data)[2]){
                  index <- which(annualMeanObs[[i]]$lag %in% c(1:lag.max))
                  if (length(index)>0){
                        validation[s+1,i,1,contador+(0:(length(index)-1))] <- annualMeanObs[[i]]$acf[index]
                  }
            }
      }
      
      contador <- contador+lag.max
      # 20-years Return value:
      prob <- 1/20
      annualMeanObs <- apply(obs$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = max, na.rm = TRUE)}, INDEX = yo)
      for (i in 1:dim(obs$Data)[2]){
            estim <- fgev(annualMeanObs[,i], prob = prob, std.err = FALSE)
            validation[1,i,1,contador] <- estim$param[1]
            estim <- fgev(annualMeanObs[,i], prob = 1 - prob, std.err = FALSE)
            validation[1,i,1,contador+1] <- estim$param[1]
      }
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            annualMeanObs <- apply(obs$Data[indSeason,], MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = max, na.rm = TRUE)}, INDEX = yoS)
            for (i in 1:dim(obs$Data)[2]){
                  estim <- fgev(annualMeanObs[,i], prob = prob, std.err = FALSE)
                  validation[1+s,i,1,contador] <- estim$param[1]
                  estim <- fgev(annualMeanObs[,i], prob = 1 - prob, std.err = FALSE)
                  validation[1+s,i,1,contador+1] <- estim$param[1]
            }
      }
      
      # Annual Cicles:
      annualCicleObs <- apply(obs$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = mo)
      contador <- contador+2
      validation[1,,1,contador] <- apply(annualCicleObs, MARGIN = 2, FUN = min, na.rm = TRUE)
      contador <- contador+1
      validation[1,,1,contador] <- apply(annualCicleObs, MARGIN = 2, FUN = max, na.rm = TRUE)
      contador <- contador+1
      validation[1,,1,contador] <- (apply(annualCicleObs, MARGIN = 2, FUN = max, na.rm = TRUE)-apply(annualCicleObs, MARGIN = 2, FUN = min, na.rm = TRUE))
      contador <- contador+1
      validation[1,,1,contador] <- 100*(apply(annualCicleObs, MARGIN = 2, FUN = max, na.rm = TRUE)-apply(annualCicleObs, MARGIN = 2, FUN = min, na.rm = TRUE))/apply(annualCicleObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      contador <- contador+1
      # Monthly mean
      validation[1,,1,contador+(0:(dim(annualCicleObs)[1]-1))] <- annualCicleObs
      contador <- contador+dim(annualCicleObs)[1]

      # Warm Spell Length Distribution
      threshold <- warm.threshold
      bin.data <- obs$Data
      bin.data[obs$Data > threshold] <- 1
      bin.data[obs$Data <= threshold] <- 0
      tmp <- apply(bin.data, MARGIN = 2, FUN = rle)
      annualMeanObs <- array(data = NA, dim = c(3,dim(obs$Data)[2]))
      for (i in 1:dim(obs$Data)[2]){
            index <- which(tmp[[i]]$values == 1)
            annualMeanObs[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
      yoS <- unique(yo)
      auxWarm <- array(data = NA, dim = c(length(yoS),dim(bin.data)[2]))
      for (y in 1:length(yoS)){
            indYear <- which(yo == yoS[y])
            tmp <- apply(bin.data[indYear,], MARGIN = 2, FUN = rle)
            for (i in 1:dim(bin.data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  auxWarm[y,i] <- max(tmp[[i]]$lengths[index])
            }
      }
      annualMeanObs[3,] <- apply(auxWarm, MARGIN = 2, FUN = median, na.rm = TRUE)
      validation[1,,1,contador+(0:(dim(annualMeanObs)[1]-1))] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            tmp <- apply(bin.data[indSeason,], MARGIN = 2, FUN = rle)
            annualMeanObs <- array(data = NA, dim = c(2,dim(obs$Data)[2]))
            for (i in 1:dim(obs$Data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  annualMeanObs[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
            }
            validation[1+s,,1,contador+c(0,1)] <- annualMeanObs
      }
      contador <- contador+3

      # Cold Spell Length Distribution
      threshold <- cold.threshold
      bin.data <- obs$Data
      bin.data[obs$Data < threshold] <- 1
      bin.data[obs$Data >= threshold] <- 0
      tmp <- apply(bin.data, MARGIN = 2, FUN = rle)
      annualMeanObs <- array(data = NA, dim = c(3,dim(obs$Data)[2]))
      for (i in 1:dim(obs$Data)[2]){
            index <- which(tmp[[i]]$values == 1)
            annualMeanObs[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
      yoS <- unique(yo)
      auxCold <- array(data = NA, dim = c(length(yoS),dim(bin.data)[2]))
      for (y in 1:length(yoS)){
            indYear <- which(yo == yoS[y])
            tmp <- apply(bin.data[indYear,], MARGIN = 2, FUN = rle)
            for (i in 1:dim(bin.data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  auxCold[y,i] <- max(tmp[[i]]$lengths[index])
            }
      }
      annualMeanObs[3,] <- apply(auxCold, MARGIN = 2, FUN = median, na.rm = TRUE)
      validation[1,,1,contador+(0:(dim(annualMeanObs)[1]-1))] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            tmp <- apply(bin.data[indSeason,], MARGIN = 2, FUN = rle)
            annualMeanObs <- array(data = NA, dim = c(2,dim(obs$Data)[2]))
            for (i in 1:dim(obs$Data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  annualMeanObs[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
            }
            validation[1+s,,1,contador+c(0,1)] <- annualMeanObs
      }
      contador <- contador+3
      
      # sign of low pass filtered signal	x	x		signOfLowPassSignal	Does not distinguish between temporal scales
      # dt <- 1
      # smoothingperiod <- 11
      # xAnnual <- apply(obsPr$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yo)
      # wf <- 0.001
      # f <- 1 / smoothingperiod
      # fny <- 1 / (2 * dt)
      # N <- dim(xAnnual)[1]
      # if (floor(N/2) != N/2){
      # N <- N-1
      # }
      # df <- fny / (N / 2)
      # fs <- c(1 : (N / 2), c(N / 2) : 1) * df
      # Hf <- 0.5 + atan(-(fs - f) / wf) / (pi)
      # aux <- apply(xAnnual[1:N,], MARGIN = 2, FUN = fft)
      # xlow <- 1 / N * Re(apply(aux * Hf, MARGIN = 2, fft, inverse = TRUE))
      # xlowanom <- xlow - apply(xlow, MARGIN = 2, FUN = mean, na.rm = TRUE)
      # xbin <- xlowanom
      # xbin[xlowanom > 0] <- 1
      # xbin[xlowanom < 0] <- -1
      
      # proportion of variance in low frequency	x	x		fractionOfLowFrequencyVariance	Does not distinguish between temporal scales
      lowVarPeriod <- lowVarPeriod
      xAnnual <- apply(obs$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yo)
      specVar <- apply(xAnnual, MARGIN = 2, FUN = spec.pgram, na.action = na.exclude, plot = FALSE)
      annualMeanObs <- array(data = NA, dim = dim(obs$Data)[2])
      for (i in 1:dim(obs$Data)[2]){
            T <- 1 / specVar[[i]]$freq
            lowfreqvar <- sum(specVar[[i]]$spec[1 / specVar[[i]]$freq >= lowVarPeriod], na.rm = TRUE)
            totalvar <- sum(specVar[[i]]$spec, na.rm = TRUE)
            annualMeanObs[i] <- lowfreqvar / totalvar
      }
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- yo[indSeason]
            xAnnual <- apply(obs$Data[indSeason,], MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yoS)
            specVar <- apply(xAnnual, MARGIN = 2, FUN = spec.pgram, na.action = na.exclude, plot = FALSE)
            annualMeanObs <- array(data = NA, dim = dim(obs$Data)[2])
            for (i in 1:dim(obs$Data)[2]){
                  T <- 1 / specVar[[i]]$freq
                  lowfreqvar <- sum(specVar[[i]]$spec[1 / specVar[[i]]$freq >= lowVarPeriod], na.rm = TRUE)
                  totalvar <- sum(specVar[[i]]$spec, na.rm = TRUE)
                  annualMeanObs[i] <- lowfreqvar / totalvar
            }
            validation[1+s,,1,contador] <- annualMeanObs
      }
      contador <- contador + 1
      # Cramer Von Misses
      annualMeanObs <- array(data = NA, dim = dim(obs$Data)[2])
      for (i in 1:dim(obs$Data)[2]){
            x <- obs$Data[,i]
            y <- prd$Data[,i]
            seq.all <- range(c(x, y), na.rm = TRUE)
            breaks <- (seq.all[2]-seq.all[1])/(Nbins+1)
            breaks.s <- seq(from = seq.all[1] - breaks, to = seq.all[2] + breaks, by = breaks)
            breaks.r <- seq(from = seq.all[1] - breaks, to = seq.all[2] + breaks, by = breaks)
            term1 <- (length(x) * length(y)) / (length(x) + length(y)) ** 2
            hm.s <- hist(x, breaks = breaks.s, plot = FALSE)
            hm.r <- hist(y, breaks = breaks.r, plot = FALSE)
            fs <- cumsum(hm.s$counts) / length(x)
            gr <- cumsum(hm.r$counts) / length(y)
            term2 <- sum((hm.s$counts + hm.r$counts) * ((fs - gr) ** 2))
            validation[1,i,1,contador] <- term1 * term2
      }
      for (s in 1:4){
            indSeason <- which(mo == s)
            x <- obs$Data[indSeason,i]
            y <- prd$Data[indSeason,i]
            seq.all <- range(c(x, y), na.rm = TRUE)
            breaks <- (seq.all[2]-seq.all[1])/(Nbins+1)
            breaks.s <- seq(from = seq.all[1] - breaks, to = seq.all[2] + breaks, by = breaks)
            breaks.r <- seq(from = seq.all[1] - breaks, to = seq.all[2] + breaks, by = breaks)
            term1 <- (length(x) * length(y)) / (length(x) + length(y)) ** 2
            hm.s <- hist(x, breaks = breaks.s, plot = FALSE)
            hm.r <- hist(y, breaks = breaks.r, plot = FALSE)
            fs <- cumsum(hm.s$counts) / length(x)
            gr <- cumsum(hm.r$counts) / length(y)
            term2 <- sum((hm.s$counts + hm.r$counts) * ((fs - gr) ** 2))
            validation[1+s,i,1,contador] <- term1 * term2
      }
      
      date.vec <- as.POSIXlt(prd$Dates$start, tz = "GMT")
      yo <- date.vec$year
      mo <- date.vec$mon+1
      so <- mo
      so[which(!is.na(match(mo, c(12,1,2))))] <- 1
      so[which(!is.na(match(mo, c(3,4,5))))] <- 2
      so[which(!is.na(match(mo, c(6,7,8))))] <- 3
      so[which(!is.na(match(mo, c(9,10,11))))] <- 4
      contador <- contador + 1
      # Mean:
      mean.x <- apply(prd$Data, MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- mean.x
      annualMeanprd <- apply(prd$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = so)
      validation[2:5,,1,contador] <- annualMeanprd
      
      contador <- contador+1
      # Variance:
      var.x <- apply(prd$Data, MARGIN = 2, FUN = var, na.rm = TRUE)
      validation[1,,1,contador] <- var.x
      annualMeanprd <- apply(prd$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = var, na.rm = TRUE)}, INDEX = so)
      validation[2:5,,1,contador] <- annualMeanprd
      
      contador <- contador+1
      # Skewness:
      annualMeanprd <- (prd$Data - mean.x) / sqrt(var.x)
      annualMeanprd <- apply(annualMeanprd**3, MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- annualMeanprd
      for (s in 1:4){
            indSeason <- which(so == s)
            meanS.x <- apply(prd$Data[indSeason,], MARGIN = 2, FUN = mean, na.rm = TRUE)
            stdS.x <- apply(prd$Data[indSeason,], MARGIN = 2, FUN = sd, na.rm = TRUE)
            annualMeanprd <- (prd$Data[indSeason,] - meanS.x) / stdS.x
            annualMeanprd <- apply(annualMeanprd**3, MARGIN = 2, FUN = mean, na.rm = TRUE)
            validation[s+1,,1,contador] <- annualMeanprd
      }
      
      contador <- contador+1
      # Warm-day frequency
      threshold <- warm.threshold
      annualMeanprd <- apply((prd$Data>threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
      validation[1,,1,contador] <- apply(annualMeanprd, MARGIN = 2, FUN = mean, na.rm = TRUE)
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            annualMeanprd <- apply((prd$Data[indSeason,]>threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
            validation[1+s,,1,contador] <- apply(annualMeanprd, MARGIN = 2, FUN = mean, na.rm = TRUE)
      }
      
      contador <- contador+1
      # Cold-day frequency
      threshold <- cold.threshold
      annualMeanprd <- apply((prd$Data<threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
      validation[1,,1,contador] <- apply(annualMeanprd, MARGIN = 2, FUN = mean, na.rm = TRUE)
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            annualMeanprd <- apply((prd$Data[indSeason,]<threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
            validation[1+s,,1,contador] <- apply(annualMeanprd, MARGIN = 2, FUN = mean, na.rm = TRUE)
      }
      
      contador <- contador+1
      # 98th percentile
      annualMeanprd <- apply(prd$Data, MARGIN = 2, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
      validation[1,,1,contador] <- annualMeanprd
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanprd <- apply(prd$Data[indSeason,], MARGIN = 2, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
            validation[1+s,,1,contador] <- annualMeanprd
      }
      
      contador <- contador+1
      # Autocorrelation lag 1, 2 and 3:
      annualMeanprd <- apply(prd$Data, MARGIN = 2, FUN = acf, na.action = na.pass, plot = FALSE, lag.max = lag.max)
      for (i in 1:dim(prd$Data)[2]){
            index <- which(annualMeanprd[[i]]$lag %in% c(1:lag.max))
            if (length(index)>0){
                  validation[1,i,1,contador+(0:(length(index)-1))] <- annualMeanprd[[i]]$acf[index]
            }
      }
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanprd <- apply(prd$Data[indSeason,], MARGIN = 2, FUN = acf, na.action = na.pass, plot = FALSE, lag.max = lag.max)
            for (i in 1:dim(prd$Data)[2]){
                  index <- which(annualMeanprd[[i]]$lag %in% c(1:lag.max))
                  if (length(index)>0){
                        validation[s+1,i,1,contador+(0:(length(index)-1))] <- annualMeanprd[[i]]$acf[index]
                  }
            }
      }
      
      contador <- contador+lag.max
      # 20-years Return value:
      prob <- 1/20
      annualMeanprd <- apply(prd$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = max, na.rm = TRUE)}, INDEX = yo)
      for (i in 1:dim(prd$Data)[2]){
            estim <- fgev(annualMeanprd[,i], prob = prob, std.err = FALSE)
            validation[1,i,1,contador] <- estim$param[1]
            estim <- fgev(annualMeanprd[,i], prob = 1 - prob, std.err = FALSE)
            validation[1,i,1,contador+1] <- estim$param[1]
      }
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            annualMeanprd <- apply(prd$Data[indSeason,], MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = max, na.rm = TRUE)}, INDEX = yoS)
            for (i in 1:dim(prd$Data)[2]){
                  estim <- fgev(annualMeanprd[,i], prob = prob, std.err = FALSE)
                  validation[1+s,i,1,contador] <- estim$param[1]
                  estim <- fgev(annualMeanprd[,i], prob = 1 - prob, std.err = FALSE)
                  validation[1+s,i,1,contador+1] <- estim$param[1]
            }
      }
      
      # Annual Cicles:
      annualCicleprd <- apply(prd$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = mo)
      contador <- contador+2
      validation[1,,1,contador] <- apply(annualCicleprd, MARGIN = 2, FUN = min, na.rm = TRUE)
      contador <- contador+1
      validation[1,,1,contador] <- apply(annualCicleprd, MARGIN = 2, FUN = max, na.rm = TRUE)
      contador <- contador+1
      validation[1,,1,contador] <- (apply(annualCicleprd, MARGIN = 2, FUN = max, na.rm = TRUE)-apply(annualCicleprd, MARGIN = 2, FUN = min, na.rm = TRUE))
      contador <- contador+1
      validation[1,,1,contador] <- 100*(apply(annualCicleprd, MARGIN = 2, FUN = max, na.rm = TRUE)-apply(annualCicleprd, MARGIN = 2, FUN = min, na.rm = TRUE))/apply(annualCicleprd, MARGIN = 2, FUN = mean, na.rm = TRUE)
      contador <- contador+1
      # Monthly mean
      validation[1,,1,contador+(0:(dim(annualCicleprd)[1]-1))] <- annualCicleprd
      contador <- contador+dim(annualCicleprd)[1]
      
      # Warm Spell Length Distribution
      threshold <- warm.threshold
      bin.data <- prd$Data
      bin.data[prd$Data > threshold] <- 1
      bin.data[prd$Data <= threshold] <- 0
      tmp <- apply(bin.data, MARGIN = 2, FUN = rle)
      annualMeanprd <- array(data = NA, dim = c(3,dim(prd$Data)[2]))
      for (i in 1:dim(prd$Data)[2]){
            index <- which(tmp[[i]]$values == 1)
            annualMeanprd[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
      yoS <- unique(yo)
      auxWarm <- array(data = NA, dim = c(length(yoS),dim(bin.data)[2]))
      for (y in 1:length(yoS)){
            indYear <- which(yo == yoS[y])
            tmp <- apply(bin.data[indYear,], MARGIN = 2, FUN = rle)
            for (i in 1:dim(bin.data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  auxWarm[y,i] <- max(tmp[[i]]$lengths[index])
            }
      }
      annualMeanprd[3,] <- apply(auxWarm, MARGIN = 2, FUN = median, na.rm = TRUE)
      validation[1,,1,contador+(0:(dim(annualMeanprd)[1]-1))] <- annualMeanprd
      for (s in 1:4){
            indSeason <- which(so == s)
            tmp <- apply(bin.data[indSeason,], MARGIN = 2, FUN = rle)
            annualMeanprd <- array(data = NA, dim = c(2,dim(prd$Data)[2]))
            for (i in 1:dim(prd$Data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  annualMeanprd[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
            }
            validation[1+s,,1,contador+c(0,1)] <- annualMeanprd
      }
      contador <- contador+3
      
      # Cold Spell Length Distribution
      threshold <- cold.threshold
      bin.data <- prd$Data
      bin.data[prd$Data < threshold] <- 1
      bin.data[prd$Data >= threshold] <- 0
      tmp <- apply(bin.data, MARGIN = 2, FUN = rle)
      annualMeanprd <- array(data = NA, dim = c(3,dim(prd$Data)[2]))
      for (i in 1:dim(prd$Data)[2]){
            index <- which(tmp[[i]]$values == 1)
            annualMeanprd[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
      yoS <- unique(yo)
      auxCold <- array(data = NA, dim = c(length(yoS),dim(bin.data)[2]))
      for (y in 1:length(yoS)){
            indYear <- which(yo == yoS[y])
            tmp <- apply(bin.data[indYear,], MARGIN = 2, FUN = rle)
            for (i in 1:dim(bin.data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  auxCold[y,i] <- max(tmp[[i]]$lengths[index])
            }
      }
      annualMeanprd[3,] <- apply(auxCold, MARGIN = 2, FUN = median, na.rm = TRUE)
      validation[1,,1,contador+(0:(dim(annualMeanprd)[1]-1))] <- annualMeanprd
      for (s in 1:4){
            indSeason <- which(so == s)
            tmp <- apply(bin.data[indSeason,], MARGIN = 2, FUN = rle)
            annualMeanprd <- array(data = NA, dim = c(2,dim(prd$Data)[2]))
            for (i in 1:dim(prd$Data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  annualMeanprd[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
            }
            validation[1+s,,1,contador+c(0,1)] <- annualMeanprd
      }
      contador <- contador+3
      
      # sign of low pass filtered signal      x	x		signOfLowPassSignal	Does not distinguish between temporal scales
      # dt <- 1
      # smoothingperiod <- 11
      # xAnnual <- apply(prdPr$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yo)
      # wf <- 0.001
      # f <- 1 / smoothingperiod
      # fny <- 1 / (2 * dt)
      # N <- dim(xAnnual)[1]
      # if (floor(N/2) != N/2){
      # N <- N-1
      # }
      # df <- fny / (N / 2)
      # fs <- c(1 : (N / 2), c(N / 2) : 1) * df
      # Hf <- 0.5 + atan(-(fs - f) / wf) / (pi)
      # aux <- apply(xAnnual[1:N,], MARGIN = 2, FUN = fft)
      # xlow <- 1 / N * Re(apply(aux * Hf, MARGIN = 2, fft, inverse = TRUE))
      # xlowanom <- xlow - apply(xlow, MARGIN = 2, FUN = mean, na.rm = TRUE)
      # xbin <- xlowanom
      # xbin[xlowanom > 0] <- 1
      # xbin[xlowanom < 0] <- -1
      
      # proportion of variance in low frequency	x	x		fractionOfLowFrequencyVariance	Does not distinguish between temporal scales
      lowVarPeriod <- lowVarPeriod
      xAnnual <- apply(prd$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yo)
      specVar <- apply(xAnnual, MARGIN = 2, FUN = spec.pgram, na.action = na.exclude, plot = FALSE)
      annualMeanprd <- array(data = NA, dim = dim(prd$Data)[2])
      for (i in 1:dim(prd$Data)[2]){
            T <- 1 / specVar[[i]]$freq
            lowfreqvar <- sum(specVar[[i]]$spec[1 / specVar[[i]]$freq >= lowVarPeriod], na.rm = TRUE)
            totalvar <- sum(specVar[[i]]$spec, na.rm = TRUE)
            annualMeanprd[i] <- lowfreqvar / totalvar
      }
      validation[1,,1,contador] <- annualMeanprd
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- yo[indSeason]
            xAnnual <- apply(prd$Data[indSeason,], MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yoS)
            specVar <- apply(xAnnual, MARGIN = 2, FUN = spec.pgram, na.action = na.exclude, plot = FALSE)
            annualMeanprd <- array(data = NA, dim = dim(prd$Data)[2])
            for (i in 1:dim(prd$Data)[2]){
                  T <- 1 / specVar[[i]]$freq
                  lowfreqvar <- sum(specVar[[i]]$spec[1 / specVar[[i]]$freq >= lowVarPeriod], na.rm = TRUE)
                  totalvar <- sum(specVar[[i]]$spec, na.rm = TRUE)
                  annualMeanprd[i] <- lowfreqvar / totalvar
            }
            validation[1+s,,1,contador] <- annualMeanprd
      }
      return(validation)
}
# End
