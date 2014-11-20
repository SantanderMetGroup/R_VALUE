validationPrecip <- function(obs, prd, pr.threshold = 1, r10.threshold = 10, lag.max = 3, lowVarPeriod = 1, Nbins = 100) {

      validation <- array(data = NA, dim = c(5,dim(obs$Data)[2],1,79), dimnames = list(season = c("Annual","DJF","MAM","JJA","SON"), stations = dimnames(obs$xyCoords)[[1]], realizations = 1, score = c("obsMean","obsVar","obsSkewness","obsR01","obsR10","obsR10p","obsR98p","obsACF1","obsACF2","obsACF3","obsRV20lb","obsRV20ub","obsWWProb","obsWDProb","obsDWProb","obsDDProb","obsMinAC","obsMaxAC","obsAmpAC","obsRelAmpAC","obsMeanJan","obsMeanFeb","obsMeanMar","obsMeanApr","obsMeanMay","obsMeanJun","obsMeanJul","obsMeanAug","obsMeanSep","obsMeanOct","obsMeanNov","obsMeanDec","obsWetSpell50","obsWetSpell90","obsMaxWetSpell","obsDrySpell50","obsDrySpell90","obsMaxDrySpell","obsProVarLowFreq","cmIndex","prdMean","prdVar","prdSkewness","prdR01","prdR10","prdR10p","prdR98p","prdACF1","prdACF2","prdACF3","prdRV20lb","prdRV20ub","prdWWProb","prdWDProb","prdDWProb","prdDDProb","prdMinAC","prdMaxAC","prdAmpAC","prdRelAmpAC","prdMeanJan","prdMeanFeb","prdMeanMar","prdMeanApr","prdMeanMay","prdMeanJun","prdMeanJul","prdMeanAug","prdMeanSep","prdMeanOct","prdMeanNov","prdMeanDec","prdWetSpell50","prdWetSpell90","prdMaxWetSpell","prdDrySpell50","prdDrySpell90","prdDryWetSpell","prdProVarLowFreq")))
      # pr.threshold <- 1
      # r10.threshold <- 10
      # lag.max <- 3
      # lowVarPeriod <- 11
      # Nbins <- 100
      
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
      # Wet-day frequency
      threshold <- pr.threshold
      annualMeanObs <- apply((obs$Data>=threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
      validation[1,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            annualMeanObs <- apply((obs$Data[indSeason,]>=threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
            validation[1+s,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      }
      
      contador <- contador+1
      # Heavy rainy days frequency
      threshold <- r10.threshold
      annualMeanObs <- apply((obs$Data>threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
      validation[1,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            annualMeanObs <- apply((obs$Data[indSeason,]>threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
            validation[1+s,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      }
      
      contador <- contador+1
      # Amount falling in heavy rainy days
      threshold <- r10.threshold
      aux <- obs$Data
      aux[obs$Data<=threshold] <- NA
      annualMeanObs <- apply(aux, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
      validation[1,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            aux <- obs$Data[indSeason,]
            aux[aux<=threshold] <- NA
            annualMeanObs <- apply(aux, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
            validation[1+s,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      }
      
      contador <- contador+1
      # 98th percentile
      threshold <- pr.threshold
      aux <- obs$Data
      aux[obs$Data<threshold] <- NA
      annualMeanObs <- apply(aux, MARGIN = 2, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            aux <- obs$Data[indSeason,]
            aux[aux<threshold] <- NA
            annualMeanObs <- apply(aux, MARGIN = 2, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
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
      
      # Transition probabilities
      contador <- contador+2
      # wet-wet
      threshold <- pr.threshold
      annualMeanObs <- apply((obs$Data[1:(dim(obs$Data)[1]-1),] >= threshold)*(obs$Data[2:dim(obs$Data)[1],] >= threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanObs <- apply((obs$Data[indSeason[1:(length(indSeason)-1)],] >= threshold)*(obs$Data[indSeason[2:length(indSeason)],] >= threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
            validation[1+s,,1,contador] <- annualMeanObs
      }
      
      contador <- contador+1
      # wet-dry
      annualMeanObs <- apply((obs$Data[1:(dim(obs$Data)[1]-1),] >= threshold)*(obs$Data[2:dim(obs$Data)[1],] < threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanObs <- apply((obs$Data[indSeason[1:(length(indSeason)-1)],] >= threshold)*(obs$Data[indSeason[2:length(indSeason)],] < threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
            validation[1+s,,1,contador] <- annualMeanObs
      }
      
      contador <- contador+1
      # dry-wet
      annualMeanObs <- apply((obs$Data[1:(dim(obs$Data)[1]-1),] < threshold)*(obs$Data[2:dim(obs$Data)[1],] >= threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanObs <- apply((obs$Data[indSeason[1:(length(indSeason)-1)],] < threshold)*(obs$Data[indSeason[2:length(indSeason)],] >= threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
            validation[1+s,,1,contador] <- annualMeanObs
      }
      
      contador <- contador+1
      # dry-dry
      annualMeanObs <- apply((obs$Data[1:(dim(obs$Data)[1]-1),] < threshold)*(obs$Data[2:dim(obs$Data)[1],] < threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanObs <- apply((obs$Data[indSeason[1:(length(indSeason)-1)],] < threshold)*(obs$Data[indSeason[2:length(indSeason)],] < threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
            validation[1+s,,1,contador] <- annualMeanObs
      }
      
      # Annual Cicles:
      annualCicleObs <- apply(obs$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = mo)
      contador <- contador+1
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
      # Wet/Dry Spell Length Distribution
      bin.data <- obs$Data
      bin.data[obs$Data >= 1] <- 1
      bin.data[obs$Data < 1] <- 0
      tmp <- apply(bin.data, MARGIN = 2, FUN = rle)
      annualMeanObs <- array(data = NA, dim = c(6,dim(obs$Data)[2]))
      for (i in 1:dim(obs$Data)[2]){
            index <- which(tmp[[i]]$values == 1)
            annualMeanObs[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
            index <- which(tmp[[i]]$values == 0)
            annualMeanObs[4:5,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
      yoS <- unique(yo)
      auxWet <- array(data = NA, dim = c(length(yoS),dim(bin.data)[2]))
      auxDry <- array(data = NA, dim = c(length(yoS),dim(bin.data)[2]))
      for (y in 1:length(yoS)){
            indYear <- which(yo == yoS[y])
            tmp <- apply(bin.data[indYear,], MARGIN = 2, FUN = rle)
            for (i in 1:dim(bin.data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  auxWet[y,i] <- max(tmp[[i]]$lengths[index])
                  index <- which(tmp[[i]]$values == 0)
                  auxDry[y,i] <- max(tmp[[i]]$lengths[index])
            }
      }
      annualMeanObs[3,] <- apply(auxWet, MARGIN = 2, FUN = median, na.rm = TRUE)
      annualMeanObs[6,] <- apply(auxDry, MARGIN = 2, FUN = median, na.rm = TRUE)
      validation[1,,1,contador+(0:(dim(annualMeanObs)[1]-1))] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            tmp <- apply(bin.data[indSeason,], MARGIN = 2, FUN = rle)
            annualMeanObs <- array(data = NA, dim = c(4,dim(obs$Data)[2]))
            for (i in 1:dim(obs$Data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  annualMeanObs[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
                  index <- which(tmp[[i]]$values == 0)
                  annualMeanObs[3:4,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
            }
            validation[1+s,,1,contador+c(0,1,3,4)] <- annualMeanObs
      }
      contador <- contador+6
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
      annualMeanObs <- apply(prd$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = so)
      validation[2:5,,1,contador] <- annualMeanObs
      
      contador <- contador+1
      # Variance:
      var.x <- apply(prd$Data, MARGIN = 2, FUN = var, na.rm = TRUE)
      validation[1,,1,contador] <- var.x
      annualMeanObs <- apply(prd$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = var, na.rm = TRUE)}, INDEX = so)
      validation[2:5,,1,contador] <- annualMeanObs
      
      contador <- contador+1
      # Skewness:
      annualMeanObs <- (prd$Data - mean.x) / sqrt(var.x)
      annualMeanObs <- apply(annualMeanObs**3, MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            meanS.x <- apply(prd$Data[indSeason,], MARGIN = 2, FUN = mean, na.rm = TRUE)
            stdS.x <- apply(prd$Data[indSeason,], MARGIN = 2, FUN = sd, na.rm = TRUE)
            annualMeanObs <- (prd$Data[indSeason,] - meanS.x) / stdS.x
            annualMeanObs <- apply(annualMeanObs**3, MARGIN = 2, FUN = mean, na.rm = TRUE)
            validation[s+1,,1,contador] <- annualMeanObs
      }
      
      contador <- contador+1
      # Wet-day frequency
      threshold <- pr.threshold
      annualMeanObs <- apply((prd$Data>=threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
      validation[1,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            annualMeanObs <- apply((prd$Data[indSeason,]>=threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
            validation[1+s,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      }
      
      contador <- contador+1
      # Heavy rainy days frequency
      threshold <- r10.threshold
      annualMeanObs <- apply((prd$Data>threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
      validation[1,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            annualMeanObs <- apply((prd$Data[indSeason,]>threshold), MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
            validation[1+s,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      }
      
      contador <- contador+1
      # Amount falling in heavy rainy days
      threshold <- r10.threshold
      aux <- prd$Data
      aux[prd$Data<=threshold] <- NA
      annualMeanObs <- apply(aux, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
      validation[1,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            aux <- prd$Data[indSeason,]
            aux[aux<=threshold] <- NA
            annualMeanObs <- apply(aux, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
            validation[1+s,,1,contador] <- apply(annualMeanObs, MARGIN = 2, FUN = mean, na.rm = TRUE)
      }
      
      contador <- contador+1
      # 98th percentile
      threshold <- pr.threshold
      aux <- prd$Data
      aux[prd$Data<threshold] <- NA
      annualMeanObs <- apply(aux, MARGIN = 2, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            aux <- prd$Data[indSeason,]
            aux[aux<threshold] <- NA
            annualMeanObs <- apply(aux, MARGIN = 2, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
            validation[1+s,,1,contador] <- annualMeanObs
      }
      
      contador <- contador+1
      # Autocorrelation lag 1, 2 and 3:
      annualMeanObs <- apply(prd$Data, MARGIN = 2, FUN = acf, na.action = na.pass, plot = FALSE, lag.max = lag.max)
      for (i in 1:dim(prd$Data)[2]){
            index <- which(annualMeanObs[[i]]$lag %in% c(1:lag.max))
            if (length(index)>0){
                  validation[1,i,1,contador+(0:(length(index)-1))] <- annualMeanObs[[i]]$acf[index]
            }
      }
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanObs <- apply(prd$Data[indSeason,], MARGIN = 2, FUN = acf, na.action = na.pass, plot = FALSE, lag.max = lag.max)
            for (i in 1:dim(prd$Data)[2]){
                  index <- which(annualMeanObs[[i]]$lag %in% c(1:lag.max))
                  if (length(index)>0){
                        validation[s+1,i,1,contador+(0:(length(index)-1))] <- annualMeanObs[[i]]$acf[index]
                  }
            }
      }
      
      contador <- contador+lag.max
      # 20-years Return value:
      prob <- 1/20
      annualMeanObs <- apply(prd$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = max, na.rm = TRUE)}, INDEX = yo)
      for (i in 1:dim(prd$Data)[2]){
            estim <- fgev(annualMeanObs[,i], prob = prob, std.err = FALSE)
            validation[1,i,1,contador] <- estim$param[1]
            estim <- fgev(annualMeanObs[,i], prob = 1 - prob, std.err = FALSE)
            validation[1,i,1,contador+1] <- estim$param[1]
      }
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- date.vec$year[indSeason]
            annualMeanObs <- apply(prd$Data[indSeason,], MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = max, na.rm = TRUE)}, INDEX = yoS)
            for (i in 1:dim(prd$Data)[2]){
                  estim <- fgev(annualMeanObs[,i], prob = prob, std.err = FALSE)
                  validation[1+s,i,1,contador] <- estim$param[1]
                  estim <- fgev(annualMeanObs[,i], prob = 1 - prob, std.err = FALSE)
                  validation[1+s,i,1,contador+1] <- estim$param[1]
            }
      }
      
      # Transition probabilities
      contador <- contador+2
      # wet-wet
      threshold <- pr.threshold
      annualMeanObs <- apply((prd$Data[1:(dim(prd$Data)[1]-1),] >= threshold)*(prd$Data[2:dim(prd$Data)[1],] >= threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanObs <- apply((prd$Data[indSeason[1:(length(indSeason)-1)],] >= threshold)*(prd$Data[indSeason[2:length(indSeason)],] >= threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
            validation[1+s,,1,contador] <- annualMeanObs
      }
      
      contador <- contador+1
      # wet-dry
      annualMeanObs <- apply((prd$Data[1:(dim(prd$Data)[1]-1),] >= threshold)*(prd$Data[2:dim(prd$Data)[1],] < threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanObs <- apply((prd$Data[indSeason[1:(length(indSeason)-1)],] >= threshold)*(prd$Data[indSeason[2:length(indSeason)],] < threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
            validation[1+s,,1,contador] <- annualMeanObs
      }
      
      contador <- contador+1
      # dry-wet
      annualMeanObs <- apply((prd$Data[1:(dim(prd$Data)[1]-1),] < threshold)*(prd$Data[2:dim(prd$Data)[1],] >= threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanObs <- apply((prd$Data[indSeason[1:(length(indSeason)-1)],] < threshold)*(prd$Data[indSeason[2:length(indSeason)],] >= threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
            validation[1+s,,1,contador] <- annualMeanObs
      }
      
      contador <- contador+1
      # dry-dry
      annualMeanObs <- apply((prd$Data[1:(dim(prd$Data)[1]-1),] < threshold)*(prd$Data[2:dim(prd$Data)[1],] < threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            annualMeanObs <- apply((prd$Data[indSeason[1:(length(indSeason)-1)],] < threshold)*(prd$Data[indSeason[2:length(indSeason)],] < threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
            validation[1+s,,1,contador] <- annualMeanObs
      }
      
      # Annual Cicles:
      annualCicleObs <- apply(prd$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = mo)
      contador <- contador+1
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
      # Wet/Dry Spell Length Distribution
      bin.data <- prd$Data
      bin.data[prd$Data >= 1] <- 1
      bin.data[prd$Data < 1] <- 0
      tmp <- apply(bin.data, MARGIN = 2, FUN = rle)
      annualMeanObs <- array(data = NA, dim = c(6,dim(prd$Data)[2]))
      for (i in 1:dim(prd$Data)[2]){
            index <- which(tmp[[i]]$values == 1)
            annualMeanObs[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
            index <- which(tmp[[i]]$values == 0)
            annualMeanObs[4:5,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
      yoS <- unique(yo)
      auxWet <- array(data = NA, dim = c(length(yoS),dim(bin.data)[2]))
      auxDry <- array(data = NA, dim = c(length(yoS),dim(bin.data)[2]))
      for (y in 1:length(yoS)){
            indYear <- which(yo == yoS[y])
            tmp <- apply(bin.data[indYear,], MARGIN = 2, FUN = rle)
            for (i in 1:dim(bin.data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  auxWet[y,i] <- max(tmp[[i]]$lengths[index])
                  index <- which(tmp[[i]]$values == 0)
                  auxDry[y,i] <- max(tmp[[i]]$lengths[index])
            }
      }
      annualMeanObs[3,] <- apply(auxWet, MARGIN = 2, FUN = median, na.rm = TRUE)
      annualMeanObs[6,] <- apply(auxDry, MARGIN = 2, FUN = median, na.rm = TRUE)
      validation[1,,1,contador+(0:(dim(annualMeanObs)[1]-1))] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            tmp <- apply(bin.data[indSeason,], MARGIN = 2, FUN = rle)
            annualMeanObs <- array(data = NA, dim = c(4,dim(prd$Data)[2]))
            for (i in 1:dim(prd$Data)[2]){
                  index <- which(tmp[[i]]$values == 1)
                  annualMeanObs[1:2,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
                  index <- which(tmp[[i]]$values == 0)
                  annualMeanObs[3:4,i] <- quantile(tmp[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
            }
            validation[1+s,,1,contador+c(0,1,3,4)] <- annualMeanObs
      }
      contador <- contador+6
      # sign of low pass filtered signal	x	x		signOfLowPassSignal	Does not distinguish between temporal scales
      # dt <- 1
      # smoothingperiod <- 11
      # xAnnual <- apply(prd$Data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yo)
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
      annualMeanObs <- array(data = NA, dim = dim(prd$Data)[2])
      for (i in 1:dim(prd$Data)[2]){
            T <- 1 / specVar[[i]]$freq
            lowfreqvar <- sum(specVar[[i]]$spec[1 / specVar[[i]]$freq >= lowVarPeriod], na.rm = TRUE)
            totalvar <- sum(specVar[[i]]$spec, na.rm = TRUE)
            annualMeanObs[i] <- lowfreqvar / totalvar
      }
      validation[1,,1,contador] <- annualMeanObs
      for (s in 1:4){
            indSeason <- which(so == s)
            yoS <- yo[indSeason]
            xAnnual <- apply(prd$Data[indSeason,], MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yoS)
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
      return(validation)
}
# End
