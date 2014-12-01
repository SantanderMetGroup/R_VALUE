#' @title datevec
#' @author S. Herrera
#' @export
#' @keywords internal

getVectorialDates <- function(obj){
      date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
      vectorialDates <- array(data = NA, dim = c(length(date.vec),3)) 
      vectorialDates[,1] <- date.vec$year
      vectorialDates[,2] <- date.vec$mon+1
      vectorialDates[,3] <- date.vec$mon+1
      vectorialDates[which(!is.na(match(vectorialDates[,2], c(12,1,2)))),3] <- 1
      vectorialDates[which(!is.na(match(vectorialDates[,2], c(3,4,5)))),3] <- 2
      vectorialDates[which(!is.na(match(vectorialDates[,2], c(6,7,8)))),3] <- 3
      vectorialDates[which(!is.na(match(vectorialDates[,2], c(9,10,11)))),3] <- 4
      return(vectorialDates)
}

#' @title Sum
#' @description Get annual and seasonal sum from a station or field object
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch} and J. Bedia
#' @export
#' @keywords internal

# Sum:
getSum <- function(data, MARGIN){
      meanObj <- apply(data, MARGIN = MARGIN, FUN = sum, na.rm = TRUE)
      return(meanObj)
}

#' @title Mean
#' @description Get annual and seasonal mean from a station or field object
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch} and J. Bedia
#' @export
#' @keywords internal

# Mean:
getMean <- function(data, MARGIN){
      meanObj <- apply(data, MARGIN = MARGIN, FUN = mean, na.rm = TRUE)
      return(meanObj)
}

#' @title Get annual and seasonal variance from a station or field object
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch} and J. Bedia
#' @export
#' @keywords internal

# Variance:
getVar <- function(data, MARGIN){
      meanObj <- apply(data, MARGIN = MARGIN, FUN = var, na.rm = TRUE)
      return(meanObj)
}

#' @title Get annual and seasonal skewness from a station or field object
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch} and J. Bedia
#' @export
#' @keywords internal

# Skewness:
getSkew <- function(data, MARGIN){
      mean.x <- apply(data, MARGIN = MARGIN, FUN = mean, na.rm = TRUE)
      std.x <- apply(data, MARGIN = MARGIN, FUN = sd, na.rm = TRUE)
      auxObj <- (data - mean.x) / std.x
      auxObj <- apply(auxObj**3, MARGIN = MARGIN, FUN = mean, na.rm = TRUE)
      meanObj <- auxObj
      return(meanObj)
}

#' @title Get annual and seasonal amount of days above of a predefined threshold from a station or field object
#' @author Dougals Maraun \email{dmaraun@@geomar.de} and J. Bedia
#' @export
#' @keywords internal

# Above-threshold frequency
getFreqGT <- function(data, threshold, MARGIN = NULL){
      meanObj <- apply((data>threshold), MARGIN = MARGIN, FUN = sum, na.rm = TRUE)
      return(meanObj)
}

#' @title Get annual and seasonal amount of days greater or equal than a predefined threshold from a station or field object
#' @description Function to calculate the average number of days/season  with measurable precipitation (e.g. precipitation > 1mm).
#' @author Elke Hertig \email{elke.hertig@@geo.uni-augsburg.de} and J. Bedia
#' @export
#' @keywords internal

# Above-equal threshold frequency
getFreqGET <- function(data, threshold, MARGIN = NULL){
      meanObj <- apply((data>=threshold), MARGIN = MARGIN, FUN = sum, na.rm = TRUE)
      return(meanObj)
}

#' @title Get annual and seasonal amount of days below of a predefined threshold from a station or field object
#' @author Dougals Maraun \email{dmaraun@@geomar.de} and J. Bedia
#' @export
#' @keywords internal

# Below-threshold frequency
getFreqLT <- function(data, threshold, MARGIN = NULL){
      meanObj <- apply((data<threshold), MARGIN = MARGIN, FUN = sum, na.rm = TRUE)
      return(meanObj)
}

#' @title 98th percentile
#' @description Get annual and seasonal 98th percentile from a station or field object
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg} and J. Bedia
#' @export
#' @keywords internal

# 98th percentile
get98th <- function(data, MARGIN){
      meanObj <- apply(data, MARGIN = MARGIN, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
      return(meanObj)
}

#' @title Autocorrelation
#' @description Get annual and seasonal Autocorrelation at pre-defined lags from a station or field object
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg}, S. Herrera and J. Bedia
#' @export
#' @keywords internal


# Autocorrelation lag 1, 2 and 3:
getACF <- function(data, lag.max = 3){
      meanObj <- array(dim = c(1,dim(data)[2],1,lag.max))
      mean.x <- apply(data, MARGIN = 2, FUN = acf, na.action = na.pass, plot = FALSE, lag.max = lag.max)
      for (i in 1:dim(data)[2]){
            index <- which(mean.x[[i]]$lag %in% c(1:lag.max))
            if (length(index)>0){
                  meanObj[1,i,1,1:length(index)] <- mean.x[[i]]$acf[index]
            }
      }
      return(meanObj)
}

#' @title Return Value
#' @description Get annual and seasonal 1/prob return value (left/right tail) from a station or field object. Require package - evd
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg} and J. Bedia
#' @export
#' @keywords internal
#' @importFrom evd fgev

# 20-years Return value:

getReturnValue <- function(data, prob, INDEX = 1:dim(data)[1]){
      meanObj <- array(data = NA, dim = c(1,dim(data)[2],1,2))
      mean.x <- apply(data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = max, na.rm = TRUE)}, INDEX = INDEX)
      for (i in 1:dim(data)[2]){
        if (any(is.finite(mean.x[,i]))){
            estim <- fgev(mean.x[which(is.finite(mean.x[,i])),i], prob = prob, std.err = FALSE)
            meanObj[1,i,1,1] <- estim$param[1]
            estim <- fgev(mean.x[which(is.finite(mean.x[,i])),i], prob = 1 - prob, std.err = FALSE)
            meanObj[1,i,1,2] <- estim$param[1]
        }
      }
      return(meanObj)
}

#' @title Annual Cycle
#' @description Function to compute VALUE indices for mean annual cycle
#' @author Sven Kotlarski \email{sven.kotlarski@@env.ethz.ch}, J. Bedia and S. Herrera
#' @export
#' @keywords internal

# Annual Cicles:
getAnnualCicle <- function(data, INDEX = 1:dim(data)[1]){
      meanObj <- array(data = NA, dim = c(1,dim(data)[2],1,4))
      mean.x <- apply(data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = INDEX)
      meanObj[1,,1,1] <- apply(mean.x, MARGIN = 2, FUN = min, na.rm = TRUE)
      meanObj[1,,1,2] <- apply(mean.x, MARGIN = 2, FUN = max, na.rm = TRUE)
      meanObj[1,,1,3] <- (apply(mean.x, MARGIN = 2, FUN = max, na.rm = TRUE)-apply(mean.x, MARGIN = 2, FUN = min, na.rm = TRUE))
      meanObj[1,,1,4] <- 100*(apply(mean.x, MARGIN = 2, FUN = max, na.rm = TRUE)-apply(mean.x, MARGIN = 2, FUN = min, na.rm = TRUE))/apply(mean.x, MARGIN = 2, FUN = mean, na.rm = TRUE)
      return(meanObj)
}

#' @title Spell length distribution
#' @description This function calculates pre-defined quantiles of a spell-distribution and the median of annual maximum consecutive spells
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg} and J. Bedia and S. Herrera
#' @export
#' @keywords internal

# Above Spell Length Distribution
getGTsld <- function(data, threshold, INDEX = 1:dim(data)[1]){
      yoS <- unique(INDEX)
      bin.data <- data
      bin.data[data > threshold] <- 1
      bin.data[data <= threshold] <- 0
      meanObj <- array(data = NA, dim = c(1,dim(data)[2],1,3))
      mean.x <- apply(bin.data, MARGIN = 2, FUN = rle)
      for (i in 1:dim(data)[2]){
            index <- which(mean.x[[i]]$values == 1)
            meanObj[1,i,1,1:2] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
      aux <- array(data = NA, dim = c(length(yoS),dim(data)[2]))
      for (y in 1:length(yoS)){
            indYear <- which(INDEX == yoS[y])
            if (length(indYear)>0){
                  indices <- rep(list(bquote()), length(dim(data)))
                  for (d in 1:length(dim(data))){
                        indices[[d]] <- 1:dim(data)[d]
                  }
                  indices[[1]] <- indYear
                  callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
                  mean.x <- apply(eval(callObj), MARGIN = 2, FUN = rle)
                  for (i in 1:dim(data)[2]){
                        index <- which(mean.x[[i]]$values == 1)
                        aux[y,i] <- max(mean.x[[i]]$lengths[index])
                  }
            }
      }
      meanObj[1,,1,3] <- apply(aux, MARGIN = 2, FUN = median, na.rm = TRUE)
      return(meanObj)
}

#' @title Get annual and seasonal Below of a predefined threshold Spell Length Distribution from a station or field object
#' @export
#' @keywords internal

# Below Spell Length Distribution
getLTsld <- function(data, threshold, INDEX = 1:dim(data)[1]){
      yoS <- unique(INDEX)
      bin.data <- data
      bin.data[data < threshold] <- 1
      bin.data[data >= threshold] <- 0
      meanObj <- array(data = NA, dim = c(1,dim(data)[2],1,3))
      mean.x <- apply(bin.data, MARGIN = 2, FUN = rle)
      for (i in 1:dim(data)[2]){
            index <- which(mean.x[[i]]$values == 1)
            meanObj[1,i,1,1:2] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
      aux <- array(data = NA, dim = c(length(yoS),dim(data)[2]))
      for (y in 1:length(yoS)){
            indYear <- which(INDEX == yoS[y])
            if (length(indYear)>0){
                  indices <- rep(list(bquote()), length(dim(data)))
                  for (d in 1:length(dim(data))){
                        indices[[d]] <- 1:dim(data)[d]
                  }
                  indices[[1]] <- indYear
                  callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
                  mean.x <- apply(eval(callObj), MARGIN = 2, FUN = rle)
                  for (i in 1:dim(data)[2]){
                        index <- which(mean.x[[i]]$values == 1)
                        aux[y,i] <- max(mean.x[[i]]$lengths[index])
                  }
            }
      }
      meanObj[1,,1,3] <- apply(aux, MARGIN = 2, FUN = median, na.rm = TRUE)
      return(meanObj)
}

# sign of low pass filtered signal  x  x		signOfLowPassSignal	Does not distinguish between temporal scales
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

#' @title Get annual and seasonal proportion of variance in low frequency
#' @description Proportion of variance in low frequency
#' @author Dougals Maraun \email{dmaraun@@geomar.de} and J. Bedia and S. Herrera
#' @export
#' @keywords internal

# Proportion of variance in low frequency
getVarLF <- function(data, lowVarPeriod, INDEX = 1:dim(data)[1]){
      meanObj <- array(data = NA, dim = c(1,dim(data)[2],1,1))
      mean.x <- apply(data, MARGIN = 2, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = INDEX)
      for (i in 1:dim(data)[2]){
        if (any(is.finite(mean.x[,i]))){
          specVar <- spec.pgram(mean.x[,i], na.action = na.exclude, plot = FALSE)
          T <- 1 / specVar$freq
          lowfreqvar <- sum(specVar$spec[1 / specVar$freq >= lowVarPeriod], na.rm = TRUE)
          totalvar <- sum(specVar$spec, na.rm = TRUE)
          meanObj[1,i,1,1] <- lowfreqvar / totalvar
        }
      }
      return(meanObj)
}

#' @title Get annual and seasonal Cramer von Misses index
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch} and J. Bedia and S. Herrera
#' @export
#' @keywords internal

# Cramer von Misses
getCM <- function(dataRef, data, Nbins = 100){
  meanObj <- array(data = NA, dim = c(1,dim(dataRef)[2],1,1))
  for (i in 1:dim(dataRef)[2]){
    if (any(is.finite(dataRef[,i])) && any(is.finite(data[,i]))){
      indObjRef <- rep(list(bquote()), length(dim(dataRef)))
      for (d in 1:length(dim(dataRef))){
        indObjRef[[d]] <- 1:dim(dataRef)[d]
      }
      indObjRef[[2]] <- i
      callObjRef <- as.call(c(list(as.name("["),quote(dataRef)), indObjRef))
      x <- eval(callObjRef)
      indObj <- rep(list(bquote()), length(dim(data)))
      for (d in 1:length(dim(data))){
        indObj[[d]] <- 1:dim(data)[d]
      }
      indObj[[2]] <- i
      callObj <- as.call(c(list(as.name("["),quote(data)), indObj))
      y <- eval(callObj)
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
      meanObj[1,i,1,1] <- term1 * term2
    }
  }
  return(meanObj)
}

#' Especific function for precipitation
#' @title Amount from events above threshold
#' @description Get annual and seasonal amount falling in heavy rainy days from a station or field object
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg} and J. Bedia and S. Herrera
#' @export
#' @keywords internal

# Amount falling in heavy rainy days
getAmountFreqGT <- function(data, threshold, MARGIN = NULL){
      aux <- data
      aux[data<=threshold] <- NA
      meanObj <- apply(data, MARGIN = MARGIN, FUN = sum, na.rm = TRUE)
      return(meanObj)
}

#' @title 98th percentile
#' @description Get annual and seasonal 98th percentile from a station or field object
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg} and J. Bedia
#' @export
#' @keywords internal

# 98th percentile
getWet98th <- function(data, threshold, MARGIN = 1:length(dim(data))){
      aux <- data
      aux[data<threshold] <- NA
      meanObj <- apply(aux, MARGIN = MARGIN, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
      return(meanObj)
}

#' @title Wet-wet probability
#' @description Get annual and seasonal wet-wet-probability from a station or field object
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg} and J. Bedia and S. Herrera
#' @export
#' @keywords internal

# Transition probabilities: Wet-Wet
getFreqWW <- function(data, threshold){
      indToday <- rep(list(bquote()), length(dim(data)))
      indTomorrow <- rep(list(bquote()), length(dim(data)))
      for (d in 1:length(dim(data))){
            indToday[[d]] <- 1:dim(data)[d]
            indTomorrow[[d]] <- 1:dim(data)[d]
      }
      indToday[[1]] <- 1:(dim(data)[1]-1)
      indTomorrow[[1]] <- 2:dim(data)[1]
      callToday <- as.call(c(list(as.name("["),quote(data)), indToday))
      callTomorrow <- as.call(c(list(as.name("["),quote(data)), indTomorrow))
      meanObj <- apply((eval(callToday) >= threshold)*(eval(callTomorrow) >= threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      return(meanObj)
}

#' @title Wet-dry probability
#' @description Get annual and seasonal wet-dry-probability from a station or field object
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg} and J. Bedia
#' @export
#' @keywords internal

# Transition probabilities: Wet-Dry
getFreqWD <- function(data, threshold){
      indToday <- rep(list(bquote()), length(dim(data)))
      indTomorrow <- rep(list(bquote()), length(dim(data)))
      for (d in 1:length(dim(data))){
            indToday[[d]] <- 1:dim(data)[d]
            indTomorrow[[d]] <- 1:dim(data)[d]
      }
      indToday[[1]] <- 1:(dim(data)[1]-1)
      indTomorrow[[1]] <- 2:dim(data)[1]
      callToday <- as.call(c(list(as.name("["),quote(data)), indToday))
      callTomorrow <- as.call(c(list(as.name("["),quote(data)), indTomorrow))
      meanObj <- apply((eval(callToday) >= threshold)*(eval(callTomorrow)< threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      return(meanObj)
}

#' @title Dry-dry probability
#' @description Get annual and seasonal dry-dry-probability from a station or field object
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg} and J. Bedia and S. Herrera
#' @export
#' @keywords internal

# Transition probabilities: Dry-Dry
getFreqDD <- function(data, threshold){
      indToday <- rep(list(bquote()), length(dim(data)))
      indTomorrow <- rep(list(bquote()), length(dim(data)))
      for (d in 1:length(dim(data))){
            indToday[[d]] <- 1:dim(data)[d]
            indTomorrow[[d]] <- 1:dim(data)[d]
      }
      indToday[[1]] <- 1:(dim(data)[1]-1)
      indTomorrow[[1]] <- 2:dim(data)[1]
      callToday <- as.call(c(list(as.name("["),quote(data)), indToday))
      callTomorrow <- as.call(c(list(as.name("["),quote(data)), indTomorrow))
      meanObj <- apply((eval(callToday) < threshold)*(eval(callTomorrow)< threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      return(meanObj)
}

#' @title Dry-wet probability
#' @description Get annual and seasonal dry-wet-probability from a station or field object
#' @author Neyko Neykov \email{neyko.neykov@@meteo.bg} and J. Bedia and S. Herrera
#' @export
#' @keywords internal

# Transition probabilities: Dry-Wet
getFreqDW <- function(data, threshold){
      indToday <- rep(list(bquote()), length(dim(data)))
      indTomorrow <- rep(list(bquote()), length(dim(data)))
      for (d in 1:length(dim(data))){
            indToday[[d]] <- 1:dim(data)[d]
            indTomorrow[[d]] <- 1:dim(data)[d]
      }
      indToday[[1]] <- 1:(dim(data)[1]-1)
      indTomorrow[[1]] <- 2:dim(data)[1]
      callToday <- as.call(c(list(as.name("["),quote(data)), indToday))
      callTomorrow <- as.call(c(list(as.name("["),quote(data)), indTomorrow))
      meanObj <- apply((eval(callToday) < threshold)*(eval(callTomorrow) >= threshold), MARGIN = 2, FUN = mean, na.rm = TRUE)
      return(meanObj)
}

#' @title Get annual and seasonal above of a predefined threshold Spell Length Distribution from a station or field object
#' @export
#' @keywords internal

# Wet/Dry Spell Length Distribution

getWDsld <- function(data, threshold, INDEX = 1:dim(data)[1]){
      yoS <- unique(INDEX)
      bin.data <- data
      bin.data[data >= threshold] <- 1
      bin.data[data < threshold] <- 0
      meanObj <- array(data = NA, dim = c(1,dim(data)[2],1,6))
      mean.x <- apply(bin.data, MARGIN = 2, FUN = rle)
      for (i in 1:dim(data)[2]){
            index <- which(mean.x[[i]]$values == 1)
            meanObj[1,i,1,1:2] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
            index <- which(mean.x[[i]]$values == 0)
            meanObj[1,i,1,4:5] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
      auxWet <- array(data = NA, dim = c(length(yoS),dim(data)[2]))
      auxDry <- array(data = NA, dim = c(length(yoS),dim(data)[2]))
      for (y in 1:length(yoS)){
            indYear <- which(INDEX == yoS[y])
            if (length(indYear)>0){
                  indices <- rep(list(bquote()), length(dim(data)))
                  for (d in 1:length(dim(data))){
                        indices[[d]] <- 1:dim(data)[d]
                  }
                  indices[[1]] <- indYear
                  callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
                  mean.x <- apply(eval(callObj), MARGIN = 2, FUN = rle)
                  for (i in 1:dim(data)[2]){
                        index <- which(mean.x[[i]]$values == 1)
                        auxWet[y,i] <- max(mean.x[[i]]$lengths[index])
                        index <- which(mean.x[[i]]$values == 0)
                        auxDry[y,i] <- max(mean.x[[i]]$lengths[index])
                  }
            }
      }
      meanObj[1,,1,3] <- apply(auxWet, MARGIN = 2, FUN = median, na.rm = TRUE)
      meanObj[1,,1,6] <- apply(auxDry, MARGIN = 2, FUN = median, na.rm = TRUE)
      return(meanObj)
}
