#' @title Get annual and seasonal mean from a station or field object
#' @export
#' @keywords internal

# Mean:
getMean <- function(obj){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply(obj$Data, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
    meanObj[1,,1,1] <- mean.x
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply(obj$Data, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
    meanObj[1,,,1] <- mean.x
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indices))
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
        meanObj[s+1,,1,1] <- mean.x
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
        meanObj[s+1,,,1] <- mean.x
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal variance from a station or field object
#' @export
#' @keywords internal

# Variance:
getVar <- function(obj){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply(obj$Data, MARGIN = obj.station.index, FUN = var, na.rm = TRUE)
    meanObj[1,,1,1] <- mean.x
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply(obj$Data, MARGIN = c(obj.station.index, obj.member.index), FUN = var, na.rm = TRUE)
    meanObj[1,,,1] <- mean.x
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indices))
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = var, na.rm = TRUE)
        meanObj[s+1,,1,1] <- mean.x
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = var, na.rm = TRUE)
        meanObj[s+1,,,1] <- mean.x
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal skewness from a station or field object
#' @export
#' @keywords internal

# Skewness:
getSkew <- function(obj){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply(obj$Data, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
    std.x <- apply(obj$Data, MARGIN = obj.station.index, FUN = sd, na.rm = TRUE)
    auxObj <- (obj$Data - mean.x) / std.x
    auxObj <- apply(auxObj**3, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
    meanObj[1,,1,1] <- auxObj
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply(obj$Data, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
    std.x <- apply(obj$Data, MARGIN = c(obj.station.index, obj.member.index), FUN = sd, na.rm = TRUE)
    mean.x <- aperm(array(data = rep(mean.x, dim(obj$Data)[obj.time.index]), dim = c(dim(mean.x),dim(obj$Data)[obj.time.index])),c(2,3,1))
    std.x <- aperm(array(data = rep(std.x, dim(obj$Data)[obj.time.index]), dim = c(dim(std.x),dim(obj$Data)[obj.time.index])),c(2,3,1))
    auxObj <- (obj$Data - mean.x) / std.x
    auxObj <- apply(auxObj**3, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
    meanObj[1,,,1] <- auxObj
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indices))
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
        std.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = sd, na.rm = TRUE)
        auxObj <- (eval(callObj) - mean.x) / std.x
        auxObj <- apply(auxObj**3, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
        meanObj[s+1,,1,1] <- auxObj
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
        std.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = sd, na.rm = TRUE)
        mean.x <- aperm(array(data = rep(mean.x, length(indSeason)), dim = c(dim(mean.x),length(indSeason))),c(2,3,1))
        std.x <- aperm(array(data = rep(std.x, length(indSeason)), dim = c(dim(std.x),length(indSeason))),c(2,3,1))
        auxObj <- (eval(callObj) - mean.x) / std.x
        auxObj <- apply(auxObj**3, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
        meanObj[s+1,,,1] <- auxObj
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal amount of days above of a predefined threshold from a station or field object
#' @export
#' @keywords internal

# Above-threshold frequency
getFreqGT <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply((obj$Data>threshold), MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
    if (length(unique(yo))>1){
      meanObj[1,,1,1] <- apply(mean.x, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
    }else{
      meanObj[1,,1,1] <- mean.x
    }
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply((obj$Data>threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
    if (length(unique(yo))>1){
      meanObj[1,,,1] <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
    }else{
      meanObj[1,,,1] <- mean.x
    }
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indices))
      yoS <- date.vec$year[indSeason]
      if (length(obj.member.index)==0){
        mean.x <- apply((eval(callObj)>threshold), MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
        if (length(unique(yoS))>1){
          meanObj[s+1,,1,1] <- apply(mean.x, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
        }else{
          meanObj[s+1,,1,1] <- mean.x
        }
      }else{
        mean.x <- apply((eval(callObj)>threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
        if (length(unique(yoS))>1){
          meanObj[s+1,,,1] <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
        }else{
          meanObj[s+1,,,1] <- mean.x
        }
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal amount of days greater or equal than a predefined threshold from a station or field object
#' @export
#' @keywords internal

# Above-equal threshold frequency
getFreqGET <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply((obj$Data>=threshold), MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
    if (length(unique(yo))>1){
      meanObj[1,,1,1] <- apply(mean.x, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
    }else{
      meanObj[1,,1,1] <- mean.x
    }
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply((obj$Data>=threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
    if (length(unique(yo))>1){
      meanObj[1,,,1] <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
    }else{
      meanObj[1,,,1] <- mean.x
    }
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indices))
      yoS <- date.vec$year[indSeason]
      if (length(obj.member.index)==0){
        mean.x <- apply((eval(callObj)>=threshold), MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
        if (length(unique(yoS))>1){
          meanObj[s+1,,1,1] <- apply(mean.x, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
        }else{
          meanObj[s+1,,1,1] <- mean.x
        }
      }else{
        mean.x <- apply((eval(callObj)>=threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
        if (length(unique(yoS))>1){
          meanObj[s+1,,,1] <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
        }else{
          meanObj[s+1,,,1] <- mean.x
        }
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal amount of days below of a predefined threshold from a station or field object
#' @export
#' @keywords internal

# Below-threshold frequency
getFreqLT <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply((obj$Data<threshold), MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
    if (length(unique(yo))>1){
      meanObj[1,,1,1] <- apply(mean.x, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
    }else{
      meanObj[1,,1,1] <- mean.x
    }
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply((obj$Data<threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
    if (length(unique(yo))>1){
      meanObj[1,,,1] <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
    }else{
      meanObj[1,,,1] <- mean.x
    }
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indices))
      yoS <- date.vec$year[indSeason]
      if (length(obj.member.index)==0){
        mean.x <- apply((eval(callObj)<threshold), MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
        if (length(unique(yoS))>1){
          meanObj[s+1,,1,1] <- apply(mean.x, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
        }else{
          meanObj[s+1,,1,1] <- mean.x
        }
      }else{
        mean.x <- apply((eval(callObj)<threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
        if (length(unique(yoS))>1){
          meanObj[s+1,,,1] <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
        }else{
          meanObj[s+1,,,1] <- mean.x
        }
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal 98th percentile from a station or field object
#' @export
#' @keywords internal

# 98th percentile
get98th <- function(obj){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply(obj$Data, MARGIN = obj.station.index, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
    meanObj[1,,1,1] <- mean.x
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply(obj$Data, MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
    meanObj[1,,,1] <- mean.x
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indices))
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
        meanObj[s+1,,1,1] <- mean.x
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
        meanObj[s+1,,,1] <- mean.x
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal Autocorrelation from a station or field object
#' @export
#' @keywords internal

# Autocorrelation lag 1, 2 and 3:
getACF <- function(obj, lag.max){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,lag.max))
    mean.x <- apply(obj$Data, MARGIN = obj.station.index, FUN = acf, na.action = na.pass, plot = FALSE, lag.max = lag.max)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      index <- which(mean.x[[i]]$lag %in% c(1:lag.max))
      if (length(index)>0){
        meanObj[1,i,1,1:length(index)] <- mean.x[[i]]$acf[index]
      }
    }
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],lag.max))
    mean.x <- apply(obj$Data, MARGIN = c(obj.station.index, obj.member.index), FUN = acf, na.action = na.pass, plot = FALSE, lag.max = lag.max)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      for (j in 1:dim(obj$Data)[obj.member.index]){
        index <- which(mean.x[[(j-1)*dim(obj$Data)[obj.station.index]+i]]$lag %in% c(1:lag.max))
        if (length(index)>0){
          meanObj[1,i,j,1:length(index)] <- mean.x[[(j-1)*dim(obj$Data)[obj.station.index]+i]]$acf[index]
        }
      }
    }
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indices))
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = acf, na.action = na.pass, plot = FALSE, lag.max = lag.max)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          index <- which(mean.x[[i]]$lag %in% c(1:lag.max))
          if (length(index)>0){
            meanObj[s+1,i,1,1:length(index)] <- mean.x[[i]]$acf[index]
          }
        }
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = acf, na.action = na.pass, plot = FALSE, lag.max = lag.max)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          for (j in 1:dim(obj$Data)[obj.member.index]){
            index <- which(mean.x[[(j-1)*dim(obj$Data)[obj.station.index]+i]]$lag %in% c(1:lag.max))
            if (length(index)>0){
              meanObj[s+1,i,j,1:length(index)] <- mean.x[[(j-1)*dim(obj$Data)[obj.station.index]+i]]$acf[index]
            }
          }
        }
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal 1/prob return value from a station or field object
#' @export
#' @keywords internal

# 20-years Return value:
getReturnValue <- function(obj, prob){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,2))
    mean.x <- apply(obj$Data, MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = max, na.rm = TRUE)}, INDEX = yo)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      estim <- fgev(mean.x[,i], prob = prob, std.err = FALSE)
      meanObj[1,i,1,1] <- estim$param[1]
      estim <- fgev(mean.x[,i], prob = 1 - prob, std.err = FALSE)
      meanObj[1,i,1,2] <- estim$param[1]
    }
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],2))
    mean.x <- apply(obj$Data, MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = max, na.rm = TRUE)}, INDEX = mo)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      for (j in 1:dim(obj$Data)[obj.member.index]){
        estim <- fgev(mean.x[,i,j], prob = prob, std.err = FALSE)
        meanObj[1,i,j,1] <- estim$param[1]
        estim <- fgev(mean.x[,i,j], prob = 1 - prob, std.err = FALSE)
        meanObj[1,i,j,2] <- estim$param[1]
      }
    }
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indices))
      yoS <- date.vec$year[indSeason]
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = max, na.rm = TRUE)}, INDEX = yoS)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          estim <- fgev(mean.x[,i], prob = prob, std.err = FALSE)
          meanObj[s+1,i,1,1] <- estim$param[1]
          estim <- fgev(mean.x[,i], prob = 1 - prob, std.err = FALSE)
          meanObj[s+1,i,1,2] <- estim$param[1]
        }
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = max, na.rm = TRUE)}, INDEX = yoS)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          for (j in 1:dim(obj$Data)[obj.member.index]){
            estim <- fgev(mean.x[,i,j], prob = prob, std.err = FALSE)
            meanObj[s+1,i,j,1] <- estim$param[1]
            estim <- fgev(mean.x[,i,j], prob = 1 - prob, std.err = FALSE)
            meanObj[s+1,i,j,2] <- estim$param[1]
          }
        }
      }
    }
  }
  return(meanObj)
}

#' @title Get annual cicles indicators from a station or field object
#' @export
#' @keywords internal

# Annual Cicles:
getAnnualCicle <- function(obj){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  mo <- date.vec$mon+1
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,4))
    mean.x <- apply(obj$Data, MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = mo)
    meanObj[1,,1,1] <- apply(mean.x, MARGIN = obj.station.index, FUN = min, na.rm = TRUE)
    meanObj[1,,1,2] <- apply(mean.x, MARGIN = obj.station.index, FUN = max, na.rm = TRUE)
    meanObj[1,,1,3] <- (apply(mean.x, MARGIN = obj.station.index, FUN = max, na.rm = TRUE)-apply(mean.x, MARGIN = obj.station.index, FUN = min, na.rm = TRUE))
    meanObj[1,,1,4] <- 100*(apply(mean.x, MARGIN = obj.station.index, FUN = max, na.rm = TRUE)-apply(mean.x, MARGIN = obj.station.index, FUN = min, na.rm = TRUE))/apply(mean.x, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],4))
    mean.x <- apply(obj$Data, MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = mo)
    meanObj[1,,,1] <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = min, na.rm = TRUE)
    meanObj[1,,,2] <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = max, na.rm = TRUE)
    meanObj[1,,,3] <- (apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = max, na.rm = TRUE)-apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = min, na.rm = TRUE))
    meanObj[1,,,4] <- 100*(apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = max, na.rm = TRUE)-apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = min, na.rm = TRUE))/apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
  }
  return(meanObj)
}

#' @title Get annual and seasonal above of a predefined threshold Spell Length Distribution from a station or field object
#' @export
#' @keywords internal

# Above Spell Length Distribution
getGTsld <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  yoS <- unique(yo)
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  bin.data <- obj$Data
  bin.data[obj$Data > threshold] <- 1
  bin.data[obj$Data <= threshold] <- 0
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,3))
    mean.x <- apply(bin.data, MARGIN = obj.station.index, FUN = rle)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      index <- which(mean.x[[i]]$values == 1)
      meanObj[1,i,1,1:2] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
    }
    aux <- array(data = NA, dim = c(length(yoS),dim(obj$Data)[obj.station.index]))
    for (y in 1:length(yoS)){
      indYear <- which(yo == yoS[y])
      if (length(indYear)>0){
        indices <- rep(list(bquote()), length(dimObj))
        for (d in 1:length(dimObj)){
          indices[[d]] <- 1:dimObj[d]
        }
        indices[[obj.time.index]] <- indYear
        callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          index <- which(mean.x[[i]]$values == 1)
          aux[y,i] <- max(mean.x[[i]]$lengths[index])
        }
      }
    }
    meanObj[1,,1,3] <- apply(aux, MARGIN = 2, FUN = median, na.rm = TRUE)
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],3))
    mean.x <- apply(bin.data, MARGIN = c(obj.station.index, obj.member.index), FUN = rle)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      for (j in 1:dim(obj$Data)[obj.member.index]){
        index <- which(mean.x[[i,j]]$values == 1)
        meanObj[1,i,j,1:2] <- quantile(mean.x[[i,j]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
    }
    aux <- array(data = NA, dim = c(length(yoS),dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index]))
    for (y in 1:length(yoS)){
      indYear <- which(yo == yoS[y])
      if (length(indYear)>0){
        indices <- rep(list(bquote()), length(dimObj))
        for (d in 1:length(dimObj)){
          indices[[d]] <- 1:dimObj[d]
        }
        indices[[obj.time.index]] <- indYear
        callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          for (j in 1:dim(obj$Data)[obj.member.index]){
            index <- which(mean.x[[i,j]]$values == 1)
            aux[y,i,j] <- max(mean.x[[i,j]]$lengths[index])
          }
        }
      }
    }
    meanObj[1,,,3] <- apply(aux, MARGIN = c(2,3), FUN = median, na.rm = TRUE)
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          index <- which(mean.x[[i]]$values == 1)
          meanObj[s+1,i,1,1:2] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
        }
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          for (j in 1:dim(obj$Data)[obj.member.index]){
            index <- which(mean.x[[i,j]]$values == 1)
            meanObj[s+1,i,j,1:2] <- quantile(mean.x[[i,j]]$lengths[index], probs = c(0.5,0.9), type = 7)
          }
        }
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal Below of a predefined threshold Spell Length Distribution from a station or field object
#' @export
#' @keywords internal

# Below Spell Length Distribution
getLTsld <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  yoS <- unique(yo)
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  bin.data <- obj$Data
  bin.data[obj$Data < threshold] <- 1
  bin.data[obj$Data >= threshold] <- 0
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,3))
    mean.x <- apply(bin.data, MARGIN = obj.station.index, FUN = rle)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      index <- which(mean.x[[i]]$values == 1)
      meanObj[1,i,1,1:2] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
    }
    aux <- array(data = NA, dim = c(length(yoS),dim(obj$Data)[obj.station.index]))
    for (y in 1:length(yoS)){
      indYear <- which(yo == yoS[y])
      if (length(indYear)>0){
        indices <- rep(list(bquote()), length(dimObj))
        for (d in 1:length(dimObj)){
          indices[[d]] <- 1:dimObj[d]
        }
        indices[[obj.time.index]] <- indYear
        callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          index <- which(mean.x[[i]]$values == 1)
          aux[y,i] <- max(mean.x[[i]]$lengths[index])
        }
      }
    }
    meanObj[1,,1,3] <- apply(aux, MARGIN = 2, FUN = median, na.rm = TRUE)
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],3))
    mean.x <- apply(bin.data, MARGIN = c(obj.station.index, obj.member.index), FUN = rle)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      for (j in 1:dim(obj$Data)[obj.member.index]){
        index <- which(mean.x[[i,j]]$values == 1)
        meanObj[1,i,j,1:2] <- quantile(mean.x[[i,j]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
    }
    aux <- array(data = NA, dim = c(length(yoS),dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index]))
    for (y in 1:length(yoS)){
      indYear <- which(yo == yoS[y])
      if (length(indYear)>0){
        indices <- rep(list(bquote()), length(dimObj))
        for (d in 1:length(dimObj)){
          indices[[d]] <- 1:dimObj[d]
        }
        indices[[obj.time.index]] <- indYear
        callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          for (j in 1:dim(obj$Data)[obj.member.index]){
            index <- which(mean.x[[i,j]]$values == 1)
            aux[y,i,j] <- max(mean.x[[i,j]]$lengths[index])
          }
        }
      }
    }
    meanObj[1,,,3] <- apply(aux, MARGIN = c(2,3), FUN = median, na.rm = TRUE)
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          index <- which(mean.x[[i]]$values == 1)
          meanObj[s+1,i,1,1:2] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
        }
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          for (j in 1:dim(obj$Data)[obj.member.index]){
            index <- which(mean.x[[i,j]]$values == 1)
            meanObj[s+1,i,j,1:2] <- quantile(mean.x[[i,j]]$lengths[index], probs = c(0.5,0.9), type = 7)
          }
        }
      }
    }
  }
  return(meanObj)
}

# sign of low pass filtered signal  x	x		signOfLowPassSignal	Does not distinguish between temporal scales
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
#' @export
#' @keywords internal

# Proportion of variance in low frequency
getVarLF <- function(obj, lowVarPeriod){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply(obj$Data, MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yo)
    specVar <- apply(mean.x, MARGIN = obj.station.index, FUN = spec.pgram, na.action = na.exclude, plot = FALSE)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      T <- 1 / specVar[[i]]$freq
      lowfreqvar <- sum(specVar[[i]]$spec[1 / specVar[[i]]$freq >= lowVarPeriod], na.rm = TRUE)
      totalvar <- sum(specVar[[i]]$spec, na.rm = TRUE)
      meanObj[1,i,1,1] <- lowfreqvar / totalvar
    }
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply(obj$Data, MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yo)
    specVar <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = spec.pgram, na.action = na.exclude, plot = FALSE)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      for (j in 1:dim(obj$Data)[obj.member.index]){
        T <- 1 / specVar[[i,j]]$freq
        lowfreqvar <- sum(specVar[[i,j]]$spec[1 / specVar[[i,j]]$freq >= lowVarPeriod], na.rm = TRUE)
        totalvar <- sum(specVar[[i,j]]$spec, na.rm = TRUE)
        meanObj[1,i,j,1] <- lowfreqvar / totalvar
      }
    }
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      yoS <- yo[indSeason]
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indices))
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yoS)
        specVar <- apply(mean.x, MARGIN = obj.station.index, FUN = spec.pgram, na.action = na.exclude, plot = FALSE)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          T <- 1 / specVar[[i]]$freq
          lowfreqvar <- sum(specVar[[i]]$spec[1 / specVar[[i]]$freq >= lowVarPeriod], na.rm = TRUE)
          totalvar <- sum(specVar[[i]]$spec, na.rm = TRUE)
          meanObj[s+1,i,1,1] <- lowfreqvar / totalvar
        }
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = mean, na.rm = TRUE)}, INDEX = yoS)
        specVar <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = spec.pgram, na.action = na.exclude, plot = FALSE)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          for (j in 1:dim(obj$Data)[obj.member.index]){
            T <- 1 / specVar[[i,j]]$freq
            lowfreqvar <- sum(specVar[[i,j]]$spec[1 / specVar[[i,j]]$freq >= lowVarPeriod], na.rm = TRUE)
            totalvar <- sum(specVar[[i,j]]$spec, na.rm = TRUE)
            meanObj[s+1,i,j,1] <- lowfreqvar / totalvar
          }
        }
      }
      
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal Cramer von Misses index
#' @export
#' @keywords internal

# Cramer von Misses

getCM <- function(objRef, obj, Nbins = 100){
  date.vec <- as.POSIXlt(objRef$Dates$start, tz = "GMT")
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObjRef <- dim(objRef$Data)
  objRef.time.index <- grep("^time$", attr(objRef$Data, "dimensions"))
  objRef.station.index <- grep("^station$", attr(objRef$Data, "dimensions"))
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(objRef$Data)[objRef.station.index],1,1))
    for (i in 1:dim(objRef$Data)[objRef.station.index]){
      indObjRef <- rep(list(bquote()), length(dimObjRef))
      for (d in 1:length(dimObjRef)){
        indObjRef[[d]] <- 1:dimObjRef[d]
      }
      indObjRef[[objRef.station.index]] <- i
      callObjRef <- as.call(c(list(as.name("["),quote(objRef$Data)), indObjRef))
      x <- eval(callObjRef)
      indObj <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indObj[[d]] <- 1:dimObj[d]
      }
      indObj[[obj.station.index]] <- i
      callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indObj))
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
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(objRef$Data)[objRef.station.index],dim(obj$Data)[obj.member.index],1))
    for (i in 1:dim(objRef$Data)[objRef.station.index]){
      indObjRef <- rep(list(bquote()), length(dimObjRef))
      for (d in 1:length(dimObjRef)){
        indObjRef[[d]] <- 1:dimObjRef[d]
      }
      indObjRef[[objRef.station.index]] <- i
      callObjRef <- as.call(c(list(as.name("["),quote(objRef$Data)), indObjRef))
      x <- eval(callObjRef)
      for (j in 1:dim(obj$Data)[obj.member.index]){
        indObj <- rep(list(bquote()), length(dimObj))
        for (d in 1:length(dimObj)){
          indObj[[d]] <- 1:dimObj[d]
        }
        indObj[[obj.station.index]] <- i
        indObj[[obj.member.index]] <- j
        callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indObj))
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
        meanObj[1,i,j,1] <- term1 * term2
      }
    }
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      if (length(obj.member.index)==0){
        for (i in 1:dim(objRef$Data)[objRef.station.index]){
          indObjRef <- rep(list(bquote()), length(dimObjRef))
          for (d in 1:length(dimObjRef)){
            indObjRef[[d]] <- 1:dimObjRef[d]
          }
          indObjRef[[objRef.time.index]] <- indSeason
          indObjRef[[objRef.station.index]] <- i
          callObjRef <- as.call(c(list(as.name("["),quote(objRef$Data)), indObjRef))
          x <- eval(callObjRef)
          indObj <- rep(list(bquote()), length(dimObj))
          for (d in 1:length(dimObj)){
            indObj[[d]] <- 1:dimObj[d]
          }
          indObj[[obj.time.index]] <- indSeason
          indObj[[obj.station.index]] <- i
          callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indObj))
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
          meanObj[s+1,i,1,1] <- term1 * term2
        }
      }else{
        for (i in 1:dim(objRef$Data)[objRef.station.index]){
          indObjRef <- rep(list(bquote()), length(dimObjRef))
          for (d in 1:length(dimObjRef)){
            indObjRef[[d]] <- 1:dimObjRef[d]
          }
          indObjRef[[objRef.time.index]] <- indSeason
          indObjRef[[objRef.station.index]] <- i
          callObjRef <- as.call(c(list(as.name("["),quote(objRef$Data)), indObjRef))
          x <- eval(callObjRef)
          for (j in 1:dim(obj$Data)[obj.member.index]){
            indObj <- rep(list(bquote()), length(dimObj))
            for (d in 1:length(dimObj)){
              indObj[[d]] <- 1:dimObj[d]
            }
            indObj[[obj.time.index]] <- indSeason
            indObj[[obj.station.index]] <- i
            indObj[[obj.member.index]] <- j
            callObj <- as.call(c(list(as.name("["),quote(obj$Data)), indObj))
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
            meanObj[s+1,i,j,1] <- term1 * term2
          }
        }
      }
    }
  }
  return(meanObj)
}

#' Especific function for precipitation
#' @title Get annual and seasonal amount falling in heavy rainy days from a station or field object
#' @export
#' @keywords internal

# Amount falling in heavy rainy days
getAmountFreqGT <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  aux <- obj$Data
  aux[obj$Data<=threshold] <- NA
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply(aux, MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
    if (length(unique(yo))>1){
      meanObj[1,,1,1] <- apply(mean.x, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
    }else{
      meanObj[1,,1,1] <- mean.x
    }
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply(aux, MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yo)
    if (length(unique(yo))>1){
      meanObj[1,,,1] <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
    }else{
      meanObj[1,,,1] <- mean.x
    }
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(aux)), indices))
      yoS <- date.vec$year[indSeason]
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
        if (length(unique(yoS))>1){
          meanObj[s+1,,1,1] <- apply(mean.x, MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
        }else{
          meanObj[s+1,,1,1] <- mean.x
        }
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, INDEX = INDEX){tapply(x, INDEX = INDEX, FUN = sum, na.rm = TRUE)}, INDEX = yoS)
        if (length(unique(yoS))>1){
          meanObj[s+1,,,1] <- apply(mean.x, MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
        }else{
          meanObj[s+1,,,1] <- mean.x
        }
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal 98th percentile of the wet days from a station or field object
#' @export
#' @keywords internal

# 98th percentile
getWet98th <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  aux <- obj$Data
  aux[obj$Data<threshold] <- NA
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply(aux, MARGIN = obj.station.index, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
    meanObj[1,,1,1] <- mean.x
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply(aux, MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
    meanObj[1,,,1] <- mean.x
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(aux)), indices))
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
        meanObj[s+1,,1,1] <- mean.x
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = function(x, probs = probs, type = type){quantile(x, probs = probs, type = type, na.rm = TRUE)}, probs = 0.98, type = 7)
        meanObj[s+1,,,1] <- mean.x
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal transition probabilities from a station or field object
#' @export
#' @keywords internal

# Transition probabilities: Wet-Wet
getFreqWW <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  indToday <- rep(list(bquote()), length(dimObj))
  indTomorrow <- rep(list(bquote()), length(dimObj))
  for (d in 1:length(dimObj)){
    indToday[[d]] <- 1:dimObj[d]
    indTomorrow[[d]] <- 1:dimObj[d]
  }
  indToday[[obj.time.index]] <- 1:(dimObj[obj.time.index]-1)
  indTomorrow[[obj.time.index]] <- 2:dimObj[obj.time.index]
  callToday <- as.call(c(list(as.name("["),quote(obj$Data)), indToday))
  callTomorrow <- as.call(c(list(as.name("["),quote(obj$Data)), indTomorrow))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply((eval(callToday) >= threshold)*(eval(callTomorrow) >= threshold), MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
    meanObj[1,,1,1] <- mean.x
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply((eval(callToday) >= threshold)*(eval(callTomorrow) >= threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
    meanObj[1,,,1] <- mean.x
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indToday <- rep(list(bquote()), length(dimObj))
      indTomorrow <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indToday[[d]] <- 1:dimObj[d]
        indTomorrow[[d]] <- 1:dimObj[d]
      }
      indToday[[obj.time.index]] <- indSeason[1:(length(indSeason)-1)]
      indTomorrow[[obj.time.index]] <- indSeason[2:length(indSeason)]
      callToday <- as.call(c(list(as.name("["),quote(obj$Data)), indToday))
      callTomorrow <- as.call(c(list(as.name("["),quote(obj$Data)), indTomorrow))
      if (length(obj.member.index)==0){
        mean.x <- apply((eval(callToday) >= threshold)*(eval(callTomorrow) >= threshold), MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
        meanObj[s+1,,1,1] <- mean.x
      }else{
        mean.x <- apply((eval(callToday) >= threshold)*(eval(callTomorrow) >= threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
        meanObj[s+1,,,1] <- mean.x
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal transition probabilities from a station or field object
#' @export
#' @keywords internal

# Transition probabilities: Wet-Dry
getFreqWD <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  indToday <- rep(list(bquote()), length(dimObj))
  indTomorrow <- rep(list(bquote()), length(dimObj))
  for (d in 1:length(dimObj)){
    indToday[[d]] <- 1:dimObj[d]
    indTomorrow[[d]] <- 1:dimObj[d]
  }
  indToday[[obj.time.index]] <- 1:(dimObj[obj.time.index]-1)
  indTomorrow[[obj.time.index]] <- 2:dimObj[obj.time.index]
  callToday <- as.call(c(list(as.name("["),quote(obj$Data)), indToday))
  callTomorrow <- as.call(c(list(as.name("["),quote(obj$Data)), indTomorrow))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply((eval(callToday) >= threshold)*(eval(callTomorrow) < threshold), MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
    meanObj[1,,1,1] <- mean.x
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply((eval(callToday) >= threshold)*(eval(callTomorrow) < threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
    meanObj[1,,,1] <- mean.x
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indToday <- rep(list(bquote()), length(dimObj))
      indTomorrow <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indToday[[d]] <- 1:dimObj[d]
        indTomorrow[[d]] <- 1:dimObj[d]
      }
      indToday[[obj.time.index]] <- indSeason[1:(length(indSeason)-1)]
      indTomorrow[[obj.time.index]] <- indSeason[2:length(indSeason)]
      callToday <- as.call(c(list(as.name("["),quote(obj$Data)), indToday))
      callTomorrow <- as.call(c(list(as.name("["),quote(obj$Data)), indTomorrow))
      if (length(obj.member.index)==0){
        mean.x <- apply((eval(callToday) >= threshold)*(eval(callTomorrow) < threshold), MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
        meanObj[s+1,,1,1] <- mean.x
      }else{
        mean.x <- apply((eval(callToday) >= threshold)*(eval(callTomorrow) < threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
        meanObj[s+1,,,1] <- mean.x
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal transition probabilities from a station or field object
#' @export
#' @keywords internal

# Transition probabilities: Dry-Dry
getFreqDD <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  indToday <- rep(list(bquote()), length(dimObj))
  indTomorrow <- rep(list(bquote()), length(dimObj))
  for (d in 1:length(dimObj)){
    indToday[[d]] <- 1:dimObj[d]
    indTomorrow[[d]] <- 1:dimObj[d]
  }
  indToday[[obj.time.index]] <- 1:(dimObj[obj.time.index]-1)
  indTomorrow[[obj.time.index]] <- 2:dimObj[obj.time.index]
  callToday <- as.call(c(list(as.name("["),quote(obj$Data)), indToday))
  callTomorrow <- as.call(c(list(as.name("["),quote(obj$Data)), indTomorrow))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply((eval(callToday) < threshold)*(eval(callTomorrow) < threshold), MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
    meanObj[1,,1,1] <- mean.x
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply((eval(callToday) < threshold)*(eval(callTomorrow) < threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
    meanObj[1,,,1] <- mean.x
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indToday <- rep(list(bquote()), length(dimObj))
      indTomorrow <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indToday[[d]] <- 1:dimObj[d]
        indTomorrow[[d]] <- 1:dimObj[d]
      }
      indToday[[obj.time.index]] <- indSeason[1:(length(indSeason)-1)]
      indTomorrow[[obj.time.index]] <- indSeason[2:length(indSeason)]
      callToday <- as.call(c(list(as.name("["),quote(obj$Data)), indToday))
      callTomorrow <- as.call(c(list(as.name("["),quote(obj$Data)), indTomorrow))
      if (length(obj.member.index)==0){
        mean.x <- apply((eval(callToday) < threshold)*(eval(callTomorrow) < threshold), MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
        meanObj[s+1,,1,1] <- mean.x
      }else{
        mean.x <- apply((eval(callToday) < threshold)*(eval(callTomorrow) < threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
        meanObj[s+1,,,1] <- mean.x
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal transition probabilities from a station or field object
#' @export
#' @keywords internal

# Transition probabilities: Dry-Wet
getFreqDW <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  indToday <- rep(list(bquote()), length(dimObj))
  indTomorrow <- rep(list(bquote()), length(dimObj))
  for (d in 1:length(dimObj)){
    indToday[[d]] <- 1:dimObj[d]
    indTomorrow[[d]] <- 1:dimObj[d]
  }
  indToday[[obj.time.index]] <- 1:(dimObj[obj.time.index]-1)
  indTomorrow[[obj.time.index]] <- 2:dimObj[obj.time.index]
  callToday <- as.call(c(list(as.name("["),quote(obj$Data)), indToday))
  callTomorrow <- as.call(c(list(as.name("["),quote(obj$Data)), indTomorrow))
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,1))
    mean.x <- apply((eval(callToday) < threshold)*(eval(callTomorrow) >= threshold), MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
    meanObj[1,,1,1] <- mean.x
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],1))
    mean.x <- apply((eval(callToday) < threshold)*(eval(callTomorrow) >= threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
    meanObj[1,,,1] <- mean.x
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indToday <- rep(list(bquote()), length(dimObj))
      indTomorrow <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indToday[[d]] <- 1:dimObj[d]
        indTomorrow[[d]] <- 1:dimObj[d]
      }
      indToday[[obj.time.index]] <- indSeason[1:(length(indSeason)-1)]
      indTomorrow[[obj.time.index]] <- indSeason[2:length(indSeason)]
      callToday <- as.call(c(list(as.name("["),quote(obj$Data)), indToday))
      callTomorrow <- as.call(c(list(as.name("["),quote(obj$Data)), indTomorrow))
      if (length(obj.member.index)==0){
        mean.x <- apply((eval(callToday) < threshold)*(eval(callTomorrow) >= threshold), MARGIN = obj.station.index, FUN = mean, na.rm = TRUE)
        meanObj[s+1,,1,1] <- mean.x
      }else{
        mean.x <- apply((eval(callToday) < threshold)*(eval(callTomorrow) >= threshold), MARGIN = c(obj.station.index, obj.member.index), FUN = mean, na.rm = TRUE)
        meanObj[s+1,,,1] <- mean.x
      }
    }
  }
  return(meanObj)
}

#' @title Get annual and seasonal above of a predefined threshold Spell Length Distribution from a station or field object
#' @export
#' @keywords internal

# Wet/Dry Spell Length Distribution

getWDsld <- function(obj, threshold){
  date.vec <- as.POSIXlt(obj$Dates$start, tz = "GMT")
  yo <- date.vec$year
  yoS <- unique(yo)
  mo <- date.vec$mon+1
  so <- mo
  so[which(!is.na(match(mo, c(12,1,2))))] <- 1
  so[which(!is.na(match(mo, c(3,4,5))))] <- 2
  so[which(!is.na(match(mo, c(6,7,8))))] <- 3
  so[which(!is.na(match(mo, c(9,10,11))))] <- 4
  dimObj <- dim(obj$Data)
  obj.time.index <- grep("^time$", attr(obj$Data, "dimensions"))
  obj.station.index <- grep("^station$", attr(obj$Data, "dimensions"))
  obj.member.index <- grep("^member$", attr(obj$Data, "dimensions"))
  bin.data <- obj$Data
  bin.data[obj$Data >= threshold] <- 1
  bin.data[obj$Data < threshold] <- 0
  if (length(obj.member.index)==0){
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],1,6))
    mean.x <- apply(bin.data, MARGIN = obj.station.index, FUN = rle)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      index <- which(mean.x[[i]]$values == 1)
      meanObj[1,i,1,1:2] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
      index <- which(mean.x[[i]]$values == 0)
      meanObj[1,i,1,4:5] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
    }
    auxWet <- array(data = NA, dim = c(length(yoS),dim(obj$Data)[obj.station.index]))
    auxDry <- array(data = NA, dim = c(length(yoS),dim(obj$Data)[obj.station.index]))
    for (y in 1:length(yoS)){
      indYear <- which(yo == yoS[y])
      if (length(indYear)>0){
        indices <- rep(list(bquote()), length(dimObj))
        for (d in 1:length(dimObj)){
          indices[[d]] <- 1:dimObj[d]
        }
        indices[[obj.time.index]] <- indYear
        callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          index <- which(mean.x[[i]]$values == 1)
          auxWet[y,i] <- max(mean.x[[i]]$lengths[index])
          index <- which(mean.x[[i]]$values == 0)
          auxDry[y,i] <- max(mean.x[[i]]$lengths[index])
        }
      }
    }
    meanObj[1,,1,3] <- apply(auxWet, MARGIN = 2, FUN = median, na.rm = TRUE)
    meanObj[1,,1,6] <- apply(auxDry, MARGIN = 2, FUN = median, na.rm = TRUE)
  }else{
    meanObj <- array(data = NA, dim = c(5,dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index],6))
    mean.x <- apply(bin.data, MARGIN = c(obj.station.index, obj.member.index), FUN = rle)
    for (i in 1:dim(obj$Data)[obj.station.index]){
      for (j in 1:dim(obj$Data)[obj.member.index]){
        index <- which(mean.x[[i,j]]$values == 1)
        meanObj[1,i,j,1:2] <- quantile(mean.x[[i,j]]$lengths[index], probs = c(0.5,0.9), type = 7)
        index <- which(mean.x[[i,j]]$values == 0)
        meanObj[1,i,j,4:5] <- quantile(mean.x[[i,j]]$lengths[index], probs = c(0.5,0.9), type = 7)
      }
    }
    auxWet <- array(data = NA, dim = c(length(yoS),dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index]))
    auxDry <- array(data = NA, dim = c(length(yoS),dim(obj$Data)[obj.station.index],dim(obj$Data)[obj.member.index]))
    for (y in 1:length(yoS)){
      indYear <- which(yo == yoS[y])
      if (length(indYear)>0){
        indices <- rep(list(bquote()), length(dimObj))
        for (d in 1:length(dimObj)){
          indices[[d]] <- 1:dimObj[d]
        }
        indices[[obj.time.index]] <- indYear
        callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          for (j in 1:dim(obj$Data)[obj.member.index]){
            index <- which(mean.x[[i,j]]$values == 1)
            auxWet[y,i,j] <- max(mean.x[[i,j]]$lengths[index])
            index <- which(mean.x[[i,j]]$values == 0)
            auxDry[y,i,j] <- max(mean.x[[i,j]]$lengths[index])
          }
        }
      }
    }
    meanObj[1,,,3] <- apply(auxWet, MARGIN = c(2,3), FUN = median, na.rm = TRUE)
    meanObj[1,,,6] <- apply(auxDry, MARGIN = c(2,3), FUN = median, na.rm = TRUE)
  }
  for (s in 1:4){
    indSeason <- which(so == s)
    if (length(indSeason)>0){
      indices <- rep(list(bquote()), length(dimObj))
      for (d in 1:length(dimObj)){
        indices[[d]] <- 1:dimObj[d]
      }
      indices[[obj.time.index]] <- indSeason
      callObj <- as.call(c(list(as.name("["),quote(bin.data)), indices))
      if (length(obj.member.index)==0){
        mean.x <- apply(eval(callObj), MARGIN = obj.station.index, FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          index <- which(mean.x[[i]]$values == 1)
          meanObj[s+1,i,1,1:2] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
          index <- which(mean.x[[i]]$values == 0)
          meanObj[s+1,i,1,4:5] <- quantile(mean.x[[i]]$lengths[index], probs = c(0.5,0.9), type = 7)
        }
      }else{
        mean.x <- apply(eval(callObj), MARGIN = c(obj.station.index, obj.member.index), FUN = rle)
        for (i in 1:dim(obj$Data)[obj.station.index]){
          for (j in 1:dim(obj$Data)[obj.member.index]){
            index <- which(mean.x[[i,j]]$values == 1)
            meanObj[s+1,i,j,1:2] <- quantile(mean.x[[i,j]]$lengths[index], probs = c(0.5,0.9), type = 7)
            index <- which(mean.x[[i,j]]$values == 0)
            meanObj[s+1,i,j,4:5] <- quantile(mean.x[[i,j]]$lengths[index], probs = c(0.5,0.9), type = 7)
          }
        }
      }
    }
  }
  return(meanObj)
}
