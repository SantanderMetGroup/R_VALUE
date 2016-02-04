#' @title Get season from a station or field object
#' @description Retrieves the season encompassed by a station or field object
#' @param obj Any object extending the station or field classes
#' @return An integer vector with the season
#' @author juaco
#' @keywords internal

getSeason <- function(obj) {
      aux <- as.POSIXlt(obj$Dates$start)$mon + 1      
      return(unique(aux))
}
# End

#' @title Get years as a factor
#' @description Extract the year as a factor (e.g. for computing annual statistics)
#' @param obj Any object extending the station or field classes
#' @return A vector of years of the same length as the time dimension of the object, 
#' seasonally-adjusted in the case of year-crossing seasons (e.g. DJF). See details.
#' @details The function performs a very basic operation, extracting the year element from the 
#' dates previously converted to POSIXlt. The trick lies in the year-crossing seasons. For instance:
#'  by convention, winter 2001 encompasses December 2000 and January, February 2001. Therefore, in order to compute
#' annual statistics for a year-crossing season, it is necessary to modify first the vector of years, 
#' and assign year 2001 to the preceding December. Similarly, the next December 2001 belongs to winter 2002,
#'  and so on... The function is useful for computing and/or plotting annual statistics, seasonal climatologies ... 
#' @section Warning:
#' The function should no be used to extract the actual years vector
#' @author juaco
#' @keywords internal

getYearsAsINDEX <- function(obj) {
      season <- getSeason(obj)
      aux.dates <- as.POSIXlt(obj$Dates$start)
      yrs <- aux.dates$year + 1900
      if (!identical(season, sort(season))) {
            yy <- unique(yrs)[-1]
            aux <- match(aux.dates$mon + 1, season)
            brks <- c(1, which(diff(aux) < 0) + 1, length(aux) + 1)
            l <- lapply(1:(length(brks) - 1), function(x) {
                  a <- yrs[brks[x] : (brks[x + 1] - 1)]
                  return(rep(yy[x], length(a)))
            })
            yrs  <- do.call("c", l)
      }
      return(yrs)
}
# End


#' @title POSIXlt conversion from character 
#' @description Converts the date codes of the Value format to \code{"POSIXlt"}
#' @param timeString Date vector as stored in VALUE files, previously coerced to character
#' @param tz Time zone. See \code{\link{loadValueStations}}
#' @return A POSIXlt vector of the same length of the input
#' @details Currently the VALUE format is intended for daily data of the form YYYMMDD. However,
#'  the function also considers the possibility of subdaily data if hourly data are introduced in
#'  the form YYYYMMDDHH, eading to a string of 10 characters.
#'  @keywords internal
#'  @author juaco

string2date <- function(timeString, tz = tz) {
      timeString = gsub("^\\s+|\\s+$", "", timeString)
      if (nchar(timeString[1]) == 8) {
            timeDates <- strptime(timeString, "%Y%m%d", tz = tz)  
      }
      if (nchar(timeString[1]) == 10) {
            timeDates <- strptime(timeString, "%Y%m%d%H", tz = tz)
      }
      return(timeDates)
}
# End



#' @title Compute time bounds
#' @description Compute start/end verification time bounds from a vector of dates.
#' @param timeDates A POSIXlt vector of dates
#' @param tz Time zone
#' @keywords internal
#' @return A list with components start and end, of POSIXct dates

timeBoundsValue <- function(timeDates, tz) {
      varTimeStep <- difftime(timeDates[2], timeDates[1])
      dateSliceStart <- as.POSIXct(timeDates)
      dateSliceEnd <- as.POSIXct(as.POSIXlt(timeDates + varTimeStep))
      usetz <- ifelse(identical(tz, ""), FALSE, TRUE)
      dateSliceStart <- format.POSIXct(dateSliceStart, "%Y-%m-%d %H:%M:%S", usetz = usetz, tz = tz)
      dateSliceEnd <- format.POSIXct(dateSliceEnd, "%Y-%m-%d %H:%M:%S", usetz = usetz, tz = tz)
      return(list("start" = dateSliceStart, "end" = dateSliceEnd))
}
# End


#' @title getIntersect
#' @description Temporal and spatial matching between obs and pred
#' @param obs Value object of observations
#' @param prd Value object of predictions
#' @return A list with obj and pred intersected
#' @author S. Herrera
#' @keywords internal

getIntersect <- function(obs, prd){
      obj <- list(obs = obs, prd = prd)
      obj$Dates$start <- intersect(obs$Dates$start,prd$Dates$start)
      obj$Dates$end <- intersect(obs$Dates$end,prd$Dates$end)
      datesValidation <- intersect(obs$Dates$start,prd$Dates$start)
      idValidation <-  intersect(attr(obs$xyCoords, "dimnames")[[1]],attr(prd$xyCoords, "dimnames")[[1]])
      dimObs <- dim(obs$Data)
      obs.time.index <- grep("^time$", attr(obs$Data, "dimensions"))
      obs.station.index <- grep("^station$", attr(obs$Data, "dimensions"))
      indObs <- which(is.element(obs$Dates$start, datesValidation))
      indObsId <- which(is.element(attr(obs$xyCoords, "dimnames")[[1]], idValidation))
      obj$xyCoords <- obs$xyCoords[which(is.element(attr(obs$xyCoords, "dimnames")[[1]], idValidation)),]
      indVal <- rep(list(bquote()), length(dimObs))
      for (d in 1:length(dimObs)) {
            indVal[[d]] <- 1:dimObs[d]
      }
      indVal[[obs.time.index]] <- indObs
      indVal[[obs.station.index]] <- indObsId
      callObs <- as.call(c(list(as.name("["),quote(obs$Data)), indVal))
      obj$obs$Data <- eval(callObs)
      attr(obj$obs$Data, "dimensions") <- attr(obs$Data, "dimensions")
      obj$obs$Dates$start <- obs$Dates$start[indObs]
      obj$obs$Dates$end <- obs$Dates$end[indObs]
      obj$obs$xyCoords <- obs$xyCoords[indObsId,]
      dimPrd <- dim(prd$Data)
      prd.time.index <- grep("^time$", attr(prd$Data, "dimensions"))
      prd.station.index <- grep("^station$", attr(prd$Data, "dimensions"))
      indPrd <- which(is.element(prd$Dates$start, datesValidation))
      indPrdId <- which(is.element(attr(prd$xyCoords, "dimnames")[[1]], idValidation))
      indVal <- rep(list(bquote()), length(dimPrd))
      for (d in 1:length(dimPrd)) {
            indVal[[d]] <- 1:dimPrd[d]
      }
      indVal[[prd.time.index]] <- indPrd
      indVal[[prd.station.index]] <- indPrdId
      callPrd <- as.call(c(list(as.name("["),quote(prd$Data)), indVal))
      obj$prd$Data <- eval(callPrd)
      attr(obj$prd$Data, "dimensions") <- attr(prd$Data, "dimensions")
      obj$prd$Dates$start <- prd$Dates$start[indPrd]
      obj$prd$Dates$end <- prd$Dates$end[indPrd]
      obj$prd$xyCoords <- prd$xyCoords[indPrdId,]
      return(obj)
}

#' @title Complete missing dimensions of VALUE objects
#' @description Inverse of drop to complete all dimensions of the Data array
#' @param A VALUE R object
#' @return The same object with all the dimensions (i.e. member, time, station)
#' @keywords internal
#' @author juaco

dimFix <- function(valueObj) {
      # Add fake 'station' dimension to single-station datasets
      if (!("station" %in% attr(valueObj$Data, "dimensions"))) {
            dimNames <- c(attr(valueObj$Data, "dimensions"), "station")
            if (length(attr(valueObj$Data, "dimensions")) == 2) { # "member","time"
                  perm <- c(2,3,1)
            } else {# "time"
                  perm <- c(2,1)
            }
            valueObj$Data <- unname(aperm(abind(valueObj$Data, NULL, along = 0), perm = perm))
            attr(valueObj$Data, "dimensions") <- dimNames
      }
      # Add fake member dimension to deterministic/obs
      if (!("member" %in% attr(valueObj$Data, "dimensions"))) {
            dimNames <- c("member", attr(valueObj$Data, "dimensions"))
            valueObj$Data <- unname(abind(valueObj$Data, NULL, along = 0))    
            attr(valueObj$Data, "dimensions") <- dimNames
      }
      return(valueObj)
}
