#' @title Get season from a station or field object
#' @description Retrieves the season encompassed by a station or field object
#' @param objDates Dates as directly extracted from any station or predictions object
#' @return An integer vector with the season
#' @author juaco
#' @keywords internal

getSeason.VALUE <- function(objDates) {
      aux <- as.numeric(substr(objDates,6,7))
      return(unique(aux))
}
# End

#' @title Get years as a factor
#' @description Extract the year as a factor (e.g. for computing annual statistics)
#' @param obj Any object extending the station or field classes
#' @return A vector of years of the same length as the time dimension of the object, 
#' seasonally-adjusted in the case of year-crossing seasons (e.g. DJF). See details.
#' @details The function performs a very basic operation, extracting the year element from the 
#' dates. The trick lies in the year-crossing seasons. For instance:
#'  by convention, winter 2001 encompasses December 2000 and January, February 2001. Therefore, in order to compute
#' annual statistics for a year-crossing season, it is necessary to modify first the vector of years, 
#' and assign year 2001 to the preceding December. Similarly, the next December 2001 belongs to winter 2002,
#'  and so on... The function is useful for computing and/or plotting annual statistics, seasonal climatologies ... 
#' @section Warning:
#' The function should no be used to extract the actual years vector
#' @author juaco
#' @keywords internal

getYearsAsINDEX.VALUE <- function(objDates) {
      season <- getSeason.VALUE(objDates)
      yrs <- as.numeric(substr(objDates,1,4))
      mon <- as.numeric(substr(objDates,6,7))
      if (!identical(season, sort(season))) {
            yy <- unique(yrs)[-1]
            aux <- match(mon, season)
            brks <- c(1, which(diff(aux) < 0) + 1, length(aux) + 1)
            l <- lapply(1:(length(brks) - 1), function(x) {
                  a <- yrs[brks[x]:(brks[x + 1] - 1)]
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


