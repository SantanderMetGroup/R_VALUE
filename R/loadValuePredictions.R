#' @title Load VALUE predictions data
#' 
#' @description Load predictions data, either deterministic (one single txt file) or stochastic (several
#'  realizations stored in sepparate txt files, bundled in a single zip file).
#' 
#' @param stationObj A station data object returned by \code{\link{loadValueStations}}. See details.
#' @param predictions.file path to the file (either text or zip) containing the predictions.
#' @param tz Optional. A time zone specification to be used for the conversion of dates. See more details in 
#' \code{\link{loadValueStations}}.
#' 
#' @return A predictions object. This is equivalent to the stations object (see \code{\link{loadValueStations}}),
#' but the data element may vary its shape to include the \code{"member"} dimension in case of stochastic
#' predictions with several realizations. Also, a global attribute \code{"datatype"} is set, and assigned 
#' the value \code{"predictions"} to differentiate it from observations data.
#' 
#' @details The idea of this function is to rely on the previously loaded observations data, so all
#'  the information required to define the predictions subset (time and station codes) is retrieved from 
#'  the observations data subset.
#'  
#' @export
#' 
#' @importFrom utils unzip
#' @importFrom abind abind
#'  
#' @family loading
#'

loadValuePredictions <- function(stationObj, predictions.file, tz = "") {
      stationObj$Data <- NULL
      season <- getSeason(stationObj)
      years <- unique(getYearsAsINDEX(stationObj))
      stids <- stationObj$Metadata$station_id
      dataset <- predictions.file
      # Multimember case, ZIP file
      if (grepl("\\.zip$", dataset)) {
            zipFileContents <- unzip(dataset, list = TRUE)$Name
            n.members <- length(zipFileContents)
            timeString <- read.csv(unz(dataset, zipFileContents[1]), colClasses = "character")[ , 1]
            timeDates <- string2date(timeString, tz)
            timeString <- NULL
            timePars <- getTimeDomainValueStations(timeDates, season, years)
            if (length(intersect(timePars$timeDates, as.POSIXlt(stationObj$Dates$start))) == 0) {
                  stop("Temporal mismatch between predictions and observations")
            }
            colNums <- match(stids, names(read.csv(unz(dataset, zipFileContents[1]))))
            member.list <- lapply(1:n.members, function(x) {
                  read.csv(unz(dataset, zipFileContents[x]))[timePars$timeInd, colNums]
            })            
            aux <- drop(do.call("abind", c(member.list, along = -1)))
            member.list <- NULL
      } else { # Deterministic case, txt file      
            n.members = 1
            timeString <- read.csv(dataset, colClasses = "character")[ , 1]      
            timeDates <- string2date(timeString, tz)
            timeString <- NULL
            timePars <- getTimeDomainValueStations(timeDates, season, years)
            if (length(intersect(timePars$timeDates, as.POSIXlt(stationObj$Dates$start))) == 0) {
                  stop("Temporal mismatch between predictions and observations")
            }
            colNums <- match(stids, names(read.csv(dataset)))
            aux <- read.csv(dataset)[timePars$timeInd, colNums]
      }
      # Set the dimensions attribute
      if (is.null(dim(aux))) {
            aux <- as.matrix(aux)
      }
      if (length(dim(aux) == 2)) {
            dimensions <- c("time", "station")
      } else {
            dimensions <- c("member", "time", "station")
      }
      aux[which(aux == -9999)] <- NA
      attr(aux, "dimensions") <- dimensions
      stationObj$Data <- aux
      attr(stationObj, "datatype") <- "predictions"
      return(stationObj)
}
# End

