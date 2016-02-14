#' @title Remove missing data stations
#' @description Remove stations over a user-defined proportion of missing data from a R.VALUE multi-station object, 
#' @param valueObj A VALUE R object, but see Details.
#' @param max.na.prop Maximum allowed proportion of missing data (Default to 0, i.e., no missing values are allowed)
#' @return A R.VALUE object without the stations above the user-defined threshold of missing data
#' @details The function assumes that all dimensions are present (i.e., function dimFix has been applied)
#' @export
#' @author J Bedia
#' @example \dontrun{
#' # Load ECA-VALUE-86 observational dataset
#' dataset <- file.path(find.package("R.VALUE"),
#'                      "example_datasets",
#'                      "VALUE_ECA_86_v2.zip")
#' obs <- loadValueStations(dataset, var = "tmin")
#' attr(obs$Data, "dimensions")
#' # Add fake member dimension for structural integrity
#' obs2 <- dimFix(obs)
#' attr(obs2$Data, "dimensions")
#' dim(obs2$Data)
#' # Retain stations without missing values (default)
#' na.omit.obs <- na.omit.VALUE(obs2)      
#' attributes(na.omit.obs) # 51 stations removed (35 preserved)
#' str(na.omit.obs)
#' # Remove only those with less than 25% missing data
#' na.omit.obs2 <- na.omit.VALUE(obs2, max.na.prop = .25)      
#' str(na.omit.obs2) # Only 2 stations discarded
#' }

na.omit.VALUE <- function(valueObj, max.na.prop = 0) {
      dimNames <- attr(valueObj$Data, "dimensions")
      if (!identical(dimNames, c("member", "time", "station"))) {
            stop("Incompatible dimensions: consider using dimFix first")
      }
      n.stations <- dim(valueObj$Data)[3]
      rm.ind <- rep(NA, n.stations)
      for (i in 1:n.stations) {
            if ((length(which(is.na(valueObj$Data[1,,i]))) / dim(valueObj$Data)[2]) > max.na.prop) rm.ind[i] <- i
      }
      rm.ind <- na.omit(rm.ind)
      if (length(rm.ind) > 0) {
            if (length(rm.ind) == n.stations) stop("No stations available fulfilling the max.na.prop condition")
            valueObj$Data <- valueObj$Data[,,-rm.ind]
            valueObj$xyCoords <- valueObj$xyCoords[-rm.ind,]
            rm.stids <- valueObj$Metadata$station_id[rm.ind]
            valueObj$Metadata <- sapply(names(valueObj$Metadata), function(x) valueObj$Metadata[[x]][-rm.ind],
                                        USE.NAMES = TRUE, simplify = FALSE)
            attr(valueObj, "na.omitted.stationIDs") <- rm.stids
            attr(valueObj, "max.na.prop") <- max.na.prop
            attr(valueObj$Data, "dimensions") <- dimNames
      }
      return(valueObj)
}
