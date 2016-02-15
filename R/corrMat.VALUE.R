#     corrMat.VALUE.R orrelation matrix for paper Figures 4 and 5 on spatial validation
#     
#     Copyright (C) 2016 Santander Meteorology Group (http://www.meteo.unican.es)
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
#' @title Correlation matrix for paper Figures 4 and 5 on spatial validation
#' @description Computes the cross correlation matrices between stations that serve as input for plotting functions
#' @param stationObj An R-VALUE object containing station data (as returned by \code{\link{loadValueStations}}).
#' @param predictions.file Path to the file storing the predictions (passed to \code{\link{loadValuePredictions}}).
#'  Default to NULL, meaning that the correlation matrix is done on the observations.
#' @param season Character string indicating the target season. Accepted values are 
#' \code{c("annual", "DJF", "MAM", "JJA", "SON")}
#' @param method Correlation method, passed to the \code{\link{cor}} function
#' @param type Type of aggregation in the case of multiple realizations. Should the aggregation of 
#' multiple members be performed \code{"before"} or \code{"after"} computing the correlations?. Ignored
#'  in the case of observations and deterministic predictions.
#' @param max.na.prop Maximum allowed proportion of missing data (Default to 0.25). See details
#' @param deseason Logical. Should the seasonal cycle from the input data series be removed?. See details.
#' @param window.width Used if \code{deseason = TRUE}. Integer number indicating the width, in days, of the window used for
#'  moving average computation of the reference daily climatology. Default to 60 days. See details.
#' @details A (circular) moving average daily climatology can be automatically calculated for each data series, considering 
#' a specific window width (60 days by default, centered around lag 0). This is used for removing the seasonal cycles when applicable. 
#' An additional argument allows to specify the maximum number of missing values allowed, being ommited from the analysis 
#' those data series above the threshold.
#' @return A 2D matrix. Attributes indicate the station names (in the row/column order they appear), and their
#' geographical coordinates.   
#' @author J. Bedia 
#' @export

corrMat.VALUE <- function(stationObj,
                          predictions.file = NULL,
                          season = c("annual", "DJF", "MAM", "JJA", "SON"),
                          method = c("pearson", "kendall", "spearman"),
                          type = c("before", "after"),
                          max.na.prop = 0.25,
                          deseason = TRUE,
                          window.width = 60) {
      season <- match.arg(season, choices = c("annual", "DJF", "MAM", "JJA", "SON"))
      season <- switch(season,
                       "annual" = 1:12,
                       "DJF" = c(12,1,2),
                       "MAM" = 3:5,
                       "JJA" = 6:8,
                       "SON" = 9:11)
      method <- match.arg(method, choices = c("pearson", "kendall", "spearman"))
      type <- match.arg(type, choices = c("before", "after"))
      o <- stationObj
      stationObj <- NULL
      if (!is.null(predictions.file)) {
            message("[", Sys.time(), "] - Loading predictions...")
            o <- suppressWarnings(loadValuePredictions(o, predictions.file))
            message("[", Sys.time(), "] - Done.")            
      }      
      o <- na.filter.VALUE(dimFix(subsetVALUE(o, season = season)), max.na.prop)
      n.mem <- dim(o$Data)[1]
      # Member aggregation before
      if (type == "before") {
            if (n.mem > 1) message("[", Sys.time(), "] - Aggregating members before computing correlation...")
            o$Data <- apply(o$Data, MARGIN = c(2,3), FUN = mean, na.rm = TRUE)
            attr(o$Data, "dimensions") <- c("time", "station")
            o <- dimFix(o)
            if (n.mem > 1) message("[", Sys.time(), "] - Done.")
            n.mem <- dim(o$Data)[1]
      }
      if (deseason) {
            message("[", Sys.time(), "] - Removing seasonal cycle...")
            n.stations <- dim(o$Data)[3]
            dates <- as.POSIXlt(o$Dates$start)
            doy <- format(dates, format = "%j")
            fil <- rep(1/window.width, window.width + 1)
            for (i in 1:n.mem) {
                  mat <- o$Data[i,,]
                  for (j in 1:n.stations) {
                        # Moving average, circular, centered on lag 0
                        x <- filter(mat[,j], filter = fil, circular = TRUE, sides = 2)
                        # Daily climatology
                        ref <- tapply(x, INDEX = doy, FUN = mean, na.rm = TRUE)
                        # Remove cycle
                        for (k in 1:length(ref)) {
                              ind <- which(doy == unique(doy)[k])
                              x[ind] <- x[ind] - ref[k]
                        }
                        o$Data[i,,j] <- x
                  }
            }
            message("[", Sys.time(), "] - Done.")
      }
      # Correlation matrices for each member
      cor.mat.list <- lapply(1:n.mem, function(x) cor(o$Data[x,,], use = "pairwise.complete.obs", method = method))
      o$Data <- NULL
      arr <- do.call("abind", c(cor.mat.list, along = -1L))
      # Member aggregation "after"
      cormat <- apply(arr, MARGIN = c(2,3), FUN = mean, na.rm = TRUE)
      attr(cormat, "station_names") <- o$Metadata$name
      attr(cormat, "lon") <- o$xyCoords[,1]
      attr(cormat, "lat") <- o$xyCoords[,2]
      return(cormat)
}









