#     jointProbMat.VALUE.R Joint probability analysis
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
#' @title Joint probability analysis
#' @description Computes different joint (precipitation) probabilities between stations 
#' @param stationObj An R-VALUE object containing station data (as returned by \code{\link{loadValueStations}}).
#' @param predictions.file Path to the file storing the predictions (passed to \code{\link{loadValuePredictions}}).
#'  Default to NULL, meaning that the correlation matrix is done on the observations.
#' @param season Character string indicating the target season. Accepted values are 
#' \code{c("annual", "DJF", "MAM", "JJA", "SON")}
#' @param aggr.type Type of aggregation in the case of multiple realizations. Should the aggregation of 
#' multiple members be performed \code{"before"} (default) or \code{"after"} computing the correlations?. Ignored
#'  in the case of observations and deterministic predictions.
#' @param prob.type Character vector indicating the type of probability to be computed. Currently accepted values are:
#' \code{"DD"} for dry-dry, \code{"DW"} for dry-wet, \code{"WW"} for wet-wet and \code{"WD"} for wet-dry.
#' @param threshold Threshold above which values are used/discarded (i.e., values greater or equal than \code{threshold} are considered as Wet).
#' @param max.na.prop Maximum allowed proportion of missing data (Default to 0.25). See details
#' @param use.ff Optional. If set to \code{TRUE}, the loaded stations/predictions array is written to disk using the \pkg{ff} package in order to avoid 
#' memory problems. Recommended when working with large stochastic prediction datasets in resource-limited machines.
#' @return A 2D matrix. Attributes indicate the station names (in the row/column order they appear), and their
#' geographical coordinates. In addition, the type of joint probability, as coded in argument \code{prob.type} is indicated in attribute \code{"joint_prob_type"}.   
#' @author J. Bedia 
#' @export
#' @importFrom ff as.ff


jointProbMat.VALUE <- function(stationObj,
                               predictions.file = NULL,
                               season = c("annual", "DJF", "MAM", "JJA", "SON"),
                               aggr.type = c("before", "after"),
                               prob.type = c("DD", "DW", "WW", "WD"),
                               threshold = 1,
                               max.na.prop = 0.25,
                               use.ff = FALSE) {
      season <- match.arg(season, choices = c("annual", "DJF", "MAM", "JJA", "SON"))
      season <- switch(season,
                       "annual" = 1:12,
                       "DJF" = c(12,1,2),
                       "MAM" = 3:5,
                       "JJA" = 6:8,
                       "SON" = 9:11)
      prob.type <- match.arg(prob.type, choices = c("DD", "DW", "WW", "WD"))
      ineq1 <- substr(prob.type, 1, 1)
      ineq2 <- substr(prob.type, 2, 2)
      ineqs <- sapply(c(ineq1, ineq2), function(x) switch(x, "D" = "<", "W" = ">="))
      expr1 <- paste("which(mat[i,,j]", ineqs[1], "threshold)")
      expr2 <- paste("sum(x", ineqs[2], "threshold, na.rm = TRUE) / length(which(!is.na(x)))")
      aggr.type <- match.arg(aggr.type, choices = c("before", "after"))
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
      if (aggr.type == "before") {
            if (n.mem > 1) message("[", Sys.time(), "] - Aggregating members before computing correlation...")
            o$Data <- apply(o$Data, MARGIN = c(2,3), FUN = mean, na.rm = TRUE)
            attr(o$Data, "dimensions") <- c("time", "station")
            o <- dimFix(o)
            if (n.mem > 1) message("[", Sys.time(), "] - Done.")
            n.mem <- dim(o$Data)[1]
      }
      n.stations <- dim(o$Data)[3]
      mat <- if (use.ff) {
            as.ff(o$Data)
      } else {
            o$Data
      }
      o$Data <- NULL
      # Joint probability matrices
      message("[", Sys.time(), "] - Calculating probabilities...")
      jp.list <- lapply(1:n.mem, function(i) {
            jpmat <- matrix(nrow = n.stations, ncol = n.stations)
            for (j in 1:n.stations) {
                  ind <- eval(parse(text = expr1))
                  prob <- length(na.omit(ind)) / length(na.omit(mat[i,,j]))
                  aux <- if (length(ind) > 1) {
                        mat[i,ind,setdiff(1:n.stations, j)]
                  } else if (length(ind) == 1) {
                        warning("Only 1 record fulfilling the threshold condition at station ", j, " (member ", i,"): 1/0 Probabilities returned.", call. = FALSE)
                        t(as.matrix(mat[i,ind,setdiff(1:n.stations,j)]))
                  } else if (length(ind) == 0) {
                        warning("No records fulfilling the threshold condition at station ", j, " (member ", i,"): NaNs returned.", call. = FALSE)
                        matrix(NaN, nrow = 1, ncol = n.stations - 1)
                  }
                  jpmat[j,setdiff(1:n.stations,j)] <- apply(aux, MARGIN = 2, FUN = function(x) eval(parse(text = expr2))*prob)
            }
            return(jpmat)
      })
      if (use.ff) close(mat)
      arr <- do.call("abind", c(jp.list, along = -1L))
      jp.list <- NULL
      # Member aggregation "after"
      if (aggr.type == "after") message("[", Sys.time(), "] - Aggregating members...")
      jpmat <- unname(apply(arr, MARGIN = c(2,3), FUN = mean, na.rm = TRUE))
      arr <- NULL
      attr(jpmat, "joint_prob_type") <- prob.type
      attr(jpmat, "station_names") <- o$Metadata$name
      attr(jpmat, "lon") <- unname(o$xyCoords[,1])
      attr(jpmat, "lat") <- unname(o$xyCoords[,2])
      message("[", Sys.time(), "] - Done.")
      return(jpmat)
}





