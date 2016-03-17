#     wrapperFUN.R Wrapper function to launch the validation
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
#' @title Wrapper function to launch the validation
#' @description Launches the VALUE validation framework according to the arguments passed by the database.
#' @author J. Bedia, D. San Martin, M. Tuni
#' @param metric Character vector.
#' @param names Character vector of the same length than \code{metric}. Names of the indices/measures to be applied
#' @param season Character vector defining the target season(s). Default to annual + 4 standard seasons.
#' @param member.aggregation Character vector of length one. What aggregation function should be applied to multipe realizations
#' before computing the indices?. Default to \code{"none"}, meaning that the indices are computed in a member-wise basis, and only
#'  after that the index values are aggregated to compute the measure. The only additional option currently used
#'   is \code{"mean"}, for cases when the realizations are averaged before computing the index. 
#'  Ignored for observations and deterministic predictions.
#' @param index.fun A character vector with the name of the R function that computes the index.
#' @param measure.fun A character vector with the name of the R function that computes the measure.
#' @param index.args A list with additional arguments passed to \code{index.fun}. It contains a key-value list for each additional argument.
#' @param measure.args Same as \code{index.args} but for the measure function.
#' @param o R object containing the observations as returned by \code{\link{loadValueStations}}. 
#' @param p R object containing the predictions as returned by \code{\link{loadValuePredictions}}.
#' @param na.prop Maximum allowable proportion of missing data. Default to 0.9
#' @return A 3D array with labelled dimensions station, season and metric
#' @importFrom abind abind
#' @export
#' @examples \dontrun{
#' # Load observations
#' obs.file <- file.path(find.package("R.VALUE"), "example_datasets", "VALUE_ECA_86_v2.zip")
#' o <- loadValueStations(obs.file, var = "tmin")
#' prdfile <- list.files(file.path(find.package("R.VALUE"), "example_datasets"),
#'                       pattern = "predictions_portal_exp1a_deterministic",
#'                       full.names = TRUE)
#' # Load predictions
#' p <- loadValuePredictions(o, predictions.file = prdfile)
#' # Create argument list for computing mean bias:
#' c1 = list()
#' c1$metric = c("obs", "pred", "measure")
#' c1$names = c("obsMean", "predMean","meanBias")
#' c1$season = c("annual", "DJF", "MAM", "JJA", "SON")
#' c1$member.aggregation = "none"
#' c1$index.fun = "index.mean.R"
#' c1$measure.fun = "measure.bias.R"
#' c1$index.args = NULL
#' c1$measure.args = NULL
#' c1$o = o
#' c1$p = p
#' c1$na.prop = .9
#' 
#' # Call validation wrapper
#' a <- do.call("wrapperFUN", c1)
#' str(a)
#' # Example stochastic
#' prdfile <- list.files(file.path(find.package("R.VALUE"), "example_datasets"),
#'                       pattern = "predictions_portal_exp1a_stochastic",
#'                       full.names = TRUE)
#' # Load predictions
#' p <- loadValuePredictions(o, predictions.file = prdfile)
#' c1$p = p
#' 
#' # Member-wise index calculation:
#' c1$member.aggregation = "none"
#' b <- do.call("wrapperFUN", c1)
#' str(b)
#' # Note the attribute 'member.aggregation'
#' 
#' # Member aggregation before computing the index (deterministic component):
#' c1$member.aggregation = "mean"
#' b1 <- do.call("wrapperFUN", c1)
#' str(b1)
#' }


wrapperFUN <- function(metric = c("obs", "pred", "measure"),
                       names = NULL,
                       season = c("annual", "DJF", "MAM", "JJA", "SON"),
                       member.aggregation = "none",
                       index.fun = NULL,
                       measure.fun = NULL,
                       index.args = NULL,
                       measure.args = NULL,
                       o = o,
                       p = p,
                       na.prop = 1) {
      metric <- match.arg(arg = metric, choices = c("obs", "pred", "measure"), several.ok = TRUE)
      season <- match.arg(arg = season, choices = c("annual", "DJF", "MAM", "JJA", "SON"), several.ok = TRUE)
      suffix <- "\\.R$|\\.r$"
      index.fun <- if (!is.null(index.fun) && grepl(suffix, index.fun)) gsub(suffix, "", index.fun)
      measure.fun <- if (!is.null(measure.fun) && grepl(suffix, measure.fun)) gsub(suffix, "", measure.fun)
      message("[", Sys.time(), "] Intersecting obs and pred...")
      o <- dimFix(o)
      p <- dimFix(p)
      int <- getIntersect(o,p)
      o <- int$obs
      p <- int$prd
      int <- NULL
      message("[", Sys.time(), "] OK")
      
      # Member aggregation (the array is re-assigned the member dimension after the aggregation)
      if (member.aggregation != "none" & dim(p$Data)[1] > 1) {
            message("[", Sys.time(), "] Aggregating members...")
            dimNames <- attr(p$Data, "dimensions")
            p$Data <- apply(p$Data,
                            MARGIN = grep("member", attr(p$Data, "dimensions"), invert = TRUE),
                            FUN = member.aggregation, na.rm = TRUE)
            p$Data <- unname(abind(p$Data, NULL, along = 0))    
            attr(p$Data, "dimensions") <- dimNames
            # attr(p$Data, "member.aggr.fun") <- member.aggregation
            message("[", Sys.time(), "] OK")
      }
      n.st <- dim(o$Data)[3]
      n.mem <- dim(p$Data)[1]
      n.metric <- length(metric)
      n.seas <- length(season)
      index.arr <- array(dim = c(n.st, n.seas, n.metric), dimnames = list("station_id" = o$Metadata$station_id,"season" = season,"metric_name" = names))
      attr(index.arr, "var") <- o$Variable$varName
      attr(index.arr, "member.aggregation") <- member.aggregation
      attr(index.arr, "max.na.prop") <- na.prop
      attr(index.arr, "index.fun") <- index.fun
      attr(index.arr, "index.args") <- index.args
      attr(index.arr, "measure.fun") <- measure.fun
      attr(index.arr, "measure.args") <- measure.args
      for (i in 1:n.st) {
            message("[", Sys.time(), "] Processing data for station \"", o$Metadata$station_id[i], "\"")
            st.o <- subsetVALUE(o, stationID = o$Metadata$station_id[i])
            st.p <- subsetVALUE(p, stationID = p$Metadata$station_id[i])
            for (j in 1:n.seas) {
                  seas <- switch(season[j],"annual" = 1:12,"DJF" = c(12,1,2),"MAM" = 3:5,"JJA" = 6:8,"SON" = 9:11)
                  sea.o <- subsetVALUE(st.o, season = seas)
                  sea.p <- subsetVALUE(st.p, season = seas)
                  # Vectorization
                  obs <- as.matrix(drop(sea.o$Data))
                  prd <- as.matrix(drop(sea.p$Data))
                  if (n.mem > 1) prd <- t(prd)
                  dates.obs <- sea.o$Dates$start
                  dates.pred <- sea.p$Dates$start
                  sea.o <- sea.p <- NULL
                  # NA filter --------
                  aux.list <- lapply(1:ncol(prd), function(x) preprocessVALUE(obs[,1], prd[,x], dates.obs, dates.pred, na.prop))
                  for (k in 1:n.metric) {
                        if (any(is.na(aux.list[[1]]$obs))) {
                              index.arr[i,j,k] <- NA
                        } else {
                              ind <- grep(metric[k], names(aux.list[[1]]))
                              if (length(ind) == 0) {# measure -----
                                    if (k > 1) {
                                          indexObs <- index.arr[i,j,"obs"]
                                          indexPrd <- index.arr[i,j,"pred"]
                                    } else {
                                          indexObs <- indexPrd <- NULL
                                    }
                                    aux <- rep(NA, n.mem)
                                    for (l in 1:n.mem) {
                                          arg.list <- list("indexObs" = indexObs,"indexPrd" = indexPrd,"obs" = aux.list[[l]]$obs,"prd" = aux.list[[l]]$pred)
                                          if (!is.null(measure.args)) {
                                                arg.list <- c(arg.list,measure.args)
                                          }
                                          aux[l] <- do.call(measure.fun, args = arg.list, quote = TRUE)   
                                    }
                                    index.arr[i,j,k] <- mean(aux, na.rm = TRUE)
                              } else {# index -----
                                    aux <- rep(NA, n.mem)
                                    for (l in 1:n.mem) {  
                                          ind <- grep(metric[k], names(aux.list[[l]]))
                                          arg.list <- list("ts" = aux.list[[l]][[ind]])
                                          if (!is.null(index.args)) {
                                                arg.list <- c(arg.list,index.args)
                                                # Subroutine for passing dates ----
                                                if ("dates" %in% names(arg.list)) {
                                                      arg.list$dates <- aux.list[[l]]$dates
                                                }
                                          }
                                          aux[l] <- do.call(index.fun, arg.list)
                                    }      
                                    index.arr[i,j,k] <- mean(aux, na.rm = TRUE)
                              }
                        }
                  }
                  aux.list <- NULL
            }
            st.o <- st.p <- NULL
      }
      return(index.arr)
}



#' @title getIntersect
#' @description Temporal and spatial matching between obs and pred
#' @param obs Value object of observations
#' @param prd Value object of predictions
#' @return A list with obj and pred intersected
#' @author S. Herrera, D. San-Martin
#' @keywords internal

getIntersect <- function(obs, prd){
  obj <- list()
  commonStartDates <- intersect(obs$Dates$start,prd$Dates$start)
  commonEndDates <- intersect(obs$Dates$end,prd$Dates$end)
  commonStations <- intersect(attr(obs$xyCoords, "dimnames")[[1]],attr(prd$xyCoords, "dimnames")[[1]])
  obj$obs <- subsetData(obs,commonStartDates, commonEndDates, commonStations)
  obj$prd <- subsetData(prd,commonStartDates, commonEndDates, commonStations)
  return(obj)
}

#' @title subsetData
#' @description Time and stations subsetting
#' @param data Value object of observations/predictions
#' @param startDates Start dates
#' @param endDates End dates
#' @param stations Stations
#' @return A list with obj and pred intersected
#' @author S. Herrera
#' @keywords internal

subsetData <- function(obj, startDates, endDates, stations){
  result = list()
  idStartDates <- which(is.element(obj$Dates$start, startDates))
  idEndDates <- which(is.element(obj$Dates$end, endDates))
  idStations <- which(is.element(attr(obj$xyCoords, "dimnames")[[1]], stations))
  result$Dates$start <- obj$Dates$start[idStartDates]
  result$Dates$end <- obj$Dates$end[idEndDates]
  result$xyCoords <- obj$xyCoords[idStations,]
  result$Metadata$source <- obj$Metadata$source[idStations]
  result$Metadata$altitude <- obj$Metadata$altitude[idStations]
  result$Metadata$name <- obj$Metadata$name[idStations]
  result$Metadata$station_id <- obj$Metadata$station_id[idStations]
  result$Data <- obj$Data[,idStartDates,idStations,drop=FALSE]
  attr(result$Data, "dimensions") <- attr(obj$Data, "dimensions")
  return(result)
}

#' @title Complete missing dimensions of VALUE objects
#' @description Inverse of drop to complete all dimensions of the Data array
#' @param A VALUE R object
#' @return The same object with all the dimensions (i.e. member, time, station)
#' @keywords internal
#' @importFrom abind abind
#' @author J. Bedia

dimFix <- function(valueObj) {
  # Add fake 'station' dimension to single-station datasets
  if (!("station" %in% attr(valueObj$Data, "dimensions"))) {
    dimNames <- c(attr(valueObj$Data, "dimensions"), "station")
    perm <- if (length(attr(valueObj$Data, "dimensions")) == 2) { # "member","time"
      c(2,3,1)
    } else {# "time"
      c(2,1)
    }
    valueObj$Data <- unname(aperm(abind(valueObj$Data, NULL, along = 0), perm = perm))
    attr(valueObj$Data, "dimensions") <- dimNames
  }
  # Add fake member dimension to deterministic/obs
  if (!("member" %in% attr(valueObj$Data, "dimensions"))) {
    dimNames <- c("member", attr(valueObj$Data, "dimensions"))
    valueObj$Data <- unname(abind(valueObj$Data, NULL, along = -1))    
    attr(valueObj$Data, "dimensions") <- dimNames
  }
  return(valueObj)
}


#' @title preprocessVALUE
#' @description Preprocessing of input data for index calculation routines
#' @param obs A time series (vector) of observations
#' @param pred A time series (vector) of predictions
#' @param dates.obs Calendar dates corresponding to the values in \code{obs}
#' @param dates.pred Calendar dates corresponding to the values in \code{pred}
#' @param na.prop Maximum allowable proportion of missing values.
#' @details The function performs missing data filtering, temporal matching and (optionally) seasonal subsetting 
#' @return A list with preprocessed \code{obs}, \code{pred} and \code{dates}, as passed to the index.* routines
#' @keywords internal
#' @author J. Bedia


preprocessVALUE <- function(obs, pred, dates.obs, dates.pred, na.prop) {
      # Temporal matching
      ind.obs <- which(is.element(dates.obs, dates.pred))
      ind.pred <- which(is.element(dates.pred, dates.obs))
      if (length(ind.obs) == 0) stop("No temporal matching between observations and predictions")
      obs <- obs[ind.obs]
      dates <- dates.obs[ind.obs]
      pred <- pred[ind.pred]
      # NA filtering
      na.ind <- union(which(is.na(obs)), which(is.na(pred)))
      # Datos utiles
      if (length(na.ind) == length(obs)) {
            obs <- pred <- dates <- NA 
      } else {
            if  (length(na.ind) > 0) {
                  na.prop.data <- round(length(na.ind) / length(obs), 2)
                  if (na.prop.data > na.prop) {
                        obs <- pred <- dates <- NA 
                  } else {
                        obs <- obs[-na.ind]
                        pred <- pred[-na.ind]
                        dates <- dates[-na.ind]
                  }
            }
      }
      list("obs" = obs, "pred" = pred, "dates" = dates)
}
