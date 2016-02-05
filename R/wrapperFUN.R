#' @title Wrapper function to launch the validation
#' @description Launches the VALUE validation framework according to the arguments passed by the 
#' @author J. Bedia, D. San Martin
#' @param metric Character vector.
#' @param names Character vector of the same length than \code{metric}. Names of the indices/measures to be applied
#' @param season Character vector defining the target season(s). Default to annual + 4 standard seasons.
#' @param aggr.type Character vector of length one. Should member aggregation be performed \code{"before"} or \code{"after"} the validation?.
#' Ignored for observations and deterministic predictions.
#' @param aggr.fun Character vector of length one. Multimember aggregation function. Default to \code{"mean"}.
#' @param index.fun A character vector with the name of the R function that computes the index.
#' @param measure.fun A character vector with the name of the R function that computes the measure.
#' @param index.args A list with additional arguments passed to \code{index.fun}. It contains a key-value list for each additional argument.
#' @param measure.args Same as \code{index.args} but for the measure function.
#' @param o R object containing the observations as returned by \code{\link{loadValueStations}}. 
#' @param p R object containing the predictions as returned by \code{\link{loadValuePredictions}}.
#' @importFrom abind abind
#' @export

wrapperFUN <- function(metric = c("obs", "pred", "measure"),
                       names = NULL,
                       season = c("annual", "DJF", "MAM", "JJA", "SON"),
                       aggr.type = c("before", "after"),
                       aggr.fun = "mean",
                       index.fun = NULL,
                       measure.fun = NULL,
                       index.args = NULL,
                       measure.args = NULL,
                       o = o,
                       p = p) {
      metric <- match.arg(arg = metric, choices = c("obs", "pred", "measure"), several.ok = TRUE)
      season <- match.arg(arg = season, choices = c("annual", "DJF", "MAM", "JJA", "SON"), several.ok = TRUE)
      aggr.type <- match.arg(arg = aggr.type, choices = c("before", "after"))
      suffix <- "\\.R$|\\.r$"
      if (grepl(suffix, index.fun)) gsub(suffix, "", index.fun)
      if (grepl(suffix, measure.fun)) gsub(suffix, "", measure.fun)
      int <- getIntersect(o,p)
      o <- dimFix(int$obs)
      p <- dimFix(int$prd)
      int <- NULL
      # Member aggregation (the array is re-assigned the member dimension after the aggregation)
      if (aggr.type == "before" & dim(p$Data)[1] > 1) {
            dimNames <- attr(p$Data, "dimensions")
            p$Data <- apply(p$Data,
                            MARGIN = grep("member", attr(p$Data, "dimensions"), invert = TRUE),
                            FUN = aggr.fun, na.rm = TRUE)
            p$Data <- unname(abind(p$Data, NULL, along = 0))    
            attr(p$Data, "dimensions") <- dimNames
            attr(p$Data, "member.aggr.fun") <- aggr.fun
      }
      n.st <- dim(o$Data)[3]
      n.mem <- dim(p$Data)[1]
      n.metric <- length(metric)
      n.seas <- length(season)
      index.arr <- array(data = NA, dim = c(n.st, n.seas, n.metric),
                         dimnames = list("station_id" = o$Metadata$station_id,
                                         "season" = season,
                                         "metric_name" = names))
      attr(index.arr, "aggr.type") <- aggr.type
      for (i in 1:n.st) {
            st.o <- subsetVALUE(o, stationID = o$Metadata$station_id[i])
            st.p <- subsetVALUE(p, stationID = p$Metadata$station_id[i])
            for (j in 1:n.seas) {
                  seas <- switch(season[j],
                                 "annual" = 1:12,
                                 "DJF" = c(12,1,2),
                                 "MAM" = 3:5,
                                 "JJA" = 6:8,
                                 "SON" = 9:11)
                  sea.o <- subsetVALUE(st.o, season = seas)
                  sea.p <- subsetVALUE(st.p, season = seas)
                  st.o <- st.p <- NULL
                  # Vectorization
                  dates.obs <- sea.o$Dates$start
                  obs <- as.matrix(drop(sea.o$Data))
                  dates.pred <- sea.p$Dates$start
                  prd <- t(as.matrix(drop(sea.p$Data))) 
                  sea.o <- sea.p <- NULL
                  aux.list <- lapply(1:ncol(prd), function(x) preprocessVALUE(obs, prd[,x], dates.obs, dates.pred))
                  for (k in 1:n.metric) {
                        ind <- grep(metric[k], names(aux.list[[1]]))
                        if (length(ind) == 0) {# measure
                              if (k > 1) {
                                    indexObs <- index.arr[i,j,grep("^obs", attr(index.arr,"dimnames")$'metric_name')]      
                                    indexPrd <- index.arr[i,j,grep("^pred", attr(index.arr,"dimnames")$'metric_name')]
                              } else {
                                    indexObs <- indexPrd <- NULL
                              }
                              aux <- rep(NA, n.mem)
                              for (l in 1:n.mem) {  
                                    arg.list <- list("indexObs" = indexObs,
                                                     "indexPrd" = indexPrd,
                                                     "obs" = aux.list[[l]]$obs,
                                                     "prd" = aux.list[[l]]$pred)
                                    if (!is.null(measure.args)) {
                                          for (m in 1:length(measure.args)) {
                                                arg.list[[length(arg.list) + 1]] <- measure.args[[m]]$value
                                                names(arg.list)[length(arg.list)] <- measure.args[[m]]$key
                                          }
                                    }
                                    aux[l] <- do.call(measure.fun, args = arg.list)   
                              }
                              index.arr[i,j,k] <- mean(aux, na.rm = TRUE)
                        } else {# index
                              aux <- rep(NA, n.mem)
                              for (l in 1:n.mem) {      
                                    ind <- grep(metric[k], names(aux.list[[l]]))
                                    arg.list <- list("ts" = aux.list[[l]][[ind]])
                                    if (!is.null(index.args)) {
                                          for (m in 1:length(index.args)) {
                                                arg.list[[length(arg.list) + 1]] <- index.args[[m]]$value
                                                names(arg.list)[length(arg.list)] <- index.args[[m]]$key
                                          }
                                    }
                                    aux[l] <- do.call(index.fun, arg.list)
                              }
                              index.arr[i,j,k] <- mean(aux, na.rm = TRUE)
                        }
                  }
                  aux.list <- NULL
            }
      }      
}



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
#' @author J. Bedia

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

#' @title preprocessVALUE
#' @description Preprocessing of input data for index calculation routines
#' @param obs A time series (vector) of observations
#' @param pred A time series (vector) of predictions
#' @param dates.obs Calendar dates corresponding to the values in \code{obs}
#' @param dates.pred Calendar dates corresponding to the values in \code{pred}
#' @param season Months to retain. Default to NULL (annual).
#' @param na.prop Maximum allowable proportion of missing values (Default to 0.25).
#' @details The function performs missing data filetring, temporal matching and (optionally) seasonal subsetting 
#' @return A list with preprocessed \code{obs}, \code{pred} and \code{dates}, as passed to the index.* routines
#' @keywords internal
#' @author J. Bedia
#' @examples \dontrun{
#' data(precipIberiaECA)
#' obs <- precipIberiaECA$observations$Data[,3]
#' pred <- precipIberiaECA$predictions$Data[,3]
#' dates.obs <- precipIberiaECA$observations$Dates$start
#' dates.pred <- precipIberiaECA$predictions$Dates$start
#' # Retain data for July-August (season = 7:8)
#' a <- preprocess.value(obs, pred, dates.obs, dates.pred, season = 7:8, na.prop = 0.5)
#' str(a)
#' }

preprocessVALUE <- function(obs, pred, dates.obs, dates.pred, season = NULL, na.prop = .25) {
      # Temporal matching
      ind.obs <- which(is.element(dates.obs, dates.pred))
      ind.pred <- which(is.element(dates.pred, dates.obs))
      if (length(ind.obs) == 0) stop("No temporal matching between observations and predictions")
      obs <- obs[ind.obs]
      dates <- dates.obs[ind.obs]
      pred <- pred[ind.pred]
      # Season filtering
      if (!is.null(season)) {
            if (any(!(season %in% 1:12))) stop("Ill-defined season")
            mon.ind <- which((as.POSIXlt(dates)$mon + 1) %in% season)
            obs <- obs[mon.ind]
            pred <- pred[mon.ind]
            dates <- dates[mon.ind]
      }
      # NA filtering
      na.ind <- union(which(is.na(obs)), which(is.na(pred)))
      # Datos utiles
      if  (length(na.ind) > 0) {
            na.prop.data <- round(length(na.ind) / length(obs), 2)
            if (na.prop.data > na.prop) {
                  stop("Maximum allowable missing data proportion (", na.prop*100, "%) exceeded (", na.prop.data*100,"%)")
            }
            obs <- obs[-na.ind]
            pred <- pred[-na.ind]
            dates <- dates[-na.ind]
      }
      return(list("obs" = obs, "pred" = pred, "dates" = dates))
}

