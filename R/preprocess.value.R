#' @title preprocess.value
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

preprocess.value <- function(obs, pred, dates.obs, dates.pred, season = NULL, na.prop = .25) {
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
