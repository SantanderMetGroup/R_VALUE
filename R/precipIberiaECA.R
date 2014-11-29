#' @title Observations and predictions for precipitation in Iberia
#' 
#' @description The object contains two objects, \code{observations} and \code{predictions}
#' of daily summer precipitation (JJA) for the period 2001-2010,
#' in a set of 9 ECA stations from the VALUE reference dataset in the Iberian Peninsula. 
#'
#' @format A list containing two objects of the types (not formally classes) \code{stations.observations}
#'  and \code{stations.predictions}
#' @details The predictions have been calculated using NCEP fields of mean daily temperature at 850mb,
#' sea-level pressure and specific humidity at 850 mb as predictors, and a GLM downscaling approach for
#'  the training period 1981-2000. Observations data were obtained from the \pkg{R_VALUE} built-in
#'  dataset \code{VALUE_ECA.zip}. NCEP data and GLM downscaling method implementation is available
#'  in the \pkg{downscaleR} package.
#' @references \url{https://github.com/SantanderMetGroup/downscaleR/wiki}
#' @name precipIberiaECA
#' @examples
#' data(precipIberiaECA)
#' str(precipIberiaECA)
NULL
