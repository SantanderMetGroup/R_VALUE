#' @title Arithmetic mean of days equal or above threshold
#' @description Function to compute the arithmetic mean of days equal or above a given threshold.
#' @author J. Bedia
#' @template templateIndexParams
#' @param threshold Threshold value. Default to 1.
#' @details The function is envisaged for calculating the mean precipitation of wet days, excluding dry ones
#'  (\emph{a.k.a.} simple day intensity index).
#' @return A float number corresponding to the mean of days above the threshold.
#' @export


index.meanGE <- function(ts, threshold = 1) {
      mean(ts[which(ts >= threshold)], na.rm = TRUE)
}
