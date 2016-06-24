#' @title Circular bias
#' @description Function to compute the bias considering circular series
#' @template templateMeasureParams
#' @return A float number corresponding to the circular bias.
#' @author D. San-Martin
#' @export

measure.biasCirc <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL) {
      distance <- (abs(indexObs - indexPrd) * -1) %% 365
      if (indexObs < indexPrd) {
            distance <- distance * -1
      }
      return(distance)
}
