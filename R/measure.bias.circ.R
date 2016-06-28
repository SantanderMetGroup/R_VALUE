#' @title Circular bias
#' @description Function to compute the bias considering circular series
#' @template templateMeasureParams
#' @return A float number corresponding to the circular bias.
#' @author D. San-Martin
#' @export

measure.bias.circ <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL) {
      if(is.null(indexObs) | is.null(indexPrd)){
            return(NA)
      }
      if(is.na(indexObs) | is.na(indexPrd)){
            return(NA)
      }
      distance <- (abs(indexObs - indexPrd) * -1) %% 365
      if (indexObs < indexPrd) {
            distance <- distance * -1
      }
      return(distance)
}
