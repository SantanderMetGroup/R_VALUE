measure.bias.circ2 <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL) {
      if(is.null(indexObs) | is.null(indexPrd)){
            return(NA)
      }
      if(is.na(indexObs) | is.na(indexPrd)){
            return(NA)
      }
      distance <- abs(indexPrd-indexObs) %% 365
      if(distance > 365/2){
            distance = 365 - distance
      }
      if(indexObs < indexPrd){
            distance <- distance * -1
      }
      return(distance)
}
