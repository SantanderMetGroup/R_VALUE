#' @title Cramer von Misses
#' @description Function to compare the observed and predicted distribution using the Cramer von Misses index.
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @template templateMeasureParams
#' @param Nbins Number of bins used for discretization. Default to 100.
#' @source L. Sachs and J. Hedderich (2006). Angewandte Statistik. Springer.  
#' @return List with the Cramer von Misses index (\code{CvM}), the p-value (\code{pvalue}) of the test 
#' and the maximum absolute difference between both the observed and predicted distributions (\code{KSdiff}).
#' @export


measure.cm <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, Nbins = 100) {
      if (length(obs) <= 1) {
            stop("Observed time series is needed")
      }
      if (length(prd) <= 1) {
            stop("Predicted time series is needed")
      }
      meanObj <- list("CvM" = NA, "pvalue" = NA, "KSdiff" = NA)
      if (any(is.finite(obs)) && any(is.finite(prd))) {
            seq.all <- range(c(obs, prd), na.rm = TRUE)
            breaks <- (seq.all[2] - seq.all[1]) / (Nbins + 1)
            breaks.s <- seq(from = seq.all[1] - breaks, to = seq.all[2] + breaks, by = breaks)
            breaks.r <- seq(from = seq.all[1] - breaks, to = seq.all[2] + breaks, by = breaks)
            term1 <- (as.numeric(length(obs)) * as.numeric(length(prd))) / (length(obs) + length(prd)) ** 2
            hm.s <- hist(obs, breaks = breaks.s, plot = FALSE)
            hm.r <- hist(prd, breaks = breaks.r, plot = FALSE)
            fs <- cumsum(hm.s$counts) / length(obs)
            gr <- cumsum(hm.r$counts) / length(prd)
            KS <- max(abs(fs-gr))
            term2 <- sum((hm.s$counts + hm.r$counts) * ((fs - gr) ** 2))
            C <- term1 * term2
            # table for C 
            if (C <= 0.184) {
                  pval <- 0.3
            } else if (C < 0.241) {
                  pval <- 0.2
            } else if (C < 0.347) {
                  pval <- 0.1
            }else if (C < 0.461) {
                  pval <- 0.05
            } else if (C < 0.743) {
                  pval <- 0.01
            } else if (C < 1.168) {
                  pval <- 0.001
            } else {
                  pval <- 0.00
            }
            meanObj <- list("CvM" = C, "pvalue" = pval, "KSdiff" = KS)
      }
      return(meanObj)
}
