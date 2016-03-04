#' @title Cramer von Misses
#' @description Function to compare the observed and predicted distribution using the Cramer von Misses index.
#' @author Ole Roessler \email{ole.roessler@@giub.unibe.ch}, J. Bedia, D. San-Mart\'in, S. Herrera
#' @template templateMeasureParams
#' @param Nbins Number of bins used for discretization. Default to 100.
#' @param what Measure to be returned. This can be the Cramer von Misses index (\code{"CvM"}), 
#' the p-value (\code{"pvalue"}) of the test or the maximum absolute difference between the observed and predicted
#'  distributions (\code{"KSdiff"}). Default to \code{"CvM"}.
#' @source L. Sachs and J. Hedderich (2006). Angewandte Statistik. Springer.  
#' @return A floating number with the measure indicated in \code{what} argument.
#' @export


measure.cm <- function(indexObs = NULL, indexPrd = NULL, obs = NULL, prd = NULL, Nbins = 100, what = c("CvM", "pval", "KSdiff")) {
      if (length(obs) <= 1) {
            stop("Observed time series is needed")
      }
      if (length(prd) <= 1) {
            stop("Predicted time series is needed")
      }
      what <- match.arg(what, choices = c("CvM", "pval", "KSdiff"))
      out <- NA
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
            # KS <- max(abs(fs - gr))
            out <- max(abs(fs - gr)) # KSdiff
            if (what == "CvM" | what == "pval") {
                  term2 <- sum((hm.s$counts + hm.r$counts) * ((fs - gr) ** 2))
                  out <- term1 * term2 # CVM index
                  if (what == "pval") {
                        # table for C 
                        if (out <= 0.184) {
                              out <- 0.3
                        } else if (out < 0.241) {
                              out <- 0.2
                        } else if (out < 0.347) {
                              out <- 0.1
                        } else if (out < 0.461) {
                              out <- 0.05
                        } else if (out < 0.743) {
                              out <- 0.01
                        } else if (out < 1.168) {
                              out <- 0.001
                        } else {
                              out <- 0.00
                        }
                  }
            }
      }
      return(out)
}
