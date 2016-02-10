#' @title Correlation matrix for paper Figures 4 and 5 on spatial validation
#' @description Computes the cross correlation matrices between stations that serve as input for plotting functions
#' @param url URL of the target dataset
#' @param var Target variable, as coded within the dataset
#' @param method Correlation method, passed to the \code{\link{cor}} function
#' @param use Optional character string giving a method for computing covariances in the presence of missing values.
#'  Passed to the \code{\link{cor}} function. Default to \code{"pairwise.complete.obs"}
#' @return A 2D matrix. Attributes indicate the station names (in the row/column order they appear), and their
#' longitude/latitude   
#' @type type Character flag indicating whether members should be aggregated before or after computing the correlation.
#' Ignored in the case of observations and deterministic predictions. 
#' @details The function downloads the whole dataset and stores it in a temporary file using \code{\link{tmpfile}}. After
#' reading the data, the file is deleted.
#' @author J. Bedia, M.J. Casado
#' @importFrom tools file_ext
#' @export

corrMat.VALUE <- function(url,
                          var,
                          season = c("annual", "DJF", "MAM", "JJA", "SON"),
                          method = c("pearson", "kendall", "spearman"),
                          use = "pairwise.complete.obs",
                          type = c("before", "after")) {
      url <- "http://meteo.unican.es/work/juaco/VALUE_ECA_86_v1.zip"
      pat <- gsub("\\.zip$|\\.txt$", "_", basename(url))
      ext <- paste0(".", file_ext(basename(url)))
      tmpfile <- tempfile(pattern = pat, fileext = ext)
      download.file(url, destfile = tmpfile)
      a <- loadValueStations(dataset = tmpfile, var = "tmin")
      unlink(tmpfile, recursive = TRUE, force = TRUE)
      a <- dimFix(a)
      
      
      
      
}




