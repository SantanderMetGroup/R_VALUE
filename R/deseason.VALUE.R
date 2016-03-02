#     deseason.VALUE.R Remove seasonal cycle from a VALUE dataset
#     
#     Copyright (C) 2016 Santander Meteorology Group (http://www.meteo.unican.es)
#
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#
#' @title Remove seasonal cycle
#' @description Remove seasonal cycle from a VALUE dataset using a moving average filter
#' @param valueObj Any VALUE object (as returned either by \code{\link{loadValueStations}} or \code{\link{loadValuePredictions}}).
#' @param max.na.prop Maximum allowed proportion of missing data (Default to 0.25). See details
#' @param window.width Used if \code{deseason = TRUE}. Integer number indicating the width, in days, of the window used for
#'  moving average computation of the reference daily climatology. Default to 31 days. See details.
#' @details A (circular) moving average daily climatology can be automatically calculated for each data series, considering 
#' a user-defined window width centered around lag 0. An additional argument allows to specify the maximum number of missing values allowed,
#'  being ommited from the analysis those data series above the threshold.
#' @return A VALUE object same as input but with seasonal cycles removed. 
#' @details A global attribute "deseason::window.width" is appended to the output, indicating the window width (in days).
#' @note Although the function is insentitive to the input season, it is recommended to perform seasonal cycle removal on
#' the annual series, rather than in particular seasons. Seasonal slices can be selected afterwards using \code{\link{subsetVALUE}}.
#' @author J. Bedia 
#' @export
#' @examples \dontrun{
#' 
#' }

deseason.VALUE <- function(valueObj, window.width = 31, max.na.prop = .25) {
      valueObj <- dimFix(valueObj)
      valueObj <- na.filter.VALUE(valueObj, max.na.prop)
      n.stations <- dim(valueObj$Data)[3]
      n.mem <- dim(valueObj$Data)[1]
      dates <- as.POSIXlt(valueObj$Dates$start)
      doy <- format(dates, format = "%j")
      fil <- rep(1/window.width, window.width + 1)
      message("[", Sys.time(),"] - Removing seasonal cycle on ", n.stations, " locations...")
      for (i in 1:n.mem) {
            mat <- valueObj$Data[i,,]
            for (j in 1:n.stations) {
                  # Moving average, circular, centered on lag 0
                  x <- filter(mat[,j], filter = fil, circular = TRUE, sides = 2)
                  # Daily climatology
                  ref <- tapply(x, INDEX = doy, FUN = mean, na.rm = TRUE)
                  # Remove cycle
                  for (k in 1:length(ref)) {
                        ind <- which(doy == unique(doy)[k])
                        x[ind] <- x[ind] - ref[k]
                  }
                  valueObj$Data[i,,j] <- x
            }
      }
      message("[", Sys.time(), "] - Done.")
      attr(valueObj, "deseason::window.width") <- window.width
      return(valueObj)
}
# End
