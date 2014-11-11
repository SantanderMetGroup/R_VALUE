
loadValueStations(dataset = "inst/GSN_Iberia.zip", var = "tmin", lonLim = -3, latLim = 42, season = 1, years = 1985:1992)


dataset = "inst//GSN_Iberia.zip"
var = "tmin"
stationID = NULL
lonLim = -3
latLim = 42
season = c(12,1,2)
years = 1991:2000



read.zip <- function(dataset) {
      zipFileInfo <- unzip(dataset, list = TRUE)
      zipFileInfo
      if(nrow(zipFileInfo) > 1)
            stop("More than one data file inside zip")
      else
            read.csv(unz(file, as.character(zipFileInfo$Name)), ...)
}


?unz
unz(filename = dataset)

zipFileInfo <- unzip(dataset, list = TRUE)

# http://stackoverflow.com/questions/12460938/r-reading-in-a-zip-data-file-without-unzipping-it
data <- read.table(unz(dataset, "stations.txt"), header=T, quote="\"", sep=",")

tolower("Stations")

library(swirl)
rm(list = ls())
swirl()
juaco

