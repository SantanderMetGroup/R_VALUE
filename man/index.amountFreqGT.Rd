% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/index.amountFreqGT.R
\name{index.amountFreqGT}
\alias{index.amountFreqGT}
\title{Amount from events above threshold}
\usage{
index.amountFreqGT(ts, threshold = 1)
}
\arguments{
\item{ts}{A vector containing the input time series}

\item{threshold}{A float number defining the threshold considered. Default to 1.}
}
\value{
A float number corresponding to the amount fallen the days above the input threshold.
}
\description{
Function to compute the amount from events above a predefined threshold.
}
\author{
Neyko Neykov, J. Bedia, D. San-Martin, S. Herrera
}
