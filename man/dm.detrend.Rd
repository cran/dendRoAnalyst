% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dm.detrend.R
\name{dm.detrend}
\alias{dm.detrend}
\title{Detrend the dendrometer data}
\usage{
dm.detrend(df, method = "gam", CalYear)
}
\arguments{
\item{df}{dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and the dendrometer data in following columns.}

\item{method}{either 'f_diff', 'gam' or 'gomp' indicating the method to detrend the dendrometer data column in \emph{df}.}

\item{CalYear}{numeric for year of calculation. If df has more than one year, assigning CalYear truncates the data of only that year.}
}
\value{
A dataframe with the detrended dendrometer series.
}
\description{
This function detrends the dendrometer data either using first difference or using gam function from \code{\link[mgcv]{mgcv}} package or the Gompertz function.
}
\examples{
\donttest{library(dendRoAnalyst)
data(gf_nepa17)
detrended<-dm.detrend(df=nepa17, method='f_diff', CalYear=2017)
head(detrended,10)}

}
