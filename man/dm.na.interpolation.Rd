% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dm.na.interpolation.R
\name{dm.na.interpolation}
\alias{dm.na.interpolation}
\title{Detection and interpolation of missing values in dendrometer data.}
\usage{
dm.na.interpolation(df, resolution, fill = FALSE, method = "spline")
}
\arguments{
\item{df}{dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and following columns with dendrometer data for the same temporal resolution and time period.}

\item{resolution}{integer, indicating the resolution of dendrometer data in \strong{minutes}.}

\item{fill}{logical, if \code{TRUE} it fills the \code{NA} values using spline interpolation. Default is \code{FALSE}.}

\item{method}{string, 'spline' for the spline interpolation or 'seasonal' for the seasonal interpolation.}
}
\value{
A dataframe containing the dendrometer data including gaps filled with either \code{NA} or interpolated values.
}
\description{
This function detects gap(s) in time series, inserts the missing rows based on the provided temporal resolution and assings \code{NA} values to the corresponding value. If required the \code{NA} values can be replaced by spline interpolation using \code{\link[zoo:na.approx]{na.spline}} of the package \pkg{zoo} or seasonal interpolation considering the seasonality of the daily pattern using \code{\link[forecast:na.interp]{na.interp}} of the package \pkg{forecast}.
}
\examples{
\donttest{library(dendRoAnalyst)
data(nepa17)
gf_nepa17<-dm.na.interpolation(df=nepa17, resolution=60)
head(gf_nepa17,10)}

}
