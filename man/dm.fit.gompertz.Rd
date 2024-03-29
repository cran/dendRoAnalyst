% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dm.fit.gompertz.R
\name{dm.fit.gompertz}
\alias{dm.fit.gompertz}
\title{Fitting gompertz function on annual dendrometer data}
\usage{
dm.fit.gompertz(df, CalYear, TreeNum, f_derivative = F)
}
\arguments{
\item{df}{dataframe with first column containing date and time in the format \code{yyyy-mm-dd HH:MM:SS} and the dendrometer data in following columns.}

\item{CalYear}{numeric for year of calculation. If df has more than one year, assigning CalYear truncates the data of only that year.}

\item{TreeNum}{numerical value indicating the tree to be analysed. E.g. '1' refers to the first dendrometer data column in \emph{df}.}

\item{f_derivative}{logical if yes returns first derivative of gompertz curve.}
}
\value{
A dataframe with the modelled dendrometer series.
}
\description{
This function modells the annual growth of dendrometer data using gompertz function.
}
\examples{
\donttest{library(dendRoAnalyst)
data(gf_nepa17)
gomp_fitted<-dm.fit.gompertz(df=gf_nepa17, TreeNum = 1, CalYear=2017)
head(gomp_fitted,10)}

}
