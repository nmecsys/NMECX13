\name{seasX13}
\alias{seasX13}
\title{Perform seasonal adjustment via X-13ARIMA-SEATS for multiple series}
\usage{
seasX13(x, ...)
}
\arguments{
\item{x}{output from readX13 function}
\item{...}{arguments to be passed to seas function from seasonal package}
}
\value{
A \code{list} containing the following elements:

\item{xSA}{seasonally adjusted time series}
\item{seasonalFactors}{seasonal factors for each series}
\item{calendarFactors}{calendar effects for each series}
\item{totalFactors}{seasonal plus calendar factors for each series}
\item{espec}{model especifications for each series}
\item{model}{output from seas function (package seasonal)  for each series}

}

\description{
seas a CSV or XLSX file. The file must have 2 or more columns. The first one must contain the sequential date of the time series. Missing values are supported. 
}

\examples{

}
