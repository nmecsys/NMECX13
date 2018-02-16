\name{seasX13}
\alias{seasX13}
\title{Perform seasonal adjustment via X-13ARIMA-SEATS for multiple series}
\usage{
seasX13(x, ...)
}
\arguments{
\item{x}{output from readX13 function}
\item{...}{arguments to be passed to seas function from \pkg{seasonal} package}
}
\value{
A \code{list} containing the following elements:

\item{xSA}{seasonally adjusted time series}
\item{seasonalFactors}{seasonal factors for each series}
\item{calendarFactors}{calendar effects for each series}
\item{totalFactors}{seasonal plus calendar factors for each series}
\item{espec}{model especifications for each series}
\item{model}{output from seas function (package seasonal)  for each series}
\item{read}{output items from readX13 function}

}

\description{
use X-13ARIMA-SEATS program by US Census Bureau to perform seasonal adjustment in time series. The function uses the \pkg{seasonal} package and applies its routine to a csv/xlsx file with multiple series simultaneously.
}

\examples{

}


