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

\item{xSA}{seasonally adjusted time series.}
\item{seasonalFactors}{}
\item{calendarFactors}{}
\item{totalFactors}{}
\item{espec}{}


 output <- list()
  output$x_as <- x_as
  output$seasonalFactors <- fator_s10
  output$calendarFactors <- fator_calendario
  output$totalFactors <- fator_total
  output$espec <- esp
  output$model <- outX13
  output
  
}
\description{
seas a CSV or XLSX file. The file must have 2 or more columns. The first one must contain the sequential date of the time series. Missing values are supported. 
}
\examples{

}
