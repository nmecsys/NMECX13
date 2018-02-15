\name{readX13}
\alias{readX13}
\title{Read data file for seasonal adjustment}
\usage{
function(path = "", sheetNumber = 1)
}
\arguments{
\item{path}{Path to the csv/xlsx file}
\item{sheetNumber}{sheet number of xlsx file }
}
\value{
A \code{list} containing the following elements:

\item{xts}{a \code{ts} or \code{mts} object with the time series in a path file .}
\item{xtsNA}{a \code{ts} or \code{mts} object identifying the missing observations in each series.}
\item{deniedNames}{a vector with time series names that will not be seasonal adjusted because their length are less than three years.}
\item{acceptedNames}{a vector with time series names that can be seasonal adjusted.}
\item{path}{Path to the csv/xlsx file}

}
\description{
Read a .csv or .xlsx file with time series content. The file must have 2 or more columns. The first one must contain the initial date of the time series. Missings are supported. 
}
\examples{

}
