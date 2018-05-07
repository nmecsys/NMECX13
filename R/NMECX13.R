#' @title NMECX13: a package to brazilian time series seasonal adjustment. 
#' 
#' @description The NMECX13 package uses X-13ARIMA-SEATS program by US Census Bureau to perform seasonal adjustment in multiple time series simultaneously.
#'
#' @note We use the `seasonal` R package developed by Christopher Sax to perform the seasonal adjustmente routine.
#' 
#' @author Daiane Marcolino de Mattos \email{daiane.mattos@fgv.br},
#'         Bianca Gonçalves \email{biagbp@gmail.com}
#'
#' @docType package
#' @name NMECX13
#'
if(getRversion() >= "2.15.1")  utils::globalVariables(c("calendarEffects", "listModels"))