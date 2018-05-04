[![Build Status](https://travis-ci.org/nmecsys/NMECX13.svg?branch=master)](https://travis-ci.org/nmecsys/NMECX13)


# NMECX13
Use X-13ARIMA-SEATS program by US Census Bureau to perform seasonal adjustment in multiple time series simultaneously.
The main function `seasX13()` uses the `seasonal` package to perform the seasonal adjustment. The function also performs an automatic correction routine so that the results are properly diagnosed (residuals without autocorrelation and seasonality).

**The package is in development. Reviews, comments and pull requests are welcome.**


## Installation and loading

```R
devtools::install_github('nmecsys/NMECX13')
library(NMECX13)
```


## Reading of the file

Read a CSV or a XLSX file or a data.frame object. The file/object must have 2 or more columns. The first one must contain the sequential date of the time series. Missing values are supported.

```R
readX13(path = "file_name.csv")
readX13(path = "file_name.xlsx", sheetNumber = 1)
```
The `sheetNumber` argument is the sheet number of xlsx file.

#### Outputs of function

`data` return a list containing the following elements:

* `xts`: time series in the path file.

* `xtsNA`: a object identifying the missing observations in each series.

* `deniedNames`: a vector naming the time series that will not be seasonally
    adjusted (less than three years of observation).
    
* `acceptedNames`: a vector naming time series that can be seasonally adjusted.

* `path`: to the csv/xlsx file

## Seasonal adjustment for data file

For more information about the `seasX13` function, check the description of function in:

```R
help(seasX13)
```
This function receive the output from `readX13` function.

```R
seasX13(x, autoCorrection = NULL, userCorrection = NULL)
```
Others function arguments:

* `autoCorrection`: a vector naming the time series should be auto corrected.

* `userCorrection`: a vector naming the time series should be corrected by user especifications


#### Outputs of function

* `xSA`: seasonally adjusted time series

* `seasonalFactors`: seasonal factors for each series

* `calendarFactors`: calendar effects for each series

* `totalFactors`: seasonal plus calendar factors for each series

* `espec`: model especifications for each series

* `model`: output from seas function (package **seasonal**) for each series

* `read`: output items from readX13 function



## Saving the results

Export CSV files for seasonally adjusted data, seasonal factors, calendar factors, total factors (seasonal & calendar factors) and model specification for each series.

```R
saveX13(output, file = "")
```

Function arguments:

* `output`: output from `seasX13` function.

* `file`: a character string naming the file


&nbsp;

# How to use NMECX13 package

This example will be used a data.frame of the R, called `serviceSurvey`.

### read a data file (csv or xlsx) or a data.frame

```R
# load and read data example
data(serviceSurvey)
data <- readX13(serviceSurvey)
```

### seasonal adjustment

```R
# auto seasonal adjustment
auto <- seasX13(data)

# some results: model especifications
auto$espec

# some results: SARIMA model (first series)
summary(auto$model$ICS)

# some results: plot (second series)
ts.plot(data$xts[,"IES"],auto$xSA[,"IES"], col = 1:2, lwd = 1:2)
legend("topright", legend = c("original", "seas. adjusted"), col = 1:2, lwd = 1:2, bty = "n")

### correct automatic seasonal adjustment: autoCorrection
# all series (be patient, 48 models will be executed for each series)
correct1 <- seasX13(auto, autoCorrection = "")

# correct just one series
correct2 <- seasX13(auto, autoCorrection = c("ISAS"))

### correct automatic seasonal adjustment: userCorrection
# edit the especification of output object from function seasX13 
auto$espec["IES","arima.model"] <- "(0 1 1)(0 1 1)"
auto$espec["IES","calendar.effects"] <- "td, carnival"

# run seasonal adjustment with userCorrection option
correct3 <- seasX13(auto, userCorrection = c("IES"))
correct3$espec
```

### export results

```R
saveX13(auto, "auto")
```

The results will be export to the same folder that contain the files.

Remember that you can export the others results as for example the corrections.