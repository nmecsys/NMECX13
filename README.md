[![Build Status](https://travis-ci.org/nmecsys/NMECX13.svg?branch=master)](https://travis-ci.org/nmecsys/NMECX13)


# NMECX13
The package `NMECX13` uses X-13ARIMA-SEATS program by US Census Bureau to perform seasonal adjustment in multiple time series simultaneously. The main function `seasX13()` uses the package `seasonal` to perform the seasonal adjustment. The function also performs an automatic correction routine so that the results are properly diagnosed (residuals without autocorrelation and seasonality).

**The package is in development. Reviews, comments and pull requests are welcome.**


## Installation

```R
devtools::install_github('nmecsys/NMECX13')
library(NMECX13)
```

## Usage

There are three steps to perform the seasonal adjustment in several series:

1. read
2. automatic seasonal adjustment, check results and correct them if necessary)
4. export results to a csv file


### 1. Reading a file

Use the function `readX13()` to read a CSV or a XLSX file or a data.frame object with the time series. The file/object must have 2 or more columns. The first one must contain the sequential date of the time series. Missing values are supported. Examples:

```R
readX13(path = "file_name.csv")
readX13(path = "file_name.xlsx", sheetNumber = 1)
readX13(path = dataFrameObject)

```
The `sheetNumber` argument is the sheet number of xlsx file.

#### readX13() outputs

The function return a list containing the following elements:

* `xts`: time series in the path file.

* `xtsNA`: a object identifying the missing observations in each series.

* `deniedNames`: a vector naming the time series that will not be seasonally 
adjusted (less than three years of observation).
    
* `acceptedNames`: a vector naming time series that can be seasonally adjusted.

* `path`: to the csv/xlsx file

### 2. Automatic seasonal adjustment

Use the function `seasX13()` to perform an automatic seasonal adjustment in time series. This function receives the output from function `readX13()`.

```R
seasX13(x, autoCorrection = NULL, userCorrection = NULL)
```

* `x`: output from function `readX13()` function;

* `autoCorrection`: a vector naming the time series should be auto corrected;

* `userCorrection`: a vector naming the time series should be corrected by user especifications.


#### seasX13() outputs

* `xSA`: seasonally adjusted time series

* `seasonalFactors`: seasonal factors for each series

* `calendarFactors`: calendar effects for each series

* `totalFactors`: seasonal plus calendar factors for each series

* `espec`: model especifications for each series

* `model`: output from seas function (package **seasonal**) for each series

* `read`: output items from readX13 function



### 3. Exporting results

The function `saveX13()` exports CSV files for seasonally adjusted data, seasonal factors, calendar factors, total factors (seasonal & calendar factors) and model specification for each series.

```R
saveX13(output, file = "")
```

Function arguments:

* `output`: output from `seasX13()` function.

* `file`: a character string naming the file


&nbsp;

# Example

This example will read a data.frame called `serviceSurvey`. The data contains 3 time series.

### step 1: read the data

```R
# load and read data example
data(serviceSurvey)
data <- readX13(serviceSurvey)
```

### step 2: perform an automatic seasonal adjustment

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
```

- correct automatic seasonal adjustment using `autoCorrection` argument from `seasX13()` function

```R
# correct all series (be patient, 48 models will be executed for each series)
correct1 <- seasX13(auto, autoCorrection = "")

# correct just one series
correct2 <- seasX13(auto, autoCorrection = c("ISAS"))
```

- correct automatic seasonal adjustment using `userCorrection` argument from `seasX13()` function.

```R
# edit the especification of output object from function seasX13 
auto$espec["IES","arima.model"] <- "(0 1 1)(0 1 1)"
auto$espec["IES","calendar.effects"] <- "td, carnival"

# run seasonal adjustment with userCorrection option
correct3 <- seasX13(auto, userCorrection = c("IES"))
correct3$espec
```

### step 3: export results

```R
saveX13(auto, "auto")
```

The results will be export to the working directory.

