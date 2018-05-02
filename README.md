[![Build Status](https://travis-ci.org/nmecsys/NMECX13.svg?branch=master)](https://travis-ci.org/nmecsys/NMECX13)

# NMECX13
Use X-13ARIMA-SEATS program by US Census Bureau to perform seasonal adjustment in multiple time series simultaneously.
The main function `seasX13()` uses the `seasonal` package to perform the seasonal adjustment. The function also performs an automatic correction routine so that the results are properly diagnosed (residuals without autocorrelation and seasonality).

**The package is in development. Reviews, comments and pull requests are welcome.**

## Installation

github
```R
devtools::install_github('nmecsys/NMECX13')
```

## How to use NMECX13 package


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
