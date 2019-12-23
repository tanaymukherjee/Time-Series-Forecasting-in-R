# Load the data
# Fiel contains data on the age of death of successive kings of England,
# starting with William the Conqueror 
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings

# next step is to store the data in a time series object in R
kingstimeseries <- ts(kings)
kingstimeseries

# data set of the number of births per month in New York city,
# from January 1946 to December 1959 (originally collected by Newton)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries

# contains monthly sales for a souvenir shop at a beach resort town in Queensland,
# Australia, for January 1987-December 1993
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries

# Plotting time series
plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)

# Transforming the series
logsouvenirtimeseries <- log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

# Decomposing Time Series
library("TTR")

kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)

# Smoothing the data
kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

# Decomposing Seasonal Data
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriescomponents$seasonal

# Plot the estimated trend, seasonal, and irregular components of the time series
plot(birthstimeseriescomponents)

# Seasonally Adjusting
birthstimeseriescomponents <- decompose(birthstimeseries)
birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

# Forecasts using Exponential Smoothing
# Simple Exponential Smoothing
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)

rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts

rainseriesforecasts$fitted
plot(rainseriesforecasts)

rainseriesforecasts$SSE
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56)

library("forecast")
rainseriesforecasts2 <- forecast(rainseriesforecasts, h=8)
rainseriesforecasts2
plot(forecast(rainseriesforecasts2))

# A correlogram of the in-sample forecast errors
acf(rainseriesforecasts2$residuals, lag.max=20, na.action = na.omit) 

# Test whether there is significant evidence for non-zero correlations
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
# Here the Ljung-Box test statistic is 17.4, and the p-value is 0.6,
# so there is little evidence of non-zero autocorrelations
# in the in-sample forecast errors at lags 1-20.

plot.ts(rainseriesforecasts2$residuals)

# Check whether the forecast errors are normally distributed with mean zero,
# we can plot a histogram of the forecast errors, with an overlaid
# normal curve that has mean zero and the same standard deviation as the
# distribution of forecast errors.
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

# Plot a histogram (with overlaid normal curve) of the
# forecast errors for the rainfall predictions
plotForecastErrors(rainseriesforecasts2$residuals)

# Holt-Winters Exponential Smoothing
# ARIMA Models
volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodust
plot(volcanodust)

volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)
acf(volcanodustseries, lag.max=20) 

acf(volcanodustseries, lag.max=20, plot=FALSE) # get the values of the autocorrelations
pacf(volcanodustseries, lag.max=20)

pacf(volcanodustseries, lag.max=20, plot=FALSE)

volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesarima
volcanodustseriesforecasts <- forecast(volcanodustseriesarima, h=31) #edited from forecast.Arima
volcanodustseriesforecasts

plot(volcanodustseriesforecasts) #edited from plot.forecast
acf(volcanodustseriesforecasts$residuals, lag.max=20)

Box.test(volcanodustseriesforecasts$residuals, lag=20, type="Ljung-Box")
plot.ts(volcanodustseriesforecasts$residuals)            # make time plot of forecast errors

plotForecastErrors(volcanodustseriesforecasts$residuals) # make a histogram
mean(volcanodustseriesforecasts$residuals)

