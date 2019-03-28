#Creating a time-series object
sales<-c(18,33,41,7,34,35,24,25,24,21,25,20,22,31,40,29,25,21,22,54,31,25,26,35)
tsales<-ts(sales, start=c(2003,1),frequency = 12)
tsales
plot(tsales)
start(tsales)
end(tsales)
frequency(tsales)
tsales.subset<-window(tsales, start=c(2003,5),end=c(2004,6))
tsales.subset

#Smoothing a time series to clarify its general trend
#Decomposing a time series in order to observe any seasional effects

#Simple moving average
library(forecast)
opar<-(par(no.readonly = TRUE))
par(mfrow=c(2,2))
ylim<-c(min(Nile), max(Nile))
plot(Nile, main= "Raw time series")
plot(ma(Nile,3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(Nile,7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(Nile, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)
#As k increases, the plot becomes increasingly smoothed.
#The challenge is to find the value of k that highlights the major patterns in the data, without under or over smoothing

#Seasonal decomposition
#Time-series data that have a seasonal aspect (such as monthly or quarterly data) can be decomposed into a trend component, a seasonal component, and an irregular component. 
#The trend component captures changes in level over time.
#The seasonal com- ponent captures cyclical effects due to the time of year. 
#The irregular (or error) component captures those influences not described by the trend and seasonal effects.

#Seasonal decomposition using stl()
plot(AirPassengers)
lAirPassengers<-log(AirPassengers)
plot(lAirPassengers, ylab="log(AirPassengers")
fit<-stl(lAirPassengers,s.window = "period")#Decompose the time series
plot(fit)
fit$time.series
exp(fit$time.series)

par(mfrow=c(2,1))
library(forecast)
monthplot(AirPassengers,xlab="",ylab="")
seasonplot(AirPassengers,year.labels = "TURE", main="")

#Simple expoential smoothing
library(forecast)
fit<-ets(nhtemp, model="ANN")
fit
forecast(fit,1)
accuracy(fit)

#Exponential smoothing with level, slope and seasonal componnents
library(forecast)
fit<-ets(log(AirPassengers), model="AAA")
fit
pred<-forecast(fit,5)
pred
pred$mean<-exp(pred$mean)
pred$lower<-exp(pred$lower)
pred$upper<-exp(pred$upper)
p<-cbind(pred$mean, pred$lower, pred$upper)
dimnames(p)[[2]]<-c("mean", "Lo 80","Lo 95","Hi 80","Hi 95")
p

#Automatic exponential forecasting with ets()
library(forecast)
fit<-ets(JohnsonJohnson)
fit

#ARIMA forecasting models

#Transforming the time serires and assessing stationarity
library(forecast)
library(tseries)
plot(Nile)
ndiffs(Nile)
dNile<-diff(Nile)
plot(dNile)
adf.test(dNile)
Acf(dNile)
pacf(dNile)

#Fitting an ARIMA model
library(forecast)
fit<-arima(Nile, order=c(0,1,1))
fit
accuracy(fit)

#Evaluating the model fit
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type="Ljung-Box")

#Forecasting with an ARIMA model
forecast(fit,3)
plot(forecast(fit,3), xlab="Year", ylab = "Annual Flow")

#Automated ARIMA forecasting
library(forecast)
fit<-auto.arima(sunspots)
fit
forecast(fit,3)
