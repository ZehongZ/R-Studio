library(TSA)
#Exhibit 8.1
#Standardized Residuals from AR(1) Model of Color
data(color)
m1.color=arima(color, order=c(1,0,0))
m1.color
plot(rstandard(m1.color), ylab='Standardized Residuals', type='o')
abline(h=0)

#Exhibit 8.2
#Standardized Residuals from AR(3) Model for Sqrt(Hare)
data(hare)
m1.hare=arima(sqrt(hare),order=c(3,0,0))
m1.hare
m2.hare=arima(sqrt(hare), order=c(3,0,0), fixed=c(NA,0,NA,NA))
m2.hare
#The Intercept term given in R is actually the mean in the centered form of the ARMA model
plot(rstandard(m2.hare), ylab='Standardized Residuals', type='o')
abline(h=0)

#Exhibit 8.3
#Standardized Residuals from Log Oil Price IMA(1,1) Model
data("oil.price")
m1.oil=arima(log(oil.price),order=c(0,1,1))
plot(rstandard(m1.oil), ylab='Standardized Residuals', type='l')
abline(h=0)

#Exhibit 8.4
#Quantile-Quantile Plot: Residuals from AR(1) Color Model
qqnorm(residuals(m1.color))
qqline(residuals(m1.color))

#Exhibit 8.5
#Quantile-Quantile Plot: Residuals from AR(3) for Hare
qqnorm(residuals(m1.hare))
qqline(residuals(m1.hare))

#Exhibit 8.6
#Quantile-Quantile Plot: Residuals from IMA(1,1) Model
qqnorm(residuals(m1.oil))
qqline(residuals(m1.oil))

#Exhibit 8.9
#Sample ACF of Residuals from AR(1) Model for Color
acf(residuals(m1.color))

#Exhibit 8.10
#Sample ACF of Residual from AR(2) Model for Hare
acf(residuals(arima(sqrt(hare), order=c(2,0,0))))

#Exhibit 8.11
#Residual Autocorrelation Values from AR(1) Model for Color
acf(residuals(m1.color), plot=F)$acf
signif(acf(residuals(m1.color),plot=F)$acf[1:6],2)#Display the first 6 acf values to 2 significant digits

#Exhibit 8.12
#Diagnostic Display for the AR(1) Model of Color Property
tsdiag(m1.color, gof=15, omit.initial=F)

