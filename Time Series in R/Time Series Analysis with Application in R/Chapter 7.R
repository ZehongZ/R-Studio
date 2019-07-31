library(TSA)

#Exhibit 7.4
#Parameter Estimation for Simulated AR(1) Models
data("ar1.s")
data("ar1.2.s")
ar(ar1.s, order.max = 1, AIC=F, method='yw')
ar(ar1.s, order.max=1, AIC=F, method='ols')
ar(ar1.s, order.max=1, AIC=F, method='mle')
ar(ar1.2.s, order.max=1, AIC=F, method = 'yw')
ar(ar1.2.s, order.max = 1, AIC=F, method='ols')
ar(ar1.2.s, order.max=1, AIC=F, method='mle')

#Exhibit 7.5
#Parameter Estimation for a Simulated AR(2) Model
data("ar2.s")
ar(ar2.s, order.max = 2, AIC=F, method='yw')
ar(ar2.s, order.max=2, AIC=F, method='ols')
ar(ar2.s, order.max=2, AIC=F, method='mle')

#Exhibit 7.6
#Parameter Estimatio for a Simulated ARMA(1,1) Model
data("arma11.s")
arima(arma11.s, order=c(1,0,1),method='CSS')
arima(arma11.s, order=c(1,0,1), method='ML')

#Exhibit 7.7
#Parameter Estimation for the Color Property Series
data(color)
ar(color,order.max=1, AIC=F, method='yw')
ar(color, order.max=1, AIC=F, method='ols')
ar(color, order.max=1, AIC=F, method='mle')

#Exhibit 7.8
#Maximum Likelihood Estiamtes from R Software
data(hare)
arima(sqrt(hare), order=c(3,0,0))

#Exhibit 7.9
#Estimation for the Difference of Logs of the Oil Price Series
data("oil.price")
arima(log(oil.price), order=c(0,1,1), method='CSS')
arima(log(oil.price), order=c(0,1,1), method='ML')

#Exhibit 7.11
#Histogram of Bootstrap Quasi-period Estimates
hist(period.repalce, prop=T, xlab='Quasi-period', axes=F, xlim=c(5,16))
