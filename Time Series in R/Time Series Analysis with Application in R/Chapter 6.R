library(TSA)
#Exhibit 6.5
#Sample Autocorrelation of an MA(1) process
data("ma1.1.s")
acf(ma1.1.s, xaxp=c(0,20,10))

#Exhibit 6.6
#Alternative Bounds for the Sample ACF for the MA(1) Process
acf(ma1.1.s, ci.type='ma', xaxp=c(0,20,10))

#Exhibit 6.7
#Sample Autocorrelation for an MA(1) Process
data("ma1.2.s")
acf(ma1.2.s, xaxp=c(0,20,10))

#Exhibit 6.8
#Sample ACF for an MA(2) Process
data("ma2.s")
acf(ma2.s,xaxp=c(0,20,10))

#Exhibit 6.9
#Alternative Bounds for the Sample ACF for the MA(2) Process
acf(ma2.s, ci.type='ma', xaxp=c(0,20,10))

#Exhibit 6.10
#Sample ACF for an AR(1) Process
data("ar1.s")
acf(ar1.s,xaxp=c(0,20,10))

#Exhibit 6.11
#Sample Partial ACF for an AR(1) Process 
pacf(ar1.s, xaxp=c(0,20,10))

#Exhibit 6.12
#Sample ACF for an AR(2) Process
acf(ar2.s, xaxp=c(0,20,10))

#Exhibit 6.13
#Sample PACF for an AR(2) Process
pacf(ar2.s, xaxp=c(0,20,10))

#Exhibit 6.14
#Simulated ARMA(1,1) Series
data("arma11.s")
plot(arma11.s, type='o', ylab=expression(Y[t]))

#Exhibit 6.15
#Sample ACF for Simulated ARMA(1,1) Series
acf(arma11.s,xaxp=c(0,20,10))

#Exhibit 6.16
#Sample PACF for Simulate ARMA(1,1) Series
pacf(arma11.s, xaxp=c(0,20,10))

#Exhibit 6.17
#Sample EACF for Simulated ARMA(1,1) Series
eacf(arma11.s)

#Exhibit 6.18
#Sample ACF for the Oil Price Time Series
data("oil.price")
acf(as.vector(oil.price),xaxp=c(0,24,12))

#Exhibit 6.19
#Sample ACF for the Difference of the Log Oil Price Series
acf(diff(as.vector(log(oil.price))),xaxp=c(0,24,12))#Looks like a MA(1)
pacf(diff(as.vector(log(oil.price))),xaxp=c(0,24,12))

#Exhibit 6.20
#Sample ACF of Overdiffernced Random Walk
data("rwalk")
acf(diff(rwalk, differences = 2), ci.type='ma', xaxp=c(0,18,9))

#Exhibit 6.21
#Sample ACF of Correctly Differenced Random Walk
acf(diff(rwalk), ci.type='ma', xaxp=c(0,18,9))

#Exhibit 6.22
#Best Subset ARMA Selection Based on BIC
set.seed(92397)
test=arima.sim(model=list(ar=c(rep(0,11),.8),
                          ma=c(rep(0,11),.7)),n=120)
res=armasubsets(y=test, nar=14, nma=14, y.name='test', ar.method = 'ols')
plot(res)

#Exhibit 6.23 
#QQ Normal Plot of the Logarithms of LA Annual Rainfall
data("larain")
qqnorm(log(larain))
qqline(log(larain))

#Exhibit 6.24
#Sample ACF of the Logarithms of LA Annual Rainfall
acf(log(larain), xaxp=c(0,20,10))

#Exhibit 6.25
#Sample ACF for the Color Property Series
data(color)
acf(color, ci.type='ma')

#Exhibit 6.26
#Sample Partial ACF for the Color Property Series
pacf(color)

#Exhibit 6.27
#Box-Cox Power Transformation Results for Hare Abundance
data(hare)
acf(hare)
BoxCox.ar(hare)

#Exhibit 6.28
#Sample ACF for Square Root of Hare Abundance
acf(hare^.5)

#Exhibit 6.29
#Sample Partial ACF for Square Root of Hare Abundance
pacf(hare^.5)

#Exhibit 6.30
#Extended ACF for Difference of Logarithms of Oil Price Series
eacf(diff(log(oil.price)))

#Exhibit 6.31
#Best Subset ARMA Model for Difference of Log
res=armasubsets(y=diff(log(oil.price)),nar=7, nma=7,y.name = 'test', ar.method = 'ols')
plot(res)

#Exhibit 6.32
#Sample ACF of Difference of Logged Oil Prices
acf(as.vector(diff(log(oil.price))),xaxp=c(0,22,11))

#Exhibit 6.33
#Sample PACF of Differnce of Logged Oil Prices
pacf(as.vector(diff(log(oil.price))), xaxp=c(0,22,11))
