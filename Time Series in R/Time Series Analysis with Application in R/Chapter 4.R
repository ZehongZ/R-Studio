library(TSA)

#Exhibit 4.2
#Time Plot of an MA(1) process
data("ma1.2.s")
plot(ma1.2.s,ylab=expression(Y[t]), type='o')

#Exhibit 4.3
#Plot of Yt vs. Yt-1 for MA(1) Series in Exhibit 4.2
plot(y=ma1.2.s, x=zlag(ma1.2.s),ylab=expression(Y[t]),xlab=expression(Y(t-1)), type='p')

#Exhibit 4.4
#plot of Yt vs. Yt-2 for MA(1) Series in Exhibit 4.2
plot(y=ma1.2.s, x=zlag(ma1.2.s,2),ylab=expression(Y[t]), xlab=expression(Y[t-2]), type='p')

#Exhibit 4.5
#Time Plot of MA(1) process with positive trend
data("ma1.1.s")
plot(ma1.1.s, ylab=expression(Y[t]), type='o')

#Exhibit 4.6
#Plot of Yt versus Yt-1 for MA(1) Sereis in Exhibit 4.5
plot(y=ma1.1.s, x=zlag(ma1.1.s), ylab=expression(Y[t]), xlab=expression(Y[t-1]), type='p')#Show apprent negative trend

#Exhibit 4.7
#Plot of Yt versus Yt-2 for MA(1) Series in Exhibit 4.5
plot(y=ma1.1.s,x=zlag(ma1.1.s,2), ylab=expression(Y[t]), xlab = expression(Y[t-2]), type='p')

#Exhibit 4.8
#Time Plot of an MA(2) Process
data("ma2.s")
plot(ma2.s,ylab=expression(Y[t]), type='o')

#Exhibit 4.9
#Plot of Yt vs Yt-1 for MA(2) Series 
plot(y=ma2.s, x=zlag(ma2.s), ylab=expression(Y[t]), xlab = expression(Y[t-1]), type='p')

#Exhibit 4.10
#Plot of Yt vs Yt-2 for MA(2) Series 
plot(y=ma2.s, x=zlag(ma2.s,2), ylab=expression(Y[t]), xlab=expression(Y[t-2]), type='p')

#Exhibit 4.11
#Plot of Yt vs. Yt-3 for MA(2) Series
plot(y=ma2.s, x=zlag(ma2.s,3), ylab=expression(Y[t]), xlab=expression(Y[t-3]), type='p')

#Exhibit 4.13
#Time Plot of an AR(1) Series
data("ar1.s")
plot(ar1.s, ylab=expression(Y[t]), type='o')

#Exhibit 4.14
#Plot of Yt vs. Yt-1 for AR(1)
plot(y=ar1.s, x=zlag(ar1.s), ylab=expression(Y[t]),xlab = expression(Y[t-1]), type='p')

#Exhibit 4.15
#Plot of Yt vs Yt-1 for AR(1) Series
plot(y=ar1.s, x=zlag(ar1.s,2), ylab=expression(Y[t]), xlab=expression(Y[t-2]), type='p')

#Exhibit 4.16
#Plot of Yt versus Yt-3 for AR(1) Series
plot(y=ar1.s, x=zlag(ar1.s,3), ylab=expression(Y[t]),xlab = expression(Y[t-3]), type='p')

#Exhibit 4.19
#Time Plot of a AR(2) Series
data(ar2.s)
plot(ar2.s, ylab=expression(Y[t]), type='o')

