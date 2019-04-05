#Import libraries#
library(ggplot2)
library(gcookbook)

#Scatterplot#
data("mtcars")
plot(mtcars$wt, mtcars$mpg)
qplot(mtcars$wt,mtcars$mpg)
ggplot(mtcars,aes(x=wt, y=mpg))+geom_point()

#Line Graph#
data("pressure")
plot(pressure$temperature,pressure$pressure,type='l')
#(add point or multiple lines)
plot(pressure$temperature,pressure$pressure, type='l')
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2, col="red")
points(pressure$temperature,pressure$pressure/2, col="red")

qplot(pressure$temperature, pressure$pressure,geom="line")
ggplot(pressure, aes(x=temperature, y=pressure))+geom_line()

qplot(temperature, pressure, data=pressure, geom=c("line","point"))
ggplot(pressure,aes(x=temperature, y=pressure))+geom_line()+geom_point()

#Bar Graph#
data("BOD")
barplot(BOD$demand,names.arg=BOD$Time)
qplot(BOD$Time, BOD$demand, geom="bar",stat="identity")

data("mtcars")
table(mtcars$cyl)
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))

#(Bar graph of values)#
ggplot(BOD, aes(x=Time, y=demand))+geom_bar(stat="identity")
#(Bar graph of counts)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()


#Histogram#
data("mtcars")
hist(mtcars$mpg)
qplot(mtcars$mpg)
#Specify approcimate number of bins with breaks
hist(mtcars$mpg,breaks=10)
qplot(mpg, data=mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg))+geom_histogram(binwidth = 4)

#Box Plot#
data("ToothGrowth")
boxplot(len~supp, data=ToothGrowth)
qplot(ToothGrowth$supp,ToothGrowth$len,geom="boxplot")
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
#(Put interaction of two variables on x-axis)
boxplot(len~supp+dose, data=ToothGrowth)
#(Using three separte vectors)
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom="boxplot")
#(Get the columns from the data frame)
qplot(interaction(supp,dose),len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()

#Plotting a Function Curve
curve(x^3-5*x, from=-4,to=4)
#(Plot a user-defined function)
myfun<-function(xvar){
  1/(1+exp(-xvar+10))
}
curve(myfun(x),add=TRUE, col="red")
qplot(c(0,20),fun=myfun,stat="function",geom="line")
ggplot(data.frame(x=c(0,20)),aes(x=x))+stat_function(fun=myfun,geom="line")
