#Import libraries
library(ggplot2)
library(gcookbook)

#Making a basic Histogram
data("faithful")
ggplot(faithful, aes(x=waiting))+geom_histogram()

#Another basic Histogram
w<-faithful$waiting
ggplot(NULL, aes(x=w))+geom_histogram()

#(Set the width of each bin to 5)
ggplot(faithful, aes(x=waiting))+geom_histogram(binwidth = 5, fill="white",colour="black")
#(Divide the x range into 15 bins)
binsize<-diff(range(faithful$waiting))/15
ggplot(faithful, aes(x=waiting))+geom_histogram(binwidth = binsize, fill="white", colour="black")
#(Change the apperance)
h<-ggplot(faithful, aes(x=waiting))
h+geom_histogram(binwidth = 8, fill="white", colour="black",origin=31)
h+geom_histogram(binwidth = 8, fill="white",colour="black",origin=35)

#Making Multiple Histograms from Grouped Data
library(MASS)
data("birthwt")
ggplot(birthwt, aes(x=bwt))+geom_histogram(fill="white",colour="black")+facet_grid(smoke~.)
#(Change the labels)
birthwt1<-birthwt
birthwt1$smoke<-factor(birthwt1$smoke)
library(plyr)
birthwt1$smoke<-revalue(birthwt1$smoke,c("0"="No Smoke","1"="Smoke"))
ggplot(birthwt1, aes(x=bwt))+geom_histogram(fill="white",colour="black")+facet_grid(smoke~.)
ggplot(birthwt, aes(x=bwt))+geom_histogram(fill="white",colour="black")+facet_grid(race~.)
#(y scales to be resized independently)
ggplot(birthwt, aes(x=bwt))+geom_histogram(fill="white", colour="black")+facet_grid(race~., scales="free")
#(Mao the grouping variable to fill)
birthwt1$smoke<-factor(birthwt1$smoke)
ggplot(birthwt1, aes(x=bwt, fill=smoke))+geom_histogram(position="identity",alpha=0.4)

#Making a Density Curve
ggplot(faithful, aes(x=waiting))+geom_density()
#(Not along the side and bottom)
ggplot(faithful, aes(x=waiting))+geom_line(stat="density")+expand_limits(y=0)
#(Set the amount of smoothing)
ggplot(faithful, aes(x=waiting))+geom_line(stat='density', adjust=.25, colour="red")+geom_line(stat='density')+geom_line(stat='density',adjust=2,colour="blue")
#(Show more curve by changing x range)
ggplot(faithful, aes(x=waiting))+geom_density(fill="blue",alpha=.2)+xlim(35,105)
#(Draws a blue polygon with geom_density, then adds a line on top)
ggplot(faithful, aes(x=waiting))+geom_density(fill="blue",colour=NA, alpha=.2)+geom_line(stat='density')+xlim(35,105)
#(Compare the theoretical and observed distributions)
ggplot(faithful, aes(x=waiting, y=..density..))+geom_histogram(fill="cornsilk", colour="grey60",size=.2)+geom_density()+xlim(35,105)

#Making Multiple Density Curves from Grouped Data
library(MASS)
birthwt1<-birthwt
birthwt1$smoke<-factor(birthwt1$smoke)
ggplot(birthwt1, aes(x=bwt, colour=smoke))+geom_density()
ggplot(birthwt1, aes(x=bwt, fill=smoke))+geom_density(alpha=.3)
#(Facet graphs align vertically)
ggplot(birthwt1, aes(x=bwt))+geom_density()+facet_grid(smoke~.)
#(Changes the labels)
library(plyr)
birthwt1$smoke<-revalue(birthwt1$smoke, c("0"="No Smoke","1"="Smoke"))
ggplot(birthwt1, aes(x=bwt))+geom_density()+facet_grid(smoke~.)
#(Histogram along with Density Curve)
ggplot(birthwt1, aes(x=bwt, y=..density..))+geom_histogram(binwidth = 200, fill="cornsilk", colour="grey60",size=.2)+geom_density()+facet_grid(smoke~.)

#Making a Frequency Polygon
ggplot(faithful, aes(x=waiting))+geom_freqpoly()
#(Control the bins width for frequency polygon)
ggplot(faithful, aes(x=waiting))+geom_freqpoly(binwidth=4)
#(Controls the number of bins)
binsize<-diff(range(faithful$waiting))/15
ggplot(faithful, aes(x=waiting))+geom_freqpoly(binwidth=binsize)
#(Change the width of the boxes)
ggplot(birthwt, aes(x=factor(race),y=bwt))+geom_boxplot(width=.5)
#(Smaller points and hollow circles)
ggplot(birthwt, aes(x=factor(race),y=bwt))+geom_boxplot(outlier.size=1.5, outlier.shape = 21)
#Boxplot just for a single group
ggplot(birthwt, aes(x=1, y=bwt))+geom_boxplot()+scale_x_continuous(breaks=NULL)+theme(axis.title.x = element_blank())

#Adding Notches to a Box Plot
library(MASS)
ggplot(birthwt, aes(x=factor(race),y=bwt))+geom_boxplot(notch=TRUE)

#Adding Means to a Box Plot(Show on Diamond)
library(MASS)
ggplot(birthwt, aes(x=factor(race),y=bwt))+geom_boxplot()+stat_summary(fun.y="mean", geom="point",shape=23, size=3, fill="white")

#Making a Vioin Plot
library(gcookbook)
p<-ggplot(heightweight, aes(x=sex, y=heightIn))
p+geom_violin()
#(Hide outliers)
p+geom_violin()+geom_boxplot(width=.1, fill="black", outlier.colour = NA)+stat_summary(fun.y=median, geom="point",fill="white",shape=21, size=2.5)
#(Keep the tails)
p+geom_violin(trim=FALSE)
#(Scaled area proportional to number of observations)
p+geom_violin(scale="count")
#(More smoothing)
p+geom_violin(adjust=2)
#(Less smoothing)
p+geom_violin(adjust=.5)

#Wilkinson Dot Plot
library(gcookbook)
countries2009<-subset(countries, Year==2009&healthexp>2000)
p<-ggplot(countries2009, aes(x=infmortality))
p+geom_dotplot()
p+geom_dotplot(binwdith=.25)+geom_rug()+scale_y_continuous(breaks=NULL)+theme(axis.title.y=element_blank())
#(Use bins that are arranged with a fixed, regular spacing)
p+geom_dotplot(method="histodot",binwidth = .25)+geom_rug()+scale_y_continuous(breaks=NULL)+theme(axis.title.y=element_blank())
#(Stacked centerd the Dots)
p+geom_dotplot(binwidth = .25, stackdir = "center")+scale_y_continuous(breaks=NULL)+theme(axis.title.y=element_blank())
p+geom_dotplot(binwidth = .25, stackdir = "centerwhole")+scale_y_continuous(breaks=NULL)+theme(axis.title.y = element_blank())

#Making Multiple Dot Plots for Grouped Data
library(gcookbook)
data("heightweight")
ggplot(heightweight, aes(x=sex, y=heightIn))+geom_dotplot(binaxis="y",binwidth = .5, stackdir="center")
#(Make the dots hollow and have the box plots not show outliers)
ggplot(heightweight, aes(x=sex, y=heightIn))+geom_boxplot(outlier.colour = NA, width=.4)+geom_dotplot(binaxis = "y",binwidth = .5, stackdir="center", fill=NA)
#(Show x tick labels as text corresponding to the factor levels)
ggplot(heightweight, aes(x=sex, y=heightIn))+geom_boxplot(aes(x=as.numeric(sex)+.2, group=sex), width=.25)+geom_dotplot(aes(x=as.numeric(sex)-.2, group=sex),binaxis = "y", binwidth = .5, stackdir = "center")+scale_x_continuous(breaks=1:nlevels(heightweight$sex), labels=levels(heightweight$sex))

#Making a Density Plot of Two-Dimensional Data
data("faithful")
p<-ggplot(faithful,aes(x=eruptions, y=waiting))
p+geom_point()+stat_density2d()
#(Map the hieght of the desity curve to the color of the contour lines)
p+stat_density2d(aes(colour=..level..))
#(Map density estimate to fill color)
p+stat_density2d(aes(fill=..density..), geom="raster",contour=FALSE)
#(With points, and map density estimate to alpha)
p+geom_point()+stat_density2d(aes(alpha=..density..),geom="tile",contour = FALSE)
#(Control the bandwith of the estimate)
p+stat_density2d(aes(fill=..density..),geom="raster",contour=FALSE, h=c(.5,5))
