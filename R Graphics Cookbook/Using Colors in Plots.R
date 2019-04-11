#Import libraries
library(ggplot2)
library(gcookbook)

#Setting the Colors of Objects
ggplot(mtcars, aes(x=wt, y=mpg))+geom_point(colour="red")
library(MASS)
ggplot(birthwt, aes(x=bwt))+geom_histogram(fill="red",colour="black")

#Mapping Variables to Colors
library(gcookbook)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(colour="black",position = "dodge", stat="identity")
ggplot(cabbage_exp, aes(x=Date, y=Weight))+geom_bar(aes(fill=Cultivar),colour="black",position = "dodge", stat="identity")
ggplot(mtcars, aes(x=wt, y=mpg, colour=cyl))+geom_point()
ggplot(mtcars, aes(x=wt, y=mpg))+geom_point(aes(colour=cyl))
#(Categorical)
m<-mtcars
m$cyl<-factor(m$cyl)
ggplot(m, aes(x=wt, y=mpg, colour=cyl))+geom_point()

#Using a Different Palette for a Discrete Variable
library(gcookbook)
p<-ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup))+geom_area()
p
p+scale_fill_discrete()
p+scale_fill_hue()
#(Color Brewer Palette)
p+scale_fill_brewer()
#(Change of Color Scale)
h<-ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex))+geom_point()
h+scale_color_hue(l=45)#Slightly darker
library(RColorBrewer)
p+scale_fill_brewer(palette="Oranges")
p+scale_fill_grey()
#(Different range of greys)
p+scale_fill_grey(start=0.7, end=0)

#Using a Manuaaly Defined Palette for a Discrete Variable
library(gcookbook)
h<-ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex))+geom_point()
#(Folor Names)
h+scale_color_manual(values=c("red","blue"))
#(RGB values)
h+scale_color_manual(values = c("#CC6666","#7777DD"))
#(Specify the colors in a different order by using a named vector)
h+scale_color_manual(values = c(m="blue",f="red"))

#Using a Manully Defined Palette for a Continuous variable
library(gcookbook)
p<-ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb))+geom_point(size=3)
#(With a gradient between two colors)
p+scale_colour_gradient(low="black",high="white")
#(A gradient with a white midpoint)
library(scales)
p+scale_color_gradient2(low=muted("red"), mid="white",high=muted("blue"),midpoint = 110)
#(A gradient of n colors)
p+scale_colour_gradientn(colours=c("darkred","orange","yellow","white"))

#Coloring a Shaded Region Based on Value
library(gcookbook)
cb<-subset(climate ,Source=="Berkeley")
cb$valence[cb$Anomaly10y>=0]<- "pos"
cb$valence[cb$Anomaly10y<0]<-"neg"
cb
ggplot(cb, aes(x=Year, y=Anomaly10y))+geom_area(aes(fill=valence))+geom_line()+geom_hline(yintercept = 0)

interp<-approx(cb$Year,cb$Anomaly10y,n=1000)
cbi<-data.frame(Year=interp$x, Anomaly10y=interp$y)
cbi$valence[cbi$Anomaly10y>=0]<-"pos"
cbi$valence[cbi$Anomaly10y<0]<-"neg"

#(Make the shaded regions partially transparent,change the colors,remove the legend,remove the padding on the left and right sides)
ggplot(cbi, aes(x=Year, y=Anomaly10y))+geom_area(aes(fill=valence),alpha=.4)+geom_line()+geom_hline(yintercept = 0)+scale_fill_manual(values = c("#CCEEFF","#FFDDDD"),guide=FALSE)+scale_x_continuous(expand = c(0,0))
