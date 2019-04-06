#Import libraries
library(ggplot2)
library(gcookbook)

#Import data
data("BOD")

#Basic Line Graph
ggplot(BOD, aes(x=Time, y=demand))+geom_line()

#Basic Line Graph with categorical variables
BOD1<-BOD
BOD1$Time<-factor(BOD1$Time)
ggplot(BOD1,aes(x=Time, y=demand, group=1))+geom_line()

#Use ylim() or expand_limits() to include y values
ggplot(BOD, aes(x=Time, y=demand))+geom_line()+ylim(0, max(BOD$demand))
ggplot(BOD, aes(x=Time, y=demand))+geom_line()+expand_limits(y=0)

#Addig Points to a Line Graph
ggplot(BOD, aes(x=Time, y=demand))+geom_line()+geom_point()

#Displaying points on the graph illustrates when each estimate was made
library(gcookbook)
data("worldpop")
ggplot(worldpop, aes(x=Year, y=Population))+geom_line()+geom_point()
#(With a log y-axis)
ggplot(worldpop, aes(x=Year, y=Population))+geom_line()+geom_point()+scale_y_log10()

#Making a Line Graph with Multiple Lines
library(plyr)
data("ToothGrowth")
tg<-ddply(ToothGrowth, c("supp","dose"), summarise, length=mean(len))
#(Map supp to colour)
ggplot(tg, aes(x=dose, y=length, colour=supp))+geom_line()
#(Map supp to linetype)
ggplot(tg, aes(x=dose, y=length,linetype=supp))+geom_line()
#(Map variables to properties of the points such as shpae and fill)
ggplot(tg, aes(x=dose, y=length, shape=supp))+geom_line()+geom_point(size=4)
ggplot(tg, aes(x=dose, y=length, fill=supp))+geom_line()+geom_point(size=4, shape=21)
#(Using dodge when points are overlap)
ggplot(tg, aes(x=dose, y=length, shape=supp))+geom_line(position=position_dodge(0.2))+geom_point(position=position_dodge(0.2),size=4)

#Changing the Appearance of Lines
ggplot(BOD, aes(x=Time, y=demand))+geom_line(linetype='dashed',size=1, colour="blue")
#(Using different palett)
library(plyr)
tg<-ddply(ToothGrowth,c('supp','dose'),summarise,length=mean(len))
ggplot(tg,aes(x=dose, y=length, colour=supp))+geom_line()+scale_colour_brewer(palette="Set1")
#Set a single constant color for all the lines
ggplot(tg,aes(x=dose, y=length, group=supp))+geom_line(colour="darkgreen",size=1.5)
ggplot(tg,aes(x=dose, y=length,colour=supp))+geom_line(linetype="dashed")+geom_point(shape=22, size=3, fill="white")

#Changing the Apperance of Points
data("BOD")
ggplot(BOD, aes(x=Time, y=demand))+geom_line()+geom_point(size=4, shape=22, colour="darkred",fill="pink")
#(Fill it with whilte)
ggplot(BOD, aes(x=Time ,y=demand))+geom_line()+geom_point(size=4, shape=21, fill="white")
#(draw differently colored points for each group by mapping variables to aesthetic properties of points, inside of aes().)
library(plyr)
tg<-ddply(ToothGrowth,c("supp","dose"),summarise,length=mean(len))
head(tg)
pd<-position_dodge(0.2)
ggplot(tg, aes(x=dose, y=length, fill=supp))+geom_line(position = pd)+geom_point(shape=21,size=3, position = pd)+scale_fill_manual(values = c("black","white"))

#Making a Graph with a Shaded Area
sunspotyear<-data.frame(Year=as.numeric(time(sunspot.year)),Sunspots=as.numeric(sunspot.year))
ggplot(sunspotyear, aes(x=Year, y=Sunspots))+geom_area()
#(Changing fill colors,alpha changes transparency)
ggplot(sunspotyear,aes(x=Year, y=Sunspots))+geom_area(colour="black",fill="blue",alpha=.2)
#(Draw area without an outline)
ggplot(sunspotyear, aes(x=Year, y=Sunspots))+geom_area(fill="blue",alpha=.2)+geom_line()

#Making a Stacked Area Graph
library(gcookbook)
data("uspopage")
ggplot(uspopage, aes(x=Year, y=Thousands,fill=AgeGroup))+geom_area()
#(Reverse legend, changes the palette,add thin lines)
ggplot(uspopage,aes(x=Year, y=Thousands, fill=AgeGroup))+geom_area(colour="black",size=.2,alpha=.4)+scale_fill_brewer(palette = "Blues",breaks=rev(levels(uspopage$AgeGroup)))
#(Reverse the stacking order)
ggplot(uspopage,aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup)))+geom_area(colour="black",size=.2,alpha=.4)+scale_fill_brewer(palette = "Blues")
#(Get ride of left and right lines)
ggplot(uspopage,aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup)))+geom_area(colour=NA, alpha=.4)+scale_fill_brewer(palette = "Blues")+geom_line(position = "stack",size=.2)

#Making a Proportional Stacked Area Graph
library(gcookbook)
library(plyr)
uspopage_prop<-ddply(uspopage, "Year", transform, Percent=Thousands/sum(Thousands)*100)
ggplot(uspopage_prop,aes(x=Year, y=Percent, fill=AgeGroup))+geom_area(colour="black",size=2, alpha=.4)+scale_fill_brewer(palette="Blues",breaks=rev(levels(uspopage$AgeGroup)))

#Adding a Confidence Region
library(gcookbook)
data("climate")
clim<-subset(climate, Source=="Berkeley",select=c("Year","Anomaly10y","Unc10y"))
clim
ggplot(clim, aes(x=Year, y=Anomaly10y))+geom_ribbon(aes(ymin=Anomaly10y-Unc10y,ymax=Anomaly10y+Unc10y),alpha=0.2)+geom_line()
#(Reverse line and ribbon)
ggplot(clim, aes(x=Year, y=Anomaly10y))+geom_line(aes(y=Anomaly10y-Unc10y),colour="grey50", linetype="dotted")+geom_line(aes(y=Anomaly10y+Unc10y),colour="grey50",linetype="dotted")+geom_line()
                                                  