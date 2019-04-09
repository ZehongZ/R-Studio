#Import libraries
library(ggplot2)
library(gcookbook)

#Swap x-axes and y-axes on a graph
data("PlantGrowth")
ggplot(PlantGrowth, aes(x=group, y=weight))+geom_boxplot()
ggplot(PlantGrowth, aes(x=group, y=weight))+geom_boxplot()+coord_flip()
ggplot(PlantGrowth, aes(x=group, y=weight))+geom_boxplot()+coord_flip()+scale_x_discrete(limits=rev(levels(PlantGrowth$group)))

#Setting the Range of a Continuous Axis
p<-ggplot(PlantGrowth, aes(x=group, y=weight))+geom_boxplot()
p
p+ylim(0, max(PlantGrowth$weight))

p+scale_y_continuous(limits=c(0,10), breaks=NULL)
#(Coordiate transform)
p+scale_y_continuous(limits=c(5,6.5))
#(Expand range)
p+expand_limits(y=0)

#Reversing a Continuous Axis
ggplot(PlantGrowth, aes(x=group, y=weight))+geom_boxplot()+scale_y_reverse()

#Changing the Order of Items on a categorical Axis
p<-ggplot(PlantGrowth, aes(x=group, y=weight))+geom_boxplot()
p+scale_x_discrete(limits=c("trt1","ctrl","trt2"))
p+scale_x_discrete(limits=rev(levels(PlantGrowth$group)))

#Setting the Scaling Ratio of the X and Y Axes
library(gcookbook)
data("marathon")
sp<-ggplot(marathon, aes(x=Half,y=Full))+geom_point()
sp+coord_fixed()
#(Same Scaling of the data)
sp+coord_fixed()+scale_y_continuous(breaks=seq(0, 420,30))+scale_x_continuous(breaks = seq(0, 420,30))
#(Fix ratio between the axes)
sp+coord_fixed()+scale_y_continuous(breaks=seq(0, 420, 30))+scale_x_continuous(breaks=seq(0, 420, 15))

#Setting the Positions of Tick Marks
ggplot(PlantGrowth, aes(x=group, y=weight))+geom_boxplot()
ggplot(PlantGrowth, aes(x=group, y=weight))+geom_boxplot()+scale_y_continuous(breaks=c(4,4.25,4.5,5,6,8))
#(Using seq() function)
seq(4,7, by=.5)
#(Set both breaks and labels for a discrete axis)
ggplot(PlantGrowth, aes(x=group, y=weight))+geom_boxplot()+scale_x_discrete(limits=c("trt2","ctrl"), breaks="ctrl")

#Removing Tick Marks and Labels
p<-ggplot(PlantGrowth, aes(x=group, y = weight))+geom_boxplot()
p+theme(axis.text.y = element_blank())
p+theme(axis.ticks = element_blank(), axis.text.y=element_blank())
p+scale_y_continuous(breaks=NULL)

#Changing the Text of Tick Labels
library(gcookbook)
data("heightweight")
hwp<-ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point()
hwp
#(To set arbitrary labels, pass values to breaks and labels in the scale)
hwp+scale_y_continuous(breaks=c(50,56,60,66,72), labels=c("Tiny","Really\nshot","Short","Medium","Tallish"))

#Changing the Appearance of Tick Labels
bp<-ggplot(PlantGrowth, aes(x=group, y=weight))+geom_boxplot()+scale_x_discrete(breaks=c("ctrl","trt1","trt2"), labels=c("Control","Treatment 1","Treatment 2"))
bp
bp+theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))
#(Rotating the text 30 degress)
bp+theme(axis.text.x=element_text(angle=30, hjust=1, vjust=1))
#(Other text properties)
bp+theme(axis.text.x=element_text(family="Times", face="italic",colour="darkred",size=rel(0.9)))

#Changing the Text of Axis Labels
library(gcookbook)
hwp<-ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex))+geom_point()
hwp
hwp+xlab("Age in years")+ylab("Height in inches")
#(Descriptive axis labels)
hwp+labs(x="Age in years", y="Height in inches")
hwp+scale_x_continuous(name="Age in years")
hwp+scale_x_continuous(name="Age\n(years")

#Removing Axis Labels
p<-ggplot(PlantGrowth, aes(x=group, y=weight))+geom_boxplot()
p+theme(axis.title.x=element_blank())

#Changing the Appearance of Axis Labels
library(gcookbook)
hwp<-ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point()
hwp+theme(axis.title.x=element_text(face="italic",colour="darkred",size=14))
#(Rotation)
hwp+ylab("Height\n(inches)")+theme(axis.title.y = element_text(angle=90, face="italic",colour="darkred",size=14))

#Showing Lines Along the Axes
library(gcookbook)
p<-ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point()
p+theme(axis.line = element_line(colour="black"))
#(Starting with a theme that has a border around the plotting area)
p+theme_bw()+theme(panel.border = element_blank(), axis.line = element_line(colour="black"))
p+theme_bw()+theme(panel.border = element_blank(),axis.line=element_line(colour="black",size = 4))
p+theme_bw()+theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=4, lineend = "square"))

#Using a Logarithmic Axis
library(MASS)
p<-ggplot(Animals, aes(x=body, y=brain, label=rownames(Animals)))+geom_text(size=3)
p
p+scale_x_log10()+scale_y_log10()

#Making a Circular Graph
library(gcookbook)
data("wind")
ggplot(wind, aes(x=DirCat, fill=SpeedCat))+geom_histogram(binwidth = 15, origin=-7.5)+coord_polar()+scale_x_continuous(limits=c(0,360))

#(Reverse the legend)
ggplot(wind, aes(x=DirCat, fill=SpeedCat))+geom_histogram(binwidth=15, origin=-7.5, colour="black", size=.25)+guides(fill=guide_legend(reverse=TRUE))+coord_polar()+scale_x_continuous(limits=c(0,360),breaks=seq(0, 360, by=45), minor_breaks = seq(0, 360, by=15))+scale_fill_brewer()

#Using Dates on an Axis
data("economics")
str(economics)
ggplot(economics, aes(x=date, y=psavert))+geom_line()
#(Specifying the breaks)
econ<-subset(economics, date>=as.Date("1992-05-01")&date<as.Date("1993-06-01"))
p<-ggplot(econ, aes(x=date, y=psavert))+geom_line()
p
#(Specify breaks as a Date vector)
datebreaks<-seq(as.Date("1992-06-01"), as.Date("1993-06-01"), by="2 month")
#(Use breaks and rotate text labels)
p+scale_x_date(breaks=datebreaks)+theme(axis.text.x=element_text(angle=30, hjust=1))
#(Specifying the formatting of the breaks changed)
library(scales)
p+scale_x_date(breaks=datebreaks, labels=date_format("%Y %b"))+theme(axis.text.x = element_text(angle=30, hjust=1))

