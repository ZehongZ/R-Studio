#Import libraries 
library(ggplot2)
library(gcookbook)

#Adding Text Annotations
data("faithful")
p<-ggplot(faithful, aes(x=eruptions, y=waiting))+geom_point()
p
p+annotate("text",x=3,y=48, label="Group 1")+annotate("text", x=4.5, y=66, label="Group 2")
#(Add any type of geometric object)
p+annotate("text", x=3, y=48, label="Group 1", family="serif", fontface="italic",colour="darkred",size=3)+annotate("text",x=4.5, y=66, label="Group 2", family="serif", fontface="italic",colour="darkred",size=3)
#(Using geom_text())
p+annotate('text',x=3, y=48, label="Group 1", alpha=.3)+geom_text(x=4.5, y=66, label="Group 2", alpha=.1)
#(If the axes are continuous, you can use the special values Inf and -Inf to place text annotations)
p+annotate("text", x=-Inf, y=Inf, label="Upper left", hjust=-.2, vjust=2)+annotate("text",x=mean(range(faithful$eruptions)), y=-Inf, vjust=-0.4, label="Bottom middle")

#Using Mathematical Expressions in Annotations
p<-ggplot(data.frame(x=c(-3,3)),aes(x=x))+stat_function(fun=dnorm)#Normal curve
p+annotate("text", x=2, y=0.3, parse=TRUE,label="frac(1, sqrt(2*pi))*e^{-x^2/2}")
#(Mix regular text with expressions)
p+annotate("text",x=0,y=0.05,parse=TRUE, size=4, label="'Function: '* y==frac(1, sqrt(2*pi))*e^{-x^2/2}")

#Adding Lines
library(gcookbook)
data("heightweight")
p<-ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex))+geom_point()
p
#(Add horizontal and vertical lines)
p+geom_hline(yintercept = 60)+geom_vline(xintercept = 14)
#(Add angled line)
p+geom_abline(intercept=37.4, slope=1.75)
#(Map values from the data to xintercept, yintercept)
library(plyr)
hw_means<-ddply(heightweight, "sex", summarise, heightIn=mean(heightIn))
hw_means
p+geom_hline(aes(yintercept=heightIn, colour=sex), data=hw_means, linetype="dashed",size=1)
#(Specify the numerical intercept manually)
data("PlantGrowth")
pg<-ggplot(PlantGrowth, aes(x=group, y=weight))+geom_point()
pg
pg+geom_vline(xintercept = 2)
pg+geom_vline(xintercept = which(levels(PlantGrowth$group)=="ctrl"))

#Adding Line Segments and Arrows
library(gcookbook)
p<-ggplot(subset(climate, Source=="Berkeley"), aes(x=Year, y=Anomaly10y))+geom_line()
p
p+annotate("segment",x=1950, xend=1980, y=-.25, yend=-.25)
#(Add arrowheads or flat ends to the line segments)
library(grid)
p+annotate("segment",x=1850, xend=1820, y=-.8, yend=-.95, colour="blue", size=2, arrow=arrow())+annotate("segment",x=1950,xend=1980, y=-.25, yend=-.25, arrow=arrow(ends="both",angle=90, length=unit(.2,"cm")))

#Adding a Shaded rectable
library(gcookbook)
p<-ggplot(subset(climate, Source=="Berkeley"),aes(x=Year,y=Anomaly10y))+geom_line()
p
p+annotate("rect",xmin=1950, xmax=1980, ymin=-1, ymax=1, alpha=.1,fill="blue")

#Highlightling an Item
data("PlantGrowth")
pg<-PlantGrowth
pg$hl<-"no"
pg$hl[pg$group=="trt2"]<-'yes'
ggplot(pg, aes(x=group, y=weight, fill=hl))+geom_boxplot()+scale_fill_manual(values=c("grey85","#FFDDCC"),guide=FALSE)
#(Use original column)
ggplot(PlantGrowth, aes(x=group, y=weight, fill=group))+geom_boxplot()+scale_fill_manual(values=c("grey85","grey85","#FFDDCC"),guide=FALSE)

#Edding Error Bars
library(gcookbook)
data("cabbage_exp")
ce<-subset(cabbage_exp, Cultivar=="c39")
#(With a bar graph)
ggplot(ce, aes(x=Date, y=Weight))+geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se),width=.2)
#(With a line graph)
ggplot(ce, aes(x=Date, y=Weight))+geom_line(aes(group=1))+geom_point(size=4)+geom_errorbar(aes(ymin=Weight-se,ymax=Weight+se),width=.2)
#(Bad:dodge width not specified)
data("cabbage_exp")
ggplot(cabbage_exp, aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(position = "dodge",stat = 'identity')+geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), position = "dodge",width=.2)
#(Good: dodge width width set to same as bar with (0.9))
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(position="dodge",stat = "identity")+geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), position=position_dodge(0.9),width=.2)
#(Dodge all the geometric elements so will aligh with the error bars)
pd<-position_dodge(.3)
ggplot(cabbage_exp, aes(x=Date, y=Weight, colour=Cultivar, group=Cultivar))+geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2,size=0.25, colour="black",position = pd)+geom_line(position=pd)+geom_point(position = pd, size=2.5)

#Adding Annotations to Individual Facets
data("mpg")
p<-ggplot(mpg, aes(x=displ, y=hwy))+geom_point()+facet_grid(.~drv)
p
f_labels<-data.frame(drv=c("4","f","r"), label=c("4wd","Front","Rear"))
p+geom_text(x=6,y=40, aes(label=label), data=f_labels)
p+annotate("text", x=6, y=42, label="label text")
