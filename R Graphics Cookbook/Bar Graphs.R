library(ggplot2)
library(gcookbook)

#Basic Bar Graph
data("pg_mean")
ggplot(pg_mean, aes(x=group, y=weight))+geom_bar(stat='identity')

#Convert the continuous variable to a discrete variable
data("BOD")
str(BOD)
ggplot(BOD,aes(x=factor(Time),y=demand))+geom_bar(stat='identity')

#Color fill
ggplot(pg_mean,aes(x=group, y=weight))+geom_bar(stat='identity',fill='lightblue',colour='black')

#Grouping Bars Together
library(gcookbook)
data("cabbage_exp")
ggplot(cabbage_exp,aes(x=Date,y=Weight,fill=Cultivar))+geom_bar(position='dodge',stat='identity')
#(scale_fill_brewer() to set the color)
ggplot(cabbage_exp,aes(x=Date,y=Weight, fill=Cultivar))+geom_bar(position='dodge',colour='black',stat='identity')+scale_fill_brewer(palette='Pastel1')

ce<-cabbage_exp[1:5,]
ce
ggplot(ce,aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(position = 'dodge',colour='black',stat='identity')+
  scale_fill_brewer(palette = 'Pastel1')

#Making a Bar Graph of Counts
data("diamonds")
ggplot(diamonds,aes(x=cut))+geom_bar()
ggplot(diamonds,aes(x=carat))+geom_bar()

#Using Colors in a Bar Graph
library(gcookbook)
data("uspopchange")
upc<-subset(uspopchange,rank(Change)>40)
upc
ggplot(upc,aes(x=Abb, y=Change, fill=Region))+geom_bar(stat = 'identity')
ggplot(upc,aes(x=reorder(Abb, Change),y=Change,fill=Region))+geom_bar(stat='identity',colour='black')+
  scale_fill_manual(values = c('#669933','#FFCC66'))+xlab('State')

#Coloring Negative and Positive Bars Differently
library(gcookbook)
data("climate")
csub<-subset(climate,Source=='Berkeley'&Year>=1900)
csub$pos<-csub$Anomaly10y>=0
csub
ggplot(csub,aes(x=Year,y=Anomaly10y,fill=pos))+geom_bar(stat='identity',position = 'identity')
ggplot(csub, aes(x=Year,y=Anomaly10y,fill=pos))+geom_bar(stat='identity',position='identity',colour='black',size=0.25)+scale_fill_manual(values=c('#CCEEFF','#FFDDDD'),guide=FALSE)

#Adjusting Bar Width and Spacing
library(gcookbook)
ggplot(pg_mean, aes(x=group, y=weight))+geom_bar(stat='identity',width = 0.5)
#(For a grouped bar graph with narrow bars)
ggplot(cabbage_exp,aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat = 'identity',width=0.5,position='dodge')
#For a grouped bar with some space between the bars
ggplot(cabbage_exp,aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat = 'identity',width=0.5,position=position_dodge(0.7))

#Making a Stacked Bar Graph
library(gcookbook)
data("cabbage_exp")
ggplot(cabbage_exp,aes(x=Date,y=Weight, fill=Cultivar))+geom_bar(stat='identity')
#(Reverse the order or legends)
ggplot(cabbage_exp,aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity')+guides(fill=guide_legend(reverse=TRUE))
#(Reverse the stacking order)
library(plyr)
ggplot(cabbage_exp,aes(x=Date,y=Weight, fill=Cultivar,order=desc(Cultivar)))+geom_bar(stat='identity')
#(Get black outline)
ggplot(cabbage_exp,aes(x=Date,y=Weight, fill=Cultivar))+geom_bar(stat='identity',colour='black')+guides(fill=guide_legend(reverse = TRUE))+scale_fill_brewer(palette = 'Pastel1')

#Making a Proportional Stacked Bar Graph
library(gcookbook)
library(plyr)
ce<-ddply(cabbage_exp, 'Date',transform,percent_weight=Weight/sum(Weight)*100)
ggplot(ce,aes(x=Date, y=percent_weight, fill=Cultivar))+geom_bar(stat='identity')
#(Using ddply() to calculate weight)
ddply(cabbage_exp,'Date',transform,percent_weight=Weight/sum(Weight)*100)
ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar))+geom_bar(stat='identity',colour='black')+guides(fill=guide_legend(reverse=TRUE))+scale_fill_brewer(palette='Pastel1')

#Adding Labels to a Bar Graph
library(gcookbook)
#(Below the top)
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar),y=Weight))+geom_bar(stat='identity')+geom_text(aes(label=Weight), vjust=1.5, colour='white')
#(Above the top)
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar),y=Weight))+geom_bar(stat='identity')+geom_text(aes(label=Weight), vjust=-0.2)
#(adjust y limits to be a little higher)
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat='identity')+geom_text(aes(label=Weight),vjust=-0.2)+ylim(0,max(cabbage_exp$Weight)*1.05)
#(Map y position slightly above bar top-y range of plot will auto-adjust)
ggplot(cabbage_exp,aes(x=interaction(Date,Cultivar),y=Weight))+geom_bar(stat='identity')+geom_text(aes(y=Weight+0.1,label=Weight))
#(Specify font size)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity',position='dodge')+geom_text(aes(label=Weight),vjust=1.5, colour='white',position=position_dodge(.9),size=3)
#(Putting labes on stacked bar graphs)
library(plyr)
ce<-arrange(cabbage_exp, Date, Cultivar)
ce<-ddply(ce, 'Date',transform, label_y=cumsum(Weight))
ce
ggplot(ce, aes(x=Date,y=Weight, fill=Cultivar))+geom_bar(stat='identity')+geom_text(aes(y=label_y,label=Weight),vjust=1.5,colour='white')
#(Putting the label in the middle of each bar)
ce<-arrange(cabbage_exp, Date, Cultivar)
ce<-ddply(ce, "Date",transform, label_y=cumsum(Weight)-0.5*Weight)
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity')+geom_text(aes(y=label_y,label=Weight),colour='white')
#change the legend order and colors, add labels in the middle with a smaller font using size, add a “kg” using paste, and make sure there are always two digits after the decimal point by using format
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar))+geom_bar(stat='identity',colour='black')+geom_text(aes(y=label_y, label=paste(format(Weight, nsmall = 2),'kg')),size=4)+guides(fill=guide_legend(reverse = TRUE))+scale_fill_brewer(palette = 'Pastel1')

#Cleveland Dot Plot
#Cleveland dot plots are sometimes used instead of bar garphs because they reduce visual clutter and are easier to read
library(gcookbook)
data("tophitters2001")
tophit<-tophitters2001[1:25,]
ggplot(tophit, aes(x=avg,y=name))+geom_point()
ggplot(tophit, aes(x=avg, y=reorder(name, avg)))+geom_point(size=3)+theme_bw()+theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_line(colour='grey60',linetype='dashed'))
#(Swap the axes so that the names go along the x-axis and the values go along the y-axis)
ggplot(tophit, aes(x=reorder(name, avg),y=avg))+geom_point(size=3)+theme_bw()+theme(axis.text.x = element_text(angle=60, hjust=1),panel.grid.major.y = element_blank(),panel.grid.minor.y = element_blank(),panel.grid.major.x = element_line(colour='grey60',linetype='dashed'))
#(Group the items by another variable)
nameorder<-tophit$name[order(tophit$lg, tophit$avg)]
tophit$name<-factor(tophit$name,levels=nameorder)
ggplot(tophit,aes(x=avg,y=name))+geom_segment(aes(yend=name),xend=0,colour='grey50')+geom_point(size=3,aes(colour=lg))+scale_colour_brewer(palette='Set1',limits=c("NL","AL"))+theme_bw()+theme(panel.grid.major.y = element_blank(),legend.position = c(1,0.55),legend.justification = c(1,0.5))
#(Using facets to separate the two groups)
ggplot(tophit,aes(x=avg,y=name))+geom_segment(aes(yend=name),xend=0, colour="grey50")+geom_point(size=3,aes(colour=lg))+scale_colour_brewer(palette="Set1",limits=c("NL","AL"), guide=FALSE)+theme_bw()+theme(panel.grid.major.y = element_blank())+facet_grid(lg~.,scales="free_y",space = "free_y")
