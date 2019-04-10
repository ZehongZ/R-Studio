#Import packages
library(ggplot2)
library(gcookbook)

#Setting the Title of a Graph
data("heightweight")
p<-ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point()
p+ggtitle("Age and Height of Schoolchildren")
#(Create a new line)
p+ggtitle("Age and Height\nof Schoolchildren")
#(Move title inside the plotting)
p+ggtitle("Age and Height of Schoolchildren")+theme(plot.title = element_text(vjust=-5.5))
p+annotate("text",x=mean(range(heightweight$ageYear)),y=Inf,label="Age and Height of Schoolchildren",vjust=1.5, size=6)

#Changing the Appearance of Text
p<-ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point()
p+theme(axis.title.x=element_text(size=16, lineheight = .9, family = "Times",face="bold.italic",colour="red"))
p+ggtitle("Age and Height\nof Schoolchildren")+theme(plot.title = element_text(size=rel(1.5),lineheight=.9, family = "Times",face="bold.italic",colour="red"))
#(Set the text properties)
p+annotate("text", x=15,y=53, label="Some Text",size=7, family="Times", fontface="bold.italic",colour="red")
p+geom_text(aes(label=weightLb), size=4, family="Times",colour="red")

#Use premade themes
library(gcookbook)
data("heightweight")
p<-ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point()
p+theme_grey()
p+theme_bw()
#(Create own theme)
p+theme_grey(base_size=16, base_family = "Times")
theme_set(theme_bw())
p

#Changing the Appearance of Theme Elements
p<-ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex))+geom_point()
#(Options for the plotting area)
p+theme(panel.grid.major = element_line(colour="red"),
        panel.grid.minor = element_line(colour="red", linetype = "dashed",size=0.2),
        panel.background = element_rect(fill = "lightblue"),
        panel.border = element_rect(colour="blue",fill=NA, size=2))
#(Options for the legend)
p+theme(legend.background = element_rect(fill="grey85",colour="red",size=1),
        legend.title = element_text(colour="blue",face="bold",size=14),
        legend.text = element_text(colour="red"),
        legend.key=element_rect(colour="blue",size=0.25))
#(Options for facets)
p+facet_grid(sex~.)+theme(
  strip.background = element_rect(fill="pink"),
  strip.text.y=element_text(size=14, angle=-90, face="bold")
)

#Hiding Grid Lines
library(gcookbook)
data("heightweight")
p<-ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point()
p+theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#(Hide vertical and horizontal grid lines)
p+theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())
p+theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
