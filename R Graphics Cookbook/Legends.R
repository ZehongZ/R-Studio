#Import libraries
library(ggplot2)
library(gcookbook)

#Remove the legend from a graph
p<-ggplot(PlantGrowth, aes(x=group, y=weight, fill=group))+geom_boxplot()
p+guides(fill=FALSE)
p+scale_fill_discrete(guide=FALSE)
p+theme(legend.position = "none")

#Changing the position of a legend
p<-ggplot(PlantGrowth, aes(x=group, y=weight, fill=group))+geom_boxplot()+scale_fill_brewer(palette="Pastel2")
p+theme(legend.position = "top")
#(Top-right corner)
p+theme(legend.position = c(1,1),legend.justification = c(1,1))
#(Add an opaque border)
p+theme(legend.position = c(.85,.2))+theme(legend.background = element_rect(fill="white",colour="black"))
#Remove the border around elements
p+theme(legend.position = c(.85,.2))+theme(legend.background = element_blank())+theme(legend.key = element_blank())

#Changing the Order of Items in a Legend
p<-ggplot(PlantGrowth, aes(x=group, y=weight, fill=group))+geom_boxplot()
p+scale_fill_discrete(limits=c("trt1","trt2","ctrl"))

p+scale_fill_grey(start=.5, end=.1, limits=c("trt1","trt2","ctrl"))
p+scale_fill_brewer(palette = "Pastel2",limits=c("trt1","trt2","crtl"))

#Reversing the Order of Items in a Legend
p<-ggplot(PlantGrowth, aes(x=group, y=weight, fill=group))+geom_boxplot()
#(Reverse the legend order)
p+guides(fill=guide_legend(reverse = TRUE))
#(Specifying the scale)
scale_fill_hue(guide=guide_legend(reverse = TRUE))

#Changing a Legend Titile
p<-ggplot(PlantGrowth, aes(x=group, y=weight, fill=group))+geom_boxplot()
p+labs(fill="Condition")
p+scale_fill_discrete(name="Condition")
#(Set the title of each individually)
hw<-ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex))+geom_point(aes(size=weightLb))+scale_size_continuous(range=c(1,4))
hw+labs(colour="Male/Female",size="Weight\n(pounds")

hw1<-ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex))+geom_point()
hw1+labs(shape="Male/Female")
hw1+labs(shape="Male/Female", colour="Male/Female")
p+guides(fill=guide_legend(title="Condition"))
#(Specify the legend title's appearance)
p+guides(fill=guide_legend(title.theme = element_text(face="italic",family="times",colour="red",size=14)))

#Removing a Legend Title
ggplot(PlantGrowth, aes(x=group, y=weight, fill=group))+geom_boxplot()+guides(fill=guide_legend(title=NULL))
scale_fill_hue(guide=guide_legend(title = NULL))

#Changing the Labels in a Legend
library(gcookbook)
p<-ggplot(PlantGrowth, aes(x=group, y=weight, fill=group))+geom_boxplot()
p+scale_fill_discrete(labels=c("Control","Treatment 1","Treatment 2"))
p+scale_fill_grey(start=.5, end=1, labels=c("Control","Treatment 1","Treatment 2"))
p+scale_fill_discrete(limits=c("trt1","trt2","ctrl"),labels=c("Treatment 1","Treatment 2","Control"))
#(Single legend that combines two separate aesthetics)
p<-ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex))+geom_point()
p+scale_shape_discrete(labels=c("Female","Male"))
p+scale_shape_discrete(labels=c("Female","Male"))+scale_colour_discrete(labels=c("Female","Male"))

#Changing the Appearance of Legend Labels
p<-ggplot(PlantGrowth, aes(x=group, y=weight, fill=group))+geom_boxplot()
p+theme(legend.text = element_text(face="italic",family="Times",colour="red",size=14))
p+guides(fill=guide_legend(label.theme = element_text(face='italic',family="Times",colour="red",size=14)))

#Using Labels with Multiple Lines of Text
p<-ggplot(PlantGrowth, aes(x=group, y=weight, fill=group))+geom_boxplot()
p+scale_fill_discrete(labels=c("Control","Type 1\ntreatment","Type2\ntreatment"))
