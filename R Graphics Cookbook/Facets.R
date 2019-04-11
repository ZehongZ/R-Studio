#Import libraries
library(ggplot2)
library(gcookbook)

#Splitting Data into Subplots with Facets
data(mpg)
p<-ggplot(mpg, aes(x=displ,y=hwy))+geom_point()
p
p+facet_grid(drv~.)#Faceted by drv
p+facet_grid(drv~cyl)#Faceted by drv and cyl
#facet_wrap()
p+facet_wrap(class~.)
#Change dimensions of facet
p+facet_wrap(~class, nrow=2)
p+facet_wrap(~class, ncol=4)

#Using Facets with Different Axes
p<-ggplot(mpg, aes(x=displ, y=hwy))+geom_point()
p+facet_grid(drv~cyl, scales="free_y")
p
p+facet_grid(drv~cyl, scales="free")

#Changing the Text of Facet Labels
mpg2<-mpg
#(Rename variables)
levels(mpg2$drv)[levels(mpg2$drv)=="4"]<-"4wd"
levels(mpg2$drv)[levels(mpg2$drv)=="f"]<-"front"
levels(mpg2$drv)[levels(mpg2$drv)=="r"]<-"Rear"
#(Plot the new data)
ggplot(mpg2, aes(x=displ, y=hwy))+geom_point()+facet_grid(drv~.)
#(labeller function to set the labels)
ggplot(mpg2, aes(x=displ, y=hwy))+geom_point()+facet_grid(drv~., labeller=label_both)
#(label_parsed())
mpg3<-mpg
levels(mpg3$drv)[levels(mpg3$drv)=="4"]<-"4^(wd)"
levels(mpg3$drv)[levels(mpg3$drv)=="f"]<-".Front %.% e^(pi*i)"
levels(mpg3$drv)[levels(mpg3$drv)=="r"]<-"4^{wd}-Front"
ggplot(mpg3, aes(x=displ, y=hwy))+geom_point()+facet_grid(drv~., labeller=label_parsed)

#Changing the Appearance of Facet Labels and Headers
library(gcookbook)
data("cabbage_exp")
ggplot(cabbage_exp, aes(x=Cultivar, y=Weight))+geom_bar(stat = "identity")+facet_grid(.~Date)+theme(strip.text = element_text(face="bold", size=rel(1.5)),strip.background = element_rect(fill="lightblue",colour = "black",size=1) )
