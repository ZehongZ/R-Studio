#Import Libraries
library(ggplot2)
library(gcookbook)

#Basic Scatter Plot
data("heightweight")
heightweight[,c("ageYear","heightIn")]
ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point()

#Hollow circles
ggplot(heightweight,aes(x=ageYear, y=heightIn))+geom_point(shape=21)

#Change circle size
ggplot(heightweight,aes(x=ageYear, y=heightIn))+geom_point(size=1.5)

#Grouping Data Points by a Variable Using Shape or Color
heightweight[,c("sex","ageYear","heightIn")]
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex))+geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex))+geom_point()

#Map a vaiable to both shape and colour
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex,colour=sex))+geom_point()

#scale_shape_manual(), scale_colour_brewer() to change shapes or colors
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex))+geom_point()+scale_shape_manual(values=c(1,2))+scale_color_brewer(palette="Set1")

#Using Different Point Shapes
library(gcookbook)
ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point(shape=3)

#Use scale_shape_manual() to change the shapes
ggplot(heightweight,aes(x=ageYear, y=heightIn, shape=sex))+geom_point(size=3)+scale_shape_manual(values=c(1,4))

#Shape represents one variable and the fill represent another vairable
hw<-heightweight
hw$weightGroup<-cut(hw$weightLb, breaks=c(-Inf, 100, Inf),labels=c("<100",">=100"))
ggplot(hw,aes(x=ageYear, y=heightIn, shape=sex,fill=weightGroup))+geom_point(size=2.5)+scale_shape_manual(values=c(21,24))+scale_fill_manual(values=c(NA, 'black'),guide=guide_legend(override.aes = list(shape=21)))

#Mapping a Continuous Vairable to Color to Size
heightweight[,c('sex','ageYear','heightIn','weightLb')]
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb))+geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb))+geom_point()

#Set the fill gradient to go from black to white and make the points larger
ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear))+geom_point(shape=21,size=2.5)+scale_fill_gradient(low="black",high="white")

#Using guide_legend() will result in a discrete legend instead of a colorbar
ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear))+geom_point(shape=21,size=2.5)+scale_fill_gradient(low="black",high="white",breaks=12:17, guide=guide_legend())

#Make the area of the points proportional to the value
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, colour=sex))+geom_point(alpha=.5)+scale_size_area()+scale_colour_brewer(palette = "Set1")

#Dealing with Overplotting
#(Make the points semitrasparent, Bin the data into rectangles, Bin the data into hexagons, Use box plots)
data("diamonds")
sp<-ggplot(diamonds, aes(x=carat, y=price))
sp+geom_point()
sp+geom_point(alpha=.1)
sp+geom_point(alpha=.01)

#Bin the data into rectangles
sp+stat_bin2d()
sp+stat_bin2d(bins=50)+scale_fill_gradient(low="lightblue",high="red",limits=c(0,6000))

#Bin the data into hexagons
library(hexbin)
sp+stat_binhex()+scale_fill_gradient(low="lightblue",high="red",limits=c(0,8000))
sp+stat_binhex()+scale_fill_gradient(low="lightblue",high="red", breaks=c(0,250,500,1000,2000,4000,6000),limits=c(0,6000))

#Jitter the points
data("ChickWeight")
sp1<-ggplot(ChickWeight, aes(x=Time, y=weight))
sp1+geom_point()
sp1+geom_point(position="jitter")
sp1+geom_boxplot(aes(group=Time))

#Adding Fitted Regression Model Lines
library(gcookbook)
sp<-ggplot(heightweight, aes(x=ageYear, y=heightIn))
#(With confidence 0.99)
sp+geom_point()+stat_smooth(method=lm,level=0.99)
#(No confidence region)
sp+geom_point()+stat_smooth(method=lm, se=FALSE)
#(Change fit line color)
sp+geom_point(colour="grey60")+stat_smooth(method = lm,se=FALSE, colour="black")
#(Loess curve)
sp+geom_point(colour="grey60")+stat_smooth(method = loess)
#(Logistic Regression)
library(MASS)
data<-biopsy
data$classn[data$class=="benigh"]=0
data$classn[data$class=="malignant"]=1
data
ggplot(data, aes(x=V1, y=classn))+geom_point(position=position_jitter(width=0.3,height=0.06),alpha=0.4,shape=21,size=1.5)+stat_smooth(method=glm, family=binomial)
#(base line + loess line)
sps<-ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex))+geom_point()+scale_colour_brewer(palette="Set1")
sps+geom_smooth()
#(Lines extrapolate from the data)
sps+geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Adding Fitted Lines from an Existing Model
library(gcookbook)
model<-lm(heightIn~ageYear+I(ageYear^2), heightweight)
model
xmin<-min(heightweight$ageYear)
xmax<-max(heightweight$ageYear)
predicted<-data.frame(ageYear=seq(xmin, xmax, length.out = 100))
predicted$heightIn<-predict(model, predicted)
predicted
sp<-ggplot(heightweight, aes(x=ageYear, y=heightIn))+geom_point(colour="grey40")
sp+geom_line(data=predicted, size=1)

#Adding Marginal Rugs to a Scatter Plot
ggplot(faithful, aes(x=eruptions, y=waiting))+geom_point()+geom_rug()
ggplot(faithful, aes(x=eruptions, y=waiting))+geom_point()+geom_rug(position = "jitter",size=.2)

#Labeling Points in a Scatter Plot
library(gcookbook)
data("countries")                                                         
subset(countries, Year==2009&healthexp>2000)
sp<-ggplot(subset(countries, Year==2009&healthexp>2000),aes(x=healthexp, y=infmortality))+geom_point()
sp+annotate("text",x=4350,y=5.4,label="Canada")+annotate("text",x=7400,y=6.8,label="USA")
sp+geom_text(aes(label=Name),size=4)
#(Setting vjust=0 will make the baseline of the text on the same level)
sp+geom_text(aes(label=Name),size=4, vjust=0)
sp+geom_text(aes(y=infmortality+.1,label=Name),size=4,vjust=0)
#(To left-justify)
sp+geom_text(aes(label=Name),size=4, hjust=0)
sp+geom_text(aes(x=healthexp+100,label=Name),size=4,hjust=0)
#(Label just some points)
cdat<-subset(countries, Year==2009&healthexp>2000)
cdat$Name1<-cdat$Name
idx<-cdat$Name1%in%c("Canada","Ireland","United Kingdom","United States","New Zealand","Iceland","Japan","Luxembourg","Netherlands","Switzerland")
idx
cdat$Name1[!idx]<-NA
cdat
ggplot(cdat, aes(x=healthexp, y=infmortality))+geom_point()+geom_text(aes(x=healthexp+100, label=Name1),size=4, hjust=0)+xlim(2000,10000)

#Balloon Plot
library(gcookbook)
cdat<-subset(countries, Year==2009&Name %in% c("Canada","Ireland","United Kingdom","United States","New Zealand","Iceland","Japan","Luxembourg","Netherlands","Switzerland"))
cdat
p<-ggplot(cdat, aes(x=healthexp, y=infmortality,size=GDP))+geom_point(shape=21, colour="black",fill="cornsilk")
p
p+scale_size_area(max_size=15)
#(Categorical)
data("HairEyeColor")
hec<-HairEyeColor[,,'Male']+HairEyeColor[,,'Female']
library(reshape2)
hec<-melt(hec, value.name = "count")
ggplot(hec, aes(x=Eye, y=Hair))+geom_point(aes(size=count),shape=21,colour="black",fill="cornsilk")+scale_size_area(max_size=20, guide=FALSE)+geom_text(aes(y=as.numeric(Hair)-sqrt(count)/22,label=count),vjust=1,colour="grey60",size=4)
