#Install Packages
library(ggplot2)

#Import data
data("mtcars")

#Visulization
ggplot(data=mtcars, aes(x=wt, y=mpg))+geom_point()+labs(title = "Automobile Data",x="Weight",y="Mile Per Gallon")

ggplot(data=mtcars, aes(x=wt, y=mpg))+geom_point(pch=17, color='blue',size=2)+geom_smooth(method='lm',color='red',linetype=2)+labs(title='Automobile Data',x='Weight',y='Miles Per Gallon')

#Grouping and Faaceting
mtcars$am<-factor(mtcars$am, levels=c(0,1),labels=c("Automatic","Manual"))
mtcars$vs<-factor(mtcars$vs, levels=c(0,1),labels=c("v-Engine","Straight Engine"))
mtcars$cyl<-factor(mtcars$cyl)
ggplot(data=mtcars, aes(x=hp,y=mpg,shape=cyl, color=cyl))+geom_point(size=3)+facet_grid(mtcars$am~mtcars$vs)+labs(title='Automobile Data by Engine Type', x="Horsepower",y='Miles Per Gallon')

#Histogram
library(lattice)
data("singer")
ggplot(singer,aes(x=height))+geom_histogram()

#Boxplot
library(lattice)
data("singer")
ggplot(singer, aes(x=voice.part, y=height))+geom_boxplot()

#Notched box plots
library(car)
data("Salaries")
ggplot(Salaries, aes(x=rank, y=salary))+
  geom_boxplot(fill="cornflowerblue",color="black",notch=TRUE)+
  geom_point(position="jitter",color="blue",alpha=.5)+
  geom_rug(side="1",color="black")
##Jitter reduces overlap points 

#Combined Violin and Box plot
library(ggplot2)
library(lattice)
data("singer")
ggplot(singer, aes(x=voice.part,y=height))+
  geom_violin(fill="lightblue")+
  geom_boxplot(fill="lightgreen",width=.2)

#Grouping-Density
library(car)
library(ggplot2)
data("Salaries")
ggplot(data=Salaries, aes(x=salary,fill=rank))+
  geom_density(alpha=.3)

#Grouping-Dot
ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary, color=rank,shape=sex))+geom_point()

#Grouping-Bar
ggplot(Salaries, aes(x=rank, fill=sex))+
  geom_bar(position = "stack")+labs(title='Position="Stack')

ggplot(Salaries, aes(x=rank, fill=sex))+
  geom_bar(position = "dodge")+labs(title='Position="Dodge')

ggplot(Salaries, aes(x=rank, fill=sex))+
  geom_bar(position = "fill")+labs(title='Position="Fill')

#Faceting (Groups appear in side-by-side)
#Bar
library(lattice)
data("singer")
ggplot(data=singer, aes(x=height))+
  geom_histogram()+
  facet_wrap(~voice.part, nrow=4)

#Point
library(ggplot2)
ggplot(Salaries, aes(x=yrs.since.phd,y=salary,color=rank, shape=rank))+geom_point()+facet_grid(.~sex)

#Kernel-density plots
library(lattice)
data("singer")
ggplot(data=singer, aes(x=height, fill=voice.part))+
  geom_density()+
  facet_grid(voice.part~.)

#Adding smoothed line
library(car)
library(ggplot2)
data("Salaries")
ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary))+
  geom_smooth()+geom_point()

#Fit a quadratic polynomial regression separately by gender
ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary, linetype=sex, shape=sex, color=sex))+
  geom_smooth(method=lm, formula=y~poly(x,2),se=FALSE, size=1)+
  geom_point(size=2)

#Modify Axes
library(car)
data("Salaries")
ggplot(data=Salaries, aes(x=rank, y=salary, fill=sex))+
  geom_boxplot()+scale_x_discrete(breaks=c("AsstProf","AssocProf","Prof"),labels=c("Assistant\nProfessor","Associate\nProfessor","Full\nProfessor"))+
  scale_y_continuous(breaks=c(50000,100000,150000,200000),labels=c("50k","100k","150k","200k"))+
  labs(title="Faculty Salary by Rank and Sex",x="",y="")

#Modify Legends
library(car)
data("Salaries")
ggplot(data=Salaries, aes(x=rank, y=salary, fill=sex))+
  geom_boxplot()+
  scale_x_discrete(breaks=c("Assprof","AssocProf","Prof"),
                   labels=c("Assistant\nProfessor","Associate\nProfessor","Full\nProfessor"))+
  scale_y_continuous(breaks=c(50000,100000,150000,200000),
                     labels=c("50k","100k","150k","200k"))+
  labs(title="Faculty Salary by Rank and Gender",x="",y="",fill="Gender")+
  theme(legend.position = c(.1,.8))

#Scale (Control the size of the point based on values)
ggplot(mtcars, aes(x=wt,y=mpg,size=disp))+
  geom_point(shape=21,color="black",fill="cornsilk")+
  labs(x="Weight",y="Miles Per Gallon",title="Bubble Chart",size="Engine\nDisplacement")

library(car)
data("Salaries")
ggplot(data=Salaries, aes(x=yrs.since.phd,y=salary,color=rank))+
  scale_color_manual(values=c("orange","olivedrab","navy"))+
  geom_point(size=2)

ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary, color=rank))+
  scale_color_brewer(palette = "Set1")+geom_point(size=2)

#Themes (Control the overall appearance of these graphs)
library(car)
data("Salaries")
mytheme<-theme(plot.title=element_text(face="bold.italic",size="14",color="brown"),
               axis.title=element_text(face="bold.italic",size=10,color="brown"),
               axis.text=element_text(face="bold",size=9,color="darkblue"),
               panel.background = element_rect(fill="white",color="darkblue"),
               panel.grid.major.y=element_line(color="grey",linetype = 1),
               panel.grid.minor.y = element_line(color="grey",linetype=2),
               panel.grid.minor.x = element_blank(),
               legend.position = "top")
ggplot(Salaries, aes(x=rank, y=salary, fill=sex))+
  geom_boxplot()+
  labs(title="Salary by Rank and Sex", x="Rank",y="Salary")+
  mytheme

#Multiple graphs per page
library(car)
data("Salaries")
p1<-ggplot(data=Salaries, aes(x=rank))+geom_bar()
p2<-ggplot(data=Salaries,aes(x=sex))+geom_bar()
p3<-ggplot(data=Salaries,aes(x=yrs.since.phd, y=salary))+geom_point()
library(gridExtra)
grid.arrange(p1,p2,p3,ncol=3)
