#5.1.1 
#Segment Data Definition
segVars<-c("age","gender","income","kids","ownHome","subscribe")
segVarType<-c("norm","binom","norm","pois","binom","binom")
segNames<-c("Suburb mix","Urban hip","Travelers","Moving up")
segSize<-c(100, 50, 80, 70)
segMeans<-matrix(c(
  40,0.5,55000,2,0.5,0.1,
  24,0.7,21000,1,0.2,0.2,
  58,0.5,64000,0,0.7,0.05,
  36,0.3,52000,2,0.3,0.2
),ncol=length(segVars),byrow = TRUE)
#Define standard deviation matrix
segSDs<-matrix(c(
  5, NA, 12000, NA, NA, NA,
  2, NA, 5000, NA, NA, NA,
  8, NA, 21000, NA, NA, NA,
  4, NA, 10000, NA, NA, NA
), ncol=length(segVars), byrow = TRUE)
#for() Loop
for (i in 1:10) {print(i)}
#Loop for real numbers
i.seq<-rep(sqrt(seq(from=2.1, to=6.2, by=1.7)),3)
for (i in i.seq){print(i)}
#Loop for character elements
for (i in c("Hello", "world,","welcome to R!")) {cat(i)}
#Loop for position index
for (i in seq_along(i.seq)){cat(i,"=", i.seq[i],"\n")}
#if() Blocks
x<-1:5
if (x>1){
  print("Hi")
} else{
  print("bye")
}
ifelse(x>1, "hi","bye")
fn.hi<-function(){"hi"}
fn.bye<-function(){"bye"}
ifelse(x>1, fn.hi(),fn.bye())

#5.1.4
#Final Segment Data Generation
seg.df<-NULL
set.seed(02554)
#Iterate over segments and create data for each
for (i in seq_along(segNames)){
  cat(i, segNames[i], "\n")
  #Empty matrix to hold this particular segment's data
  this.seg<-data.frame(matrix(NA, nrow=segSize[i],ncol=length(segVars)))
  #Within segment, iterate over variables and draw appropriate random data
  for (j in seq_along(segVars)){
    if (segVarType[j]=="norm"){
      this.seg[,j]<-rnorm(segSize[i],mean=segMeans[i,j],sd=segSDs[i,j])
    }else if (segVarType[j]=="pois"){
        this.seg[,j]<-rpois(segSize[i], lambda = segMeans[i,j])
    }else if(segVarType[j]=="binom"){
      this.seg[,j]<-rbinom(segSize[i], size=1, prob=segMeans[i,j])
    }else{
      stop("Bad segment data type: ", segVarType[j])
    }
  }
  seg.df<-rbind(seg.df, this.seg)
    }
#Make the data frame names match what we defined
names(seg.df)<-segVars
#Add segment membership for each row
seg.df$Segment<-factor(rep(segNames, times=segSize))
#Convert the binomial variables to nicely labeled factors
seg.df$ownHome<-factor(seg.df$ownHome, labels = c("ownNo","ownYes"))
seg.df$gender<-factor(seg.df$gender, labels=c("Female","Male"))
seg.df$subscribe<-factor(seg.df$subscribe, labels=c("subNo","subYes"))
summary(seg.df)
#Save the file
save(seg.df, file="segdf-Rintro-Ch5.RData")

#5.2
#Finding Descriptives by Group
mean(seg.df$income[seg.df$Segment=="Moving up"])
mean(seg.df$income[seg.df$Segment=="Moving up"]&seg.df$subscribe=="subNo")
by(seg.df$income, seg.df$Segment, mean)
by(seg.df$income, list(seg.df$income, seg.df$subscribe),mean)
aggregate(seg.df$income, list(seg.df$Segment),mean)
seg.income.mean<-aggregate(seg.df$income, list(seg.df$Segment),mean)
seg.df$segIncome<-seg.income.mean[seg.df$Segment,2]
seg.df$Segment
seg.income.mean[seg.df$Segment,]
seg.income.mean[seg.df$Segment,2]
seg.df$segIncome<-NULL

#5.2.1
#Language Brief: Basic Formula Syntax
aggregate(income~Segment, data=seg.df, mean)

#5.2.2
#Descriptives for Two-Way Groups
aggregate(income~Segment+ownHome, data=seg.df, mean)
#Extend to include many grouping variable as needed
aggregate(income~Segment+ownHome+subscribe, data=seg.df, mean)
table(seg.df$Segment,seg.df$ownHome)
with(seg.df, table(Segment, ownHome))
with(seg.df, table(kids, Segment))
xtabs(kids~Segment, data=seg.df)
aggregate(kids~Segment, data=seg.df, sum)
#Multiply the frequency table by marginal number of kids and add it up
seg.tab<-with(seg.df, table(kids, Segment))
apply(seg.tab*0:7,2, sum)
colSums(seg.tab*0:7)

#5.2.3 
#Visualizationo by Group: Frequencies and Proportions
library(lattice)
histogram(~subscribe | Segment, data=seg.df)#Proportional
histogram(~subscribe | Segment, data=seg.df, type="count",
          layout=c(4,1), col=c("burlywood", "darkolivegreen"))
#Proportion of subscribers within each segment by home ownership
histogram(~subscribe | Segment + ownHome, data=seg.df)
#Proportion of subscribers within each segment
prop.table(table(seg.df$subscribe, seg.df$Segment),margin = 2)
#Plot just the "yes"
barchart(prop.table(table(seg.df$subscribe, seg.df$Segment), margin = 2)[2,],
         xlab="Subscriber Proportion by Segment", col="darkolivegreen")

#5.2.4
#Visualization by Group, Continuous Data
seg.mean<-aggregate(income~Segment, data=seg.df, mean)
library(lattice)
barchart(income~Segment, data=seg.mean, col="grey")
#Further split by home ownership
seg.income.agg<-aggregate(income~Segment+ownHome, data = seg.df, mean)
barchart(income~Segment, data=seg.income.agg,
         groups=ownHome, auto.key=TRUE,
         par.settings=simpleTheme(col=terrain.colors(2)))
#Boxplot by factor
boxplot(income~Segment, data=seg.df, yaxt="n", ylab="Income")
ax.seq<-seq(from=0, to=120000, by=20000)
axis(side=2, at=ax.seq, labels=paste(ax.seq/1000, "k", sep=""), las=1)
#Horizontal box-and-whiskers
bwplot(Segment~income, data=seg.df, horizontal = TRUE, xlab="Income")
bwplot(Segment~income | ownHome, data=seg.df, horizontal = TRUE, xlab="Income")
