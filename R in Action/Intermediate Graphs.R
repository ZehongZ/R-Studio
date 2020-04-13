#A scatter plot with best-fit lines
attach(mtcars)
plot(wt, mpg,
     main="Basic Scatter Plot of MPG vs. Weight",
     xlab = "Car Weight (lbs/1000)",
     ylab="Miles Per Gallon", pch=19)
abline(lm(mpg~wt), col="red", lwd=2, lty=1)
lines(lowess(wt, mpg), col="blue", lwd=2, lty=2)#Add a smooth line
#Complex version
library(car)
scatterplot(mpg~wt | cyl, data=mtcars, lwd=2, span=0.75,
            main="Scatter Plot of MPG vs. Weight by # Cylinders",
            xlab="Weight of Car",
            ylab="Miles Per Gallon",
            legend.plot=TRUE,
            id.method="identify",
            labels=row.names(mtcars),
            boxplots="xy"
)

#Scatter-plot matrices
pairs(~mpg+disp+drat+wt, data=mtcars,
      main="Basic Scatter Plot Matrix")
#2
scatterplotMatrix(~mpg+disp+drat+wt, data=mtcars,
                  spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix via car package")

#High-density scatter plots
set.seed(1234)
n<-10000
c1<-matrix(rnorm(n, mean=0, sd=.5),ncol=2)
c2<-matrix(rnorm(n, mean=3, sd=2),ncol=2)
mydata<-rbind(c1, c2)
mydata<-as.data.frame(mydata)
names(mydata)<-c("x","y")
with(mydata,
     plot(x,y, pch=19, main="Scatter Plot with 10,000 Observations"))
#Change color
with(mydata,
     smoothScatter(x,y, main="Scatter Plot Colored by Smoothed Densities"))
#2
library(hexbin)
with(mydata, {
  bin<-hexbin(x,y,xbins=50)
  plot(bin,main="Hexagonal Binning with 10,000 Observations")
})

#3D scatter plots
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg,
              main="Basic 3D Scatter Plot")
detach(mtcars)
#More options
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg,pch=16,
              highlight.3d = TRUE,
              type="h",
              main="3D Scatter Plot with Vertical lines")
#Add regression plane
library(scatterplot3d)
attach(mtcars)
s3d<-scatterplot3d(wt, disp, mpg, pch=16, highlight.3d = TRUE,
                   type='h',
                   main="3D Scatter Plot with Vertical Lines and regression Plane")
fit<-lm(mpg~wt+disp)
s3d$plane3d(fit)

#Bubble Plots
attach(mtcars)
r<-sqrt(disp/pi)
symbols(wt, mpg, circle=r, inches=0.3,
        fg="white", bg="lightblue",
        main="Bubble Plot with point size proportional to displacement",
        ylab="Miles Per Gallon",
        xlab="Weight of Car")
text(wt, mpg, rownames(mtcars), cex=0.6)
detach(mtcars)

#Line Charts
opar<-par(no.readonly = TRUE)
par(mfrow=c(1,2))
t1<-subset(Orange, Tree==1)
plot(t1$age, t1$circumference,
     xlab="Age (days)",
     ylab="Circumference",
     main="Orange Tree 1 Growth")
plot(t1$age, t1$circumference,
     xlab="Age (days)",
     ylab="Circumference(mm)",
     main="Orange Tree 1 Growth",
     type="b")
par(opar)
#Line chart displaying the growth of five orange trees over time
Orange$Tree<-as.numeric(Orange$Tree)
ntrees<-max(Orange$Tree)
xrange<-range(Orange$Tree)
yrange<-range(Orange$circumference)
plot(xrange, yrange,
     type="n",
     xlab="Age (days)",
     ylab="Circumference (mm)"
)
colors<-rainbow(ntrees)
linetype<-c(1:ntrees)
plotchar<-seq(18,18+ntrees, 1)
for (i in 1:ntrees) {
  tree<-subset(Orange, Tree==i)
  lines(tree$age, tree$circumference,
        type = "b",
        lwd=2,
        lty=linetype[i],
        col=colors[i],
        pch=plotchar[i])
}
title("Tree Growth", "Example of line plot")
legend(xrange[1],yrange[2],
       1:ntrees,
       cex=0.8,
       col=colors,
       pch=plotchar,
       lty=linetype,
       title="Tree")

#Corrgrams
options(digits=2)
cor(mtcars)
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel=panel.txt,
                                 main="Corrgram of mtcars intercorrelations")
#Corrgrams2
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse,
         upper.panel = panel.pts, text.panel = panel.txt,
         diag.panel = panel.minmax,
         main="Corrgram of mtcars data using scatter plots and ellipses")
#Corrgram3
library(corrgram)
corrgram(mtcars, lower.panel = panel.shade,
         upper.panel = NULL, text.panel = panel.txt,
         main="Car Mileage Data")
#Corrgram4
library(corrgram)
cols<-colorRampPalette(c("darkgoldenrod4", "burlywood1",
                         "darkkhaki","darkgreen"))
corrgram(mtcars, order=TRUE, col.regions = cols,
         lower.panel=panel.shade,
         upper.panel = panel.conf,
         text.panel = panel.txt,
         main="A Corrgram of a Different Color")

#Mosaic Plots
ftable(Titanic)
library(vcd)
mosaic(~Class+Sex+Age+Survived, data=Titanic, shade=TRUE, legend=TRUE)
