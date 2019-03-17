#Scatter plots
attach(mtcars)
plot(wt, mpg, main="Basic Scatter plot of MPG vs. Weight", xlab="Car Weight (lbs/1000)", ylab = "Miles Per Gallon", pch=19)
abline(lm(mpg~wt), col="red", lwd=2, lty=1)
lines(lowess(wt, mpg), col="blue", lwd=2, lty=2)
#lowess() add a smoothed line. This smoothed line is a nonparametric fit line based on locally weighted polynomial regression
detach()

#Complex version of the Scatter plot
library(car)
scatterplot(mpg~wt | cyl, data=mtcars, lwd=2, span=0.75, #(Controls amount of smoothing)
            main="Scatter Plot of MPG vs. Weight by # Cylinders",
            xlab = "Weight of Car (lbs/1000)",
            ylab="Miles Per Gallon",
            legend.plot=TRUE,
            id.method="identify",
            boxplots="xy")

#Scatter-plot matrices
pairs(~mpg+disp+drat+wt, data=mtcars, main="Basic Scatter Plot Matrix")

#Complex version of the Scatter Plot
library(car)
scatterplotMatrix(~mpg+disp+drat+wt, data=mtcars, spread=FALSE, #Spread shows spread and asymmetry
                  smoother.args=list(lty=2), main="Scatter Plot Matrix via Car Package")

#High-sensity scatter plots
set.seed(1234)
n<-10000
c1<-matrix(rnorm(n, mean=0, sd=.5), ncol=2)
c2<-matrix(rnorm(n, mean=3, sd=2), ncol=2)
mydata<-rbind(c1,c2)
mydata<-as.data.frame(mydata)
names(mydata)<-c("x","y")
with(mydata,
     plot(x,y, pch=19, main="Scatter Plot with 10,000 Observations"))

#High-sensity smooth scatter plots
set.seed(1234)
n<-10000
c1<-matrix(rnorm(n, mean=0, sd=.5), ncol=2)
c2<-matrix(rnorm(n, mean=3, sd=2), ncol=2)
mydata<-rbind(c1,c2)
mydata<-as.data.frame(mydata)
names(mydata)<-c("x","y")
library(hexbin)
with(mydata,{
  bin<-hexbin(x,y, xbins=50)
  plot(bin, main="Hexagonal Binning with 10,000 Observations")
})

#3D Scatter plots
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg, main="Basic 3D Scatter Plot")
detach()

library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg, pch=16, highlight.3d=TRUE, type="h", main="3D Scatter Plot with Verticla Lines")

#3D Scatter plots with a regression plane
library(scatterplot3d)
attach(mtcars)
s3d<-scatterplot3d(wt, disp, mpg, pch=16, highlight.3d = TRUE, type="h", main="3D Scatter Plot with Vertical Lines and Regression Plane")
fit<-lm(mpg~wt+disp)
s3d$plane3d(fit)

#Spinning 3D scatter plots
install.packages("devtools")
library(devtools)
install.packages("rgl")
library(rgl)
attach(mtcars)
plot3d(wt, disp, mpg, col="red", size=5)

library(car)
with(mtcars,
     scatter3d(wt, disp,mpg))

#Bubble plots
attach(mtcars)
r<-sqrt(disp/pi)
symbols(wt, mpg, circle=r, inches=0.3, fg="white",bg="lightblue", 
        main="Bubble Plot with point size proportional to displacement",
        ylab="Miles Per Gallon",
        xlab="Weight of Car (lbs/1000)")
text(wt, mpg, rownames(mtcars), cex=0.6)
detach(mtcars)

#Line Charts
opar<-par(no.readonly = TRUE)
par(mfrow=c(1,2))
t1<-subset(Orange, Tree==1)
plot(t1$age, t1$circumference, xlab="Age", ylab="Circumference", main="Orange Tree 1 Growth")
plot(t1$age, t1$circumference, xlab="Age", ylab="Circumference", main="Orange Tree 1 Growth", type="b")
par(opar)

#Line chart displaying the growth of five orange trees over time
Orange$Tree<-as.numeric(Orange$Tree)
ntrees<-max(Orange$Tree)
#Set up the plot
xrange<-range(Orange$age)
yrange<-range(Orange$circumference)
plot(xrange, yrange, type="n", xlab="Age", ylab = "Circumference")
colors<-rainbow(ntrees)
linetype<-c(1:ntrees)
plotchar<-seq(18, 18+ntrees, 1)
#Adds lines
for (i in 1:ntrees){
  tree<-subset(Orange, Tree==i)
  lines(tree$age, tree$circumference, 
        type="b",
        lwd=2,
        lty=linetype[i],
        col=colors[i],
        pcy=plotchar[i])
}
title("Tree Growth", "Example of line plot")
#Adds a legend
legend(xrange[1], yrange[2],
       1:ntrees, 
       cex=0.8,
       col=colors,
       pch=plotchar,
       lty=linetype,
       title="Tree")

#Corrgrams
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie,
         text.panel = panel.txt,
         main="Corrgram of mtcars intercorrelations")

#Corrgrams with smoothed fit lines and confidence ellipses
library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse, upper.panel = panel.pts, text.panel=panel.txt, diag.panel = panel.minmax,
         main="Corrgram of mtcars data using scatter plots and ellipses")

#Corrgrams with shading in the lower triangle
library(corrgram)
corrgram(mtcars, lower.panel = panel.shade,
         upper.panel = panel.cor, text.panel = panel.txt,
         main="Car Mileage Data")

library(corrgram)
cols<-colorRampPalette(c("darkgoldenrod4", "burlywood1","darkkhaki", "darkgreen"))
corrgram(mtcars, order=TRUE, col.regions = cols, 
         lower.panel = panel.shade,
         upper.panel = panel.cor, text.panel = panel.txt,
         main="A Corrgram (or Horse) of a Different Color")


#Mosaic plots (for categorical variables)
library(grid)
library(vcd)
mosaic(Titanic, shade=TRUE, legend=TRUE)

library(vcd)
mosaic(~Class+Sex+Age+Survived, data=Titanic, shade=TRUE, legend=TRUE)
