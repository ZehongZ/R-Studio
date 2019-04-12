#Import libraries
library(ggplot2)
library(gcookbook)

#Making a Corrleation Matrix
data("mtcars")
mcor<-cor(mtcars)#Generate the numerical correlation matrix
round(mcor, digits = 2)
library(corrplot)
corrplot(mcor)
#(Generate a lighter palette)
col<-colorRampPalette(c("#BB4444","#EE9988","#FFFFFF","#77AADD","#4477AA"))
corrplot(mcor, method="shade",shade.col = NA, tl.col = "black",tl.srt = 45,col=col(200),addCoef.col = "black",addcolorlabel="no",order="AOE")

#Shading a Subregion Under a Function Curve
dnorm_limit<-function(x){
  y<-dnorm(x)
  y[x<0|x>2]<-NA
  return(y)
}
#(ggplot() with dummy data)
p<-ggplot(data.frame(x=c(-3,3)),aes(x=x))
p+stat_function(fun=dnorm_limit,geom="area",fill="blue",alpha=0.2)+stat_function(fun=dnorm)

#Creating a Newwork Graph
library(igraph)
#(Specify edges for a directed graph)
gd<-graph(c(1,2,2,3,2,4,1,4,5,5,3,6))
plot(gd)
#(For an undirected graph)
gu<-graph(c(1,2,2,3,2,4,1,4,5,5,3,6),directed = FALSE)
plot(gu,vertex.label=NA)

#In a network graph, the position of the nodes is unspecified by the data, and they're placed randomly. 
set.seed(229)
plot(gu)

library(gcookbook)
madmen2
head(madmen2)
g<-graph.data.frame(madmen2, directed = TRUE)
par(mar=c(0,0,0,0))#Remove unnecessary margins
plot(g, layout=layout.fruchterman.reingold, vertext.size=8, edge.arrow.size=0.5, vertext.label=NA)
#(Make a directed graph)
g<-graph.data.frame(madmen, directed = FALSE)
par(mar=c(0,0,0,0))
plot(g, layout=layout.circle, vertex.size=8, vertext.label=NA)

#Using Text Labels in a Network Graph
library(igraph)
library(gcookbook)
m<-madmen[1:nrow(madmen)%%2==1,]
g<-graph.data.frame(m, directed = FALSE)
plot(g, layout=layout.fruchterman.reingold,
     vertex.size=4,#Nodes
     vertex.label=V(g)$name, #Set the labels
     vertex.label.cex=0.8, #Font
     vertext.label.dist=0.4,#Offset the labels
     vertext.label.color="black")
#(Setting the edges)
E(g)
E(g)[c(2,11,19)]$label<-"M"#Set some of the labels to "M"
E(g)$color<-"grey70"
E(g)[c(2,11,19)]$color<-"red"
plot(g)

#Creating a Heat Map
data("presidents")
str(presidents)
head(presidents)

pres_rating<-data.frame(rating=as.numeric(presidents),
                        year=as.numeric(floor(time(presidents))),
                        quarter=as.numeric(cycle(presidents)))
pres_rating
p<-ggplot(pres_rating, aes(x=year, y=quarter, fill=rating))
p+geom_tile()
p+geom_raster()
#(Customize the appearance of the heat map)
p+geom_tile()+scale_x_continuous(breaks=seq(1940,1976, by=4))+scale_y_reverse()+scale_fill_gradient2(midpoint=50, mid="grey70",limits=c(0,100))

#Creating a Three-Dimensional Scatter Plot
library(rgl)
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg,type="s",size=0.75, lit=FALSE)
#(Add vertical segments)
interleave<-function(v1, v2) as.vector(rbind(v1,v2))
plot3d(mtcars$wt, mtcars$disp,mtcars$mpg,xlab = "Weight",ylab="Displacement",zlab="MPG",size=.75, type="s",lit=FALSE)
#(Add the segments)
segments3d(interleave(mtcars$wt,mtcars$wt),
           interleave(mtcars$disp,mtcars$disp),
           interleave(mtcars$mpg,mtcars$mpg),
           alpha=0.4, col="blue")
#(Tweak the appearance of the background and the axes)
plot3d(mtcars$wt,mtcars$disp,mtcars$mpg,
       xlab = "",ylab = "",zlab = "",
       axes=FALSE,
       size = .75, type="s",lit=FALSE)
segments3d(interleave(mtcars$wt,mtcars$wt),
           interleave(mtcars$disp,mtcars$disp),
           interleave(mtcars$mpg,mtcars$mpg),
           alpha=0.4, col="blue")
#(Draw the box)
rgl.bbox(color='grey50',emission="grey50",xlen=0,ylen=0,zlen = 0)
#(Set defautl color of future objects to black)
rgl.material(color="black")
#(Add axes to specific sides)
axes3d(edges=c("x--","y+-","z--"),
       nticks = 6,
       cex=.75)
#(Add axis labels.)
mtext3d("Weight", edge="x==",line=2)
mtext3d("Displacement",edge="y+-",line=3)
mtext3d("MPG", edge="z--",line=3)

#Adding a Prediction Surface to a Three-Dimensional Plot
predictgrid<-function(model, xvar,yvar, zvar, res=16, type=NULL){
  xrange<-range(model$model[[xvar]])
  yrange<-range(model$model[[yvar]])
  newdata<-expand.grid(x=seq(xrange[1],xrange[2],length.out = res),
                       y=seq(yrange[1],yrange[2],length.out = res))
  names(newdata)<-c(xvar,yvar)
  newdata[[zvar]]<-predict(model, newdata=newdata, type=type)
  newdata
}
df2mat<-function(p, xvar=NULL, yvar=NULL, zvar=NULL){
  if(is.null(xvar))xvar<-names(p)[1]
  if(is.null(yvar))yvar<-names(p)[2]
  if(is.null(zvar))zvar<-names(p)[3]
  x<-unique(p[[xvar]])
  y<-unique(p[[yvar]])
  z<-matrix(p[[zvar]],nrow=length(y),ncol=length(x))
  m<-list(x,y,z)
  names(m)<-c(xvar,yvar,zvar)
  m
}
interleave<-function(v1,v2) as.vector(rbind(v1,v2))
library(rgl)
m<-mtcars
mod<-lm(mpg~wt+disp+wt:disp, data=m)
m$pred_mpg<-predict(mod)
mpgrid_df<-predictgrid(mod,"wt","disp","mpg")
mpgrid_list<-df2mat(mpgrid_df)
plot3d(m$wt,m$disp,m$mpg,type="s",size=0.5, lit=FALSE)
spheres3d(m$wt,m$disp,m$pred_mpg,alpha=0.4,type="s",size=0.5,lit=FALSE)
segments3d(interleave(m$wt,m$wt),
           interleave(m$disp,m$disp),
           interleave(m$mpg,m$pred_mpg),alpha=0.4,col="red")
surface3d(mpgrid_list$wt,mpgrid_list$disp,mpgrid_list$mpg,alpha=0.4, front="lines",back="lines")
plot3d(mtcars$wt,mtcars$disp,mtcars$mpg,
       xlab="",ylab="",zlab = "",
       axes=FALSE,
       size=.5, type="s",lit=FALSE)
segments3d(interleave(m$wt,m$wt),
           interleave(m$disp,m$disp),
           interleave(m$mpg,m$mpg),
           alpha=0.4,col="red")
surface3d(mpgrid_list$wt,mpgrid_list$disp,mpgrid_list$mpg,alpha=0.4,front="lines",back="lines")
rgl.bbox(color="grey50",
         emission="grey50",
         xlen=0,ylen=0,zlen=0)
rgl.material(color="black")
axes3d(edges=c("x--","y+-","z--"),
       nticks = 6,
       cex=.75)
mtext3d("Weight", edge="x--",line=2)
mtext3d("Displacement",edge="y+-",line=3)
mtext3d("MPG",edge="z--",line=3)

#Creating a Dendrogram
library(gcookbook)
c2<-subset(countries,Year==2009)
c2<-c2[complete.cases(c2),]
set.seed(201)
c2<-c2[sample(1:nrow(c2),25),]
c2
rownames(c2)<-c2$Name
c2<-c2[,4:7]
c2
c3<-scale(c2)
c3
hc<-hclust(dist(c3))
plot(hc)
plot(hc,hang=-1)

#Creating a Vector Field
data("isabel")
islice<-subset(isabel,z==min(z))
ggplot(islice, aes(x=x,y=y))+geom_segment(aes(xend=x+vx/50,yend=y+vy/50),size=0.25)

islice<-subset(isabel,z==min(z))
every_n<-function(x,by=2){
  x<-sort(x)
  x[seq(1,length(x),by=by)]
}
keepx<-every_n(unique(isabel$x),by=4)
keepy<-every_n(unique(isabel$y),by=4)
islicesub<-subset(islice, x %in% keepx & y %in% keepy)
library(grid)
ggplot(islicesub,aes(x=x,y=y))+geom_segment(aes(xend=x+vx/50,yend=y+vy/50),arrow=arrow(length=unit(0.1,"cm")),size=0.25)

#Creating a Graph of an Empirical Cumulative Distribution Function
library(gcookbook)
ggplot(heightweight, aes(x=heightIn))+stat_ecdf()

#Creating a Mosaic Plot
data("UCBAdmissions")
ftable(UCBAdmissions)
dimnames(UCBAdmissions)
library(vcd)
mosaic(~Admit+Gender+Dept, data=UCBAdmissions)
mosaic(~Dept+Gender+Admit, data=UCBAdmissions,highlighting = "Admit",highlighting_fill=c("lightblue","pink"),direction=c("v","h","v"))

#Creating a Pie Chart
library(MASS)
data("survey")
fold<-table(survey$Fold)
fold
pie(fold)
pie(c(99,18,120),labels = c("L or R","Neight","R on L"))

#Creating a Map
library(maps)

