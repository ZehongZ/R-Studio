##Build first SOM

#Load package
library(tidyverse)
library(GGally)

#Loading and exploring the flea dataset
data(flea)
fleaTib<-as_tibble(flea)
fleaTib

#Loading the kohonen package and creating a SOM grid
library(kohonen)
somGrid<-somgrid(xdim=5, ydim=5, topo = "hexagonal",
                 neighbourhood.fct = "bubble", toroidal = FALSE)

#Training the SOM
fleaScaled<-fleaTib%>%
  select(-species)%>%
  scale()
fleaSom<-som(fleaScaled, grid=somGrid, rlen=5000,
             alpha=c(0.05,0.01))

#Plotting SOM dianostics
par(mfrow=c(2,3))
plotTypes<-c("codes","changes","count","quality",
             "dist.neighbours","mapping")
walk(plotTypes, ~plot(fleaSom, type=., shape="straight"))

#Plot heatmaps for each variable
getCodes(fleaSom)%>%
  as_tibble()%>%
  iwalk(~plot(fleaSom, type = "property", property = .,
              main=.y, shape="straight"))

#Plotting flea species onto the SOM
nodeCols<-c("cyan3", "yellow", "purple")
plot(fleaSom, type="mapping", pch=21,
     bg=nodeCols[as.numeric(fleaTib$species)],
                 shape="straight", bgcol="lightgrey")

#Plotting flea species onto the SOM
newData<-tibble(tars1=c(120,200),
                tars2=c(125,120),
                head=c(52,48),
                aede1=c(140,128),
                aede2=c(12,14),
                aede3=c(100,85))%>%
  scale(center=attr(fleaScaled, "scaled:center"),
        scale=attr(fleaScaled, "scaled:scale"))
predicted<-predict(fleaSom, newData)
plot(fleaSom, type="mapping", classif=predicted, shape="round")


##LLE
#Loading package
library(lle)
library(tidyverse)

#Load data
data("lle_scurve_data")
colnames(lle_scurve_data)<-c("x","y","z")
head(lle_scurve_data)
sTib<-as_tibble(lle_scurve_data)
sTib

#Plotting the s curve dataset in three dimensions
library(plot3D)
scatter3D(x=sTib$x, y=sTib$y, z=sTib$z, pch=19, 
          bty="b2", colkey=FALSE, theta=35, phi=10,
          col=ramp.col(c("darkred","lightblue")))

#Calculating K and performaing the LLE
llek<-calc_k(lle_scurve_data, m=2, kmin=1, kmax=20,
             parallel = TRUE, cpus=parallel::detectCores())
lleBestK<-filter(llek, rho==min(llek$rho))
lleBestK
lleCurve<-lle(lle_scurve_data, m=2, k=lleBestK$k)

#Plotting the LLE
sTib<-sTib%>%
  mutate(LLE1=lleCurve$Y[,1],
         LLE2=lleCurve$Y[,2])
ggplot(sTib, aes(LLE1, LLE2, sol=z))+
  geom_point()+
  scale_color_gradient(low="darkred", high="lightblue")+
  theme_bw()

#Performing and plotting LLE on the flea dataset
lleFleaK<-calc_k(fleaScaled, m=2, kmin=1, kmax=20,
                 parallel=TRUE, cpus=parallel::detectCores())
lleBestFleaK<-filter(lleFleaK, rho==min(lleFleaK$rho))
lleBestFleaK
lleFlea<-lle(fleaScaled, m=2, k=lleBestFleaK$k)
fleaTib<-fleaTib%>%
  mutate(LLE1=lleFlea$Y[,1],
         LLE2=lleFlea$Y[,2])
ggplot(fleaTib, aes(LLE1, LLE2, col=species))+
  geom_point()+
  theme_bw()
