library(tidyverse)
myTib<-tibble(x=1:4, y=c("londong","beijing","las vegas","berlin"))
myTib

#Converting data frames to tibbles
myDf<-data.frame(x=1:4, 
                 y=c("london","beijing","las vegas","berlin"))
dfToTib<-as.tibble(myDf)
dfToTib

#Tibbles don't convert strings to factors
myDf<-data.frame(x=1:4,
                 y=c("london","beijing","las vegas","berlin"))
myDfNotFactor<-data.frame(x=1:4,
                          y=c("london","beijing","las vegas","berlin"),
                          stringsAsFactors = FALSE)
myTib<-tibble(x=1:4,
              y=c("london","beijing","las vegas","berlin"))
class(myDf$y)
class(myDfNotFactor)
class(myTib$y)

#Want a variable to be a factor in a tibble
myTib<-tibble(x=1:4,
              y=factor(c("london","beijing","las vegas","berlin")))
myTib

#Subsetting tibbles
myDf[,1]
myTib[,1]
myTib[[1]]
myTib$x

#Variables are created sequentially
sequentialTib<-tibble(nItems=c(12,45,107),
                      cost=c(0.5,1.2,1.8),
                      totalWorth=nItems*cost)
sequentialTib

#Exploring the CO2 dataset
library(tibble)
data("CO2")
CO2tib<-as_tibble(CO2)
CO2tib

#Select() function
library(dplyr)
selectedData<-select(CO2tib,1,2,3,5)
selectedData

#Filtering rows
filteredData<-filter(selectedData, uptake>16)
filteredData

#Grouping data with the group_by() function
groupedData<-group_by(filteredData, Plant)
groupedData

#Creating summaries of variables using the summarize function
summarizedData<-summarize(groupedData, meanUp=mean(uptake), sdUp=sd(uptake))
summarizedData

#Creating new variables using the mutate() function
mutatedData<-mutate(summarizedData, cv=(sdUp/meanUp)*100)
mutatedData

#Arranging tibbles by variables using the arrange() function
arrangeData<-arrange(mutatedData, cv)
arrangeData

#Chaining dplyr operations together the %>% operator
arrangedData<-CO2tib %>%
  select(c(1:3,5))%>%
  filter (uptake>16)%>%
  group_by(Plant)%>%
  summarize(meanUp=mean(uptake), sdUp=sd(uptake))%>%
  mutate(CV=(sdUp/meanUp)*100)%>%
  arrange(CV)
arrangedData

#Plotting data with the ggplot() function
library(ggplot2)
data(iris)
myPlot<-ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width))+geom_point()+theme_bw()
myPlot

#Adding additional geom layers to a ggplot object
myPlot+
  geom_density_2d()+
  geom_smooth()

#Mapping species to the shape and color aesthetics
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, shape=Species))+
  geom_point()+
  theme_bw()
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species))+
  geom_point()+
  theme_bw()

#Grouping data with the group_by() function
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width))+
  facet_wrap(~Species)+
  geom_point()+
  theme_bw()

#Untidy Tibble
library(tibble)
library(tidyr)
patientData<-tibble(Patient=c("A","B","C"),
                    Month0=c(21,17,29),
                    Month3=c(20,21,27),
                    Month6=c(21,22,23))
patientData

#Tidying data with the gather() function
tidyPatientData<-gather(patientData, key=Month,
                        value=BMI, -Patient)
tidyPatientData

#Different ways to select columns for gathering
gather(patientData, key=Month, value=BMI, Month0:Month6)
gather(patientData, key=Month, value=BMI, c(Month0, Month3, Month6))

#Creating a list of numeric vectors
a<-20
pure<-function(){
  a<-a+1
  a
}
side_effect<-function(){
  a<<-a+1
  a
}
c(pure(),pure())
c(side_effect(),side_effect())

#Creating a list of numeric vectors
listofNumerics<-list(a=rnorm(5),
                     b=rnorm(9),
                     c=rnorm(10))
listofNumerics

#Using a for loop to iterate a function over a list
elementLengths<-vector('list', length=3)
for (i in seq_along(listofNumerics)){
  elementLengths[[i]]<-length(listofNumerics[[1]])
}
elementLengths

#Using map() to iterate a function over a list
map(listofNumerics,length)

#Returning atomic vectors with map_int(), map_chr(), and map_lgl()
map_int(listofNumerics, length)
map_chr(listofNumerics, length)
map_lgl(listofNumerics, length)

#Returning a tibble with map_int()
map_df(listofNumerics, length)

#Defining an anonymous function with function()
map(listofNumerics, function(.).+2)

#Using walk() to produce a function's side effects
par(mfrow=c(1,3))
walk(listofNumerics, hist)

#If use the name of each list element as the tile for each historgram
iwalk(listofNumerics,~hist(.x, main=.y))

#Iterating over multiple lists simultaneously
multipliers<-list(0.5,10,3)
map2(.x=listofNumerics, .y=multipliers, ~.x*.y)

#Using pmap() to iterate over mutliple lists
arguments<-expand.grid(n=c(100,200),
                       mean=c(1,10),
                       sd=c(1,10))
arguments
par(mfrow=c(2,4))
pmap(arguments, rnorm)%>%
  iwalk(~hist(.x, main=paste("Element",.y)))
