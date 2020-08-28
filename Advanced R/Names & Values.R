#Binding basics
x<-c(1,2,3)

#Copy-on-modify
x<-c(1,2,3)
y<-x
y[[3]]<4
x
cat(tracemem(x))

#List
l1<-list(1,2,3)
l2<-l1
l2[[3]]<4
l2

#Data frames
d1<-data.frame(x=c(1,5,6), y= c(2,4,3))
d1
d2<-d1
d2[,2]<-d2[,2]*2
d2
d3<-d1
d3[1,]<-d3[1,]*3
d3

#Character vectors
x<-c("a","a","abc","d")

#Object size
lobstr::obj_size(letters)

#Unbinding and the garbage collector
x<-1:3
x<-2:4
rm(x)
x
