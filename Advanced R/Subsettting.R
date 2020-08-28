#Positive integers
x<-c(2.1, 4.2, 3.3, 5.4)
x[1]
x[3]
x[c(3,1)]
x[order(x)]

#Duplicate indices will duplicate values
x[c(1,1)]

#Real numbers are truncated to integers
x[c(2.1,2.9)]

#Negative integers
x[-c(3,1)]

#Using Logical
x[c(TRUE, TRUE, FALSE, FALSE)]
x[x>3]
x[]

#Using names
y<-setNames(x, letters[1:4])
y
y[c("d","c","a")]


#Matrices and arrays
a<-matrix(1:9, nrow=3)
colnames(a)<-c("A","B","C")
a[1:2,]
a[c(TRUE, FALSE, TRUE), c("B","A")]
a[0,-2]
a[1,]
a[1,1]

vals<-outer(1:5,1:5, FUN="paste", sep=",")
vals
vals[c(4,15)]
select<-matrix(ncol=2,byrow=TRUE,c(
  1,1,
  3,1,
  2,4
))
vals[select]

#Data frame and tibble
df<-data.frame(x=1:3, y=3:1,z=letters[1:3])
df
df[c(1,3),]
df[,c(1,3)]

library(tibble)
df<-tibble(x=1:3, y=3:1, z=letters[1:3])
str(df["x"])

#Preserving dimensionality
a<-matrix(1:4,nrow = 2)
a
str(a[1,,drop=FALSE])

df<-data.frame(a=1:2, b=1:2)
str(df[,"a"])

str(df[,"a",drop=FALSE])

z<-factor(c("a","b"))
z[1]
z[1,drop=TRUE]

#subsetting and assignment
x<-1:5
x[c(1:2)]<-c(101,102)
x

x<-list(a=1, b=2)
x
x[["b"]]<-NULL
str(x)

y<-list(a=1, b=2)
y["b"]<-list(NULL)
str(y)

#mtcars[] vs mtcars
mtcars[]<-lapply(mtcars, as.integer)
typeof(mtcars)
is.data.frame(mtcars)

mtcars<-lapply(mtcars, as.integer)
typeof(mtcars)
is.data.frame(mtcars)

#Look up tables
x<-c("m","f","u","f","f","m","m")
lookup<-c(m="Male",f="Female",u=NA)
lookup[x]
x
unname(lookup[x])

#Matching and merging by hand
grades<-c(1,2,2,3,1)
info<-data.frame(
  grade=3:1,
  desc=c("Excellent","Good","Poor"),
  fail=c(F,F,T)
)
id<-match(grades,info$grade)
id
info[id,]

#Random samples and bootstraps
df<-data.frame(x=c(1,2,3,1,2),y=5:1,z=letters[1:5])
df[sample(nrow(df)),]
df

#Select 3 random rows
df[sample(nrow(df),3),]

#Select 6 bootstrap replicates
df[sample(nrow(df),6, replace=TRUE),]


#Ordering
x<-c("b","c","a")
order(x)
x[order(x)]

#Expanding aggregated counts
df<-data.frame(x=c(2,4,1),y=c(9,11,6),n=c(3,5,1))
df
rep(1:nrow(df),df$n)
df[rep(1:nrow(df),df$n),]

#Removing columns from data frames
df<-data.frame(x=1:3, y=3:1, z=letters[1:3])
df
df$z<-NULL
df

#Return only the columns you want
df<-data.frame(x=1:3, y=3:1, z=letters[1:3])
df[c("x","y")]
df[setdiff(names(df),"z"),]

#Selecting rows based on a condition
data("mtcars")
mtcars[mtcars$gear==5,]
mtcars[mtcars$gear==5 & mtcars$cyl==4,]

#Boolean algebra versus sets
x<-sample(10)<4
which(x)
unwhich<-function(x,n){
  out<-rep_len(FALSE, n)
  out[x]<-TRUE
  out
}
unwhich(which(x),10)

