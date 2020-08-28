#Longer vectors
lgl_var<-c(TRUE, FALSE)
int_var<-c(1L, 6L, 10L)
dbl_var<-c(1,2.5, 4.5)
chr_var<-c("these are","some strings")

c(c(1,2),c(3,4))

#Missing values
NA>5
10*NA
!NA

x<-c(NA,5,NA,10)
x==NA

is.na(x)

#Testing and coercion
str(c("a",1))
x<-c(FALSE, FALSE, TRUE)
as.numeric(x)
sum(x)
mean(x)
as.integer(c("1","1.5","a"))

#Attributes-Getting and Setting
a<-1:3
attr(a, "x")<-"abcdef"
attr(a,"x")
a

a<-structure(
  1:3,
  x="abcdef",
  y=4:6
)
a
str(attributes(a))


#NAMES
#When creating it
x<-c(a=1,b=2,c=3)
x

#By assigning a character vector to names()
x<-1:3
names(x)<-c("a","b","c")

#Inline, with setNames():
x<-setNames(1:3,c("a","b","c"))
x

#Dimensions
#Two scalar arguments specify row and column sizes
a<-matrix(1:6, nrow=2, ncol=3)
a

#One vector argument to describe all dimensions
b<-array(1:12, c(2,3,2))
b

#You can also modify an object in place by setting dim()
c<-1:6
c
dim(c)<-c(3,2)
c

#1d vector
str(1:3)
#Column vector
str(matrix(1:3, ncol=1))
#Row vector
str(matrix(1:3), nrow=1)
#"Array" vector
str(array(1:3,3))

#Factors
x<-factor(c("a","b","b","a"))
x

typeof(x)

attributes(x)

sex_char<-c("m","m","m")
sex_factor<-factor(sex_char, levels=c("m","f"))

table(sex_char)

table(sex_factor)

grade<-ordered(c("b","b","a","C"), levels=c("c","b","a"))
grade

#Dates
today<-Sys.Date()

typeof(today)

attributes(today)

date<-as.Date("1970-02-01")
unclass(date)

#Date-time
now_ct<-as.POSIXct("2018-08-01 22:00",tz="UTC")
now_ct

typeof(now_ct)

attributes(now_ct)

structure(now_ct, tzone="Asia/Tokyo")
structure(now_ct, tzone="America/New_York")

#Duration
one_week_1<-as.difftime(1, units="weeks")
one_week_1
typeof(one_week_1)
attributes(one_week_1)

one_week_2<-as.difftime(7, units="days")
one_week_2
typeof(one_week_2)
attributes(one_week_2)

#Lists
#Creating
l1<-list(
  1:3,
  "a",
  c(TRUE, FALSE, TRUE),
  c(2.3, 5.9)
)
typeof(l1)
str(l1)

l4<-list(list(1,2),c(3,4))
l5<-c(list(1,2),c(3,4))
str(14)
str(15)

#Testing and coercion
list(1:3)
as.list(1:3)

#Matrices and arrays
l<-list(1:3,"a",TRUE, 1.0)
l
dim(l)<-c(2,2)
l

#Data frames and tibbles
df1<-data.frame(x=1:3, y=letters[1:3])
typeof(df1)

attributes(df1)

library(tibble)
df2<-tibble(x=1:3,y=letters[1:3])
typeof(df2)
df2
attributes(df2)

#Creating
df<-data.frame(
  x=1:3,
  y=c("a","b","c")
)
str(df)

df1<-data.frame(x=1:3,
                y=c("a","b","c"),
                stringsAsFactors = FALSE)
str(df1)

df2<-tibble(
  x=1:3,
  y=c("a","b","c")
)
str(df2)
names(data.frame("1"=1))

#Row names
df3<-data.frame(
  age=c(35,27,18),
  hair=c("blond","brown","black"),
  row.names = c("Bob","Susan","Sam")
)
df3
rownames(df3)
df3["Bob",]
df3[c(1,1,1),]

#List columns
library(dplyr)
starwars
df<-data.frame(x=1:3)
df$y<-list(1:2,1:3,1:4)
data.frame(
  x=1:3,
  y=I(list(1:2,1:3,1:4))
)
tibble(
  x=1:3,
  y=list(1:2,1:3,1:4)
)

#Matrix and data frame columns
dfm<-data.frame(
  x=1:3*10
)
dfm$y<-matrix(1:9, nrow = 3)
dfm$z<-data.frame(a=3:1, b=letters[1:3], stringsAsFactors = FALSE)
str(dfm)
