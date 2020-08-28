#Choice#
grade<-function(x){
  if (x>90){
    "A"
  }else if (x>80){
    "B"
  }else if (x>50){
    "C"
  }else{
    "F"
  }
}
grade(60)
grade(90)
grade(91)

#returns a value so that you can assign the results
x1<-if(TRUE) 1 else 2
x2<-if(FALSE)1 else 2
c(x1,x2)

#paste()drop NULL inputs
greet<- function(name, birthday=FALSE){
  paste0(
    "Hi ",name,
    if (birthday)" and HAPPY BIRTHDAY"
  )
}
greet("Maria", FALSE)
greet("Jaime",TRUE)

#Vectorised if
x<-1:10
ifelse(x %% 5==0,"XXX", as.character(x))
ifelse(x %% 2 == 0, "even", "odd")

#Another vectorised equivalent
library(dplyr)
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  is.na(x)~"???",
  TRUE ~ as.character(x)
)

#switch() statement
x_option<-function(x){
  if (x=="a"){
    "option 1"
  } else if (x == "b"){
    "option 2"
  }else if (x == "C"){
    "option 3"
  }else{
    stop("Invalid 'x' value")
  }
}

x_option<-function(x){
  switch(x,
         a="option 1",
         b="option 2",
         c="option 3",
         stop("Invalid 'x' value"))
}

switch("c", a=1, b=2)

legs<-function(x) {
  switch(x,
         cow=,
         horse=,
         dog=4,
         human=,
         chicken=2,
         plant=0,
         stop("Unknow input"))
}
legs("cow")


#Loop#
for (i in 1:3){
  print(i)
}

i<-100
for (i in 1:3){}
i

#Terminate Loops
for (i in 1:10){
  if (i<3)
    next
  print(i)
  if (i>=5)
    break
}

#Common Pitfalls
means<-c(1,50,20)
out<-vector("list", length(means))
for (i in 1:length(means)){
  out[[i]]<-rnorm(10, means[i])
}

#seq_along(x) always returns a value the same length as x
seq_along(means)
out<-vector("list",length(means))
for(i in seq_along(means)){
  out[[i]]<-rnorm(10, means[[i]])
}

xs<-as.Date("2020-01-01","2010-01-01")
for (x in xs){
  print(x)
}

for (i in seq_along(xs)){
  print(xs[[i]])
}

#Function components
f02<-function(x,y){
  x+y
}
formal(f02)#the list of arguments that control how you call the function
f02(1,2)

#First-class functions
f01<-function(x){
  sin(1/x^2)
}
#Aonymous function
lapply(mtcars, function(x) length(unique(x)))
Filter(function(x) !is.numeric(x),mtcars)
integrate(function(x) sin(x)^2,0,pi)
#Put function in a list
funs<-list(
  half=function(x) x/2,
  double=function(x)x*2
)
funs$double(10)

#Invoking a function
#When arguments already in a data structure
args<-list(1:10, na.rm=TRUE)
do.call(mean, args)

#Function composition
square<-function(x)x^2
deviation<-function(x)x-mean(x)
#Nest the function calls
x<-runif(100)
sqrt(mean(square(deviation(x))))

library(magrittr)
x%>%
  deviation()%>%
  square()%>%
  mean()%>%
  sqrt()

#Get Lexical scoping
#Look up values of names based on how a function is defined, not how it is called.
x<-10
g01<-function(){
  x<-20
  x
}
g01()

#Name masking
x<-10
y<-20
g02<-function(){
  x<-1
  y<-2
  c(x,y)
}
g02()

#Function vs Variables
g07<-function(x)x+1
g08<-function(){
  g07<-function(x)x+100
  g07(10)
}
g08()

g09<-function(x)x+100
g10<-function(){
  g09<-10
  g09(g09)
}
g10()

#A fresh start
g11<-function(){
  if (!exists("a")){
    a<-1
  }else{
    a<-a+1
  }
  a
}
g11()
g11()

#Dynamic lookup
g12<-function()x+1
x<-15
g12()
x<-20
g12()

#Lazy evaluation
h01<-function(x){
  10
}
h01(stop("This is an error"))

#Promises
y<-10
h02<-function(x){
  y<-100
  x+1
}
h02(y)

double<-function(x){
  message("Calculating...")
  x*2
}

h03<-function(x){
  c(x,x)
}
h03(double(x))

#Default arguments
h04<-function(x=1, y=x*2,z=a+b){
  a<-10
  b<-100
  c(x,y,z)
}
h04()

h05<-function(x=ls()){
  a<-1
  x
}
h05()
h05(ls())

#Missing arguments
h06<-function(x=10){
  list(missing(x),x)
}#Determine if an argument's value comes from the user or from a default
str(h06())
str(h06(10))

args(sample)

sample<-function(x, size=NULL, replace=FALSE, prob=NULL){
  if(is.null(size)){
    size<-length(x)
  }
  x[sample.int(length(x),size,replace=replace, prob=prob)]
}

#dot-dot-dot
#with in function can take any number of additional arguments
i01<-function(y,z){
  list(y=y, z=z)
}

i02<-function(x,...){
  i01(...)
}

str(i02(x=1,y=2,z=3))

i03<-function(...){
  list(first=..1,third=..3)
}
str(i03(1,2,3))

i04<-function(...){
  list(...)
}
str(i04(a=1,b=2))

x<-list(c(1,3,NA), c(4,NA,6))
str(lapply(x,mean,na.rm=TRUE))

print(factor(letters),max.levels=4)
print(y~x, showEnv=TRUE)

#Existing a function#
#Implicit versus explicit returns
j01<-function(x){
  if (x<10){
    0
  }else{
    10
  }
}
j01(5)
j01(15)
#Explicitly by calling return()
j02<-function(x){
  if (x<10){
    return(0)
  }else{
    return(10)
  }
}

#Invisible values
j03<-function()1
j03
j04<-function()invisible(1)
j04
print(j04())
(j04())
str(withVisible(j04()))

#Exit handlers
j06<-function(x){
  cat("Hello\n")
  on.exit(cat("Goodbye!\n"),add=TRUE)
  
  if(x){
    return(10)
  }else{
    stop("Error")
  }
}
j06(TRUE)
j06(FALSE)

cleanup<-function(dir, code){
  old_dir<-setwd(dir)
  on.exit(setwd(old_dir), add=TRUE)
  old_opt<-options(stringsAsFactors = FALSE)
  on.exit(options(old_opt),add=TRUE)
}

with_dir<-function(dir, code){
  old<-setwd(dir)
  on.exit(setwd(old), add=TRUE)
  force(code)
}
getwd()
with_dir("-",getwd())

j08<-function(){
  on.exit(message("a"),add = TRUE)
  on.exit(message("b"),add = TRUE)
}
j08()

j09<-function(){
  on.exit(message("a"), add=TRUE, after=FALSE)
  on.exit(message("b"), add=TRUE, after=FALSE)
}
j09()

#Function forms#
#Rewriting to prefix form
x+y
'+'(x,y)
naems(df)<-c("x","y","z")
