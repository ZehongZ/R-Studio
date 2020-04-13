#Vectors
a<-c(1,2,5,3,6,-2,4)
b<-c('one','two','three')
c<-c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE)
#Referencing
a<-c('k','j','h','a','c','m')
a[3]
a[c(1,3,5)]
a[2:6]
a[c(2,3,4,5,6)]

#Creating Matrices
y<-matrix(1:20, nrow=5, ncol=4)
y
cells<-c(1,26,24,68)
rnames<-c('R1','R2')
cnames<-c('C1','C2')
mymatrix<-matrix(cells, nrow=2, ncol=2, byrow=TRUE ,dimnames = list(rnames, cnames))
mymatrix

#Using Matrix Subsccripts
x<-matrix(1:10, nrow=2)
x
x[1,c(4,5)]

#Create arrays
dim1<-c('A1','A2')
dim2<-c('B1','B2','B3')
dim3<-c('C1','C2','C3','C4')
z<-array(1:24, c(2,3,4), dimnames=list(dim1,dim2,dim3))
z

#Creating a data frame
patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
diabetes<-c('Type1','Tpype2','Type1','Type1')
status<-c('Poor','Improved','Excellent','Poor')
patientdata<-data.frame(patientID, age, diabetes, status)
patientdata
#Specifiying elements of a data frame
patientdata[1:2]
patientdata[c('diabetes','status')]
patientdata$age
#Cross-tabulate diabetes type by status
table(patientdata$diabetes,patientdata$status)

#Attach, Detach, and With
summary(mtcars$mpg)
plot(mtcars$mpg, mtcars$disp)
plot(mtcars$mpg, mtcars$wt)
#Attach/Detach
attach(mtcars)
summary(mpg)
plot(mpg,disp)
plot(mpg,wt)
detach(mtcars)
#With()
with(mtcars, {
  print(summary(mpg))
  plot(mpg,disp)
  plot(mpg,wt)
})

#Factors
status<-c('poor','improved, excellent')
status<-factor(status,order=TRUE)#(Order=True makes it an ordinal)
status
#Overwriting order default
status<-factor(status, order=TRUE, levels=c('poor','improved','excellent'))
status
#Numeric variables can be coded as factors using the levels and labels
sex<-c(1,2)
sex<-factor(sex, levels=c(1,2), labels=c('Male','Female'))
sex

#Lists
g<-'My First List'
h<-c(25,26,18,39)
j<-matrix(1:10, nrow=5)
k<-c('one','two','three')
mylist<-list(title=g, ages=h,j,k)
mylist
mylist[2]
mylist[[2]]

#Entering data from the keyboard
mydata<-data.frame(age=numeric(0),
                   gender=character(0),
                   weight=numeric(0))
mydata<-edit(mydata)
mydata<-edit(mydata)
mydatatxt<-"
age gender weight
25  m 166
30  f 116
18  f 120
"
mydata<-read.table(header=TRUE, text=mydatatxt)
mydata
