library(rvest)
cran_ml <- html("http://cran.r-project.org/web/views/MachineLearning.html")
cran_ml
ml_packages<-html_nodes(cran_ml, "a")
head(ml_packages,n=7)
