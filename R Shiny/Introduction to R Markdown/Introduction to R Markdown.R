#An interactive document is an R Markdown file that contains Shiny widgets and outputs. 
#You write the report in markdown and then launch it as an app with click of a button

#R Markdown is a file format for making dynamic documents with R. 
#An R Markdown document is written in markdown(an easy-to-write plain text format) and contains chunks of embedded R code

library(rmarkdown)

#R Markdown files are the source code for rich, reporducible documents. You can transform an R Markdown file in two ways: knit, convert
#Knit:The rmarkdown package will call the knitr package. knitr will run each chunk of R code in the document and append the results of the code to the document next to the code chunk
#Convert: You can convert your.Rmd file into an HTML, PDF or Word file. 