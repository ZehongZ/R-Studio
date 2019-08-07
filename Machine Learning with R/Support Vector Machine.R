#Import dataset
letters<-read.csv("letterdata.csv")

#Overview of the dataset
str(letters)

#Splitting data into training and test data
letters_train<-letters[1:16000,]
letters_test<-letters[16001:20000,]

#Training a model
library(e1071)
library(kernlab)
letter_classifier<-ksvm(letter~., data=letters_train, kernel="vanilladot")
letter_classifier

#Evaluating model performance
letter_predictions<-predict(letter_classifier, letters_test)
head(letter_predictions)
table(letter_predictions, letters_test$letter)
agreement<-letter_predictions==letters_test$letter
table(agreement)
prop.table(table(agreement))

#Improve model performance
letter_classifier_rbf<-ksvm(letter~., data=letters_train, kernel="rbfdot")

#Make predictions
letter_predictions_rbf<-predict(letter_classifier_rbf, letters_test)

#Evaluating model performance
agreement_rbf=letter_predictions==letters_test$letter
prop.table(table(agreement_rbf))
