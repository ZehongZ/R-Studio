####Understanding Support Vector Machines####
#SVM: Can be imagined as a surface that creates a boundary between points of data plotted in multidimensional that represent examples and their feature values
#The goal of a SVm is to create a flat boundary called a hyperplane, which divides the splace to create fairly homogeneous partitions on either side.
#SVMs can be adapted for use with nearly any type of learning task, including both classification and numeric prediction

####Classification with hyperplanes####
#SVMs use a boundary called a hyperplane to partition data into groups of similar class values
#MMH: Maximum Margin Hyperplane that creates the greatest separation between the two classes
#The support vectors are the points from each class that are the closest to the MMH; each class myst have at least one support vector, but it is possible to have more than one

####The case of nonlinear separable data####
#The solution to this problem is the use of a slack variable, which creates a soft margin that allows some points to fall on the incorrect side of the margin
#A cost value is applied to all points that violate the constraints, and rather than finding the maximum margin, the algorithm attempts to minimize the total cost
#Modifying the cost parameter will adjust the penalty, the fall on the wrong side of the hyperplane
#The greater the cost parameter, the harder the optimization will try to achieve 100 percent separation
#A lower cost parameter will place the emphasis on a wider overall margin. 

####Using kernels for non-linear space####
#The kernel trick involves a process of constructing new features that express mathematical relationships betweem measured characteristics.
#Linear kernel: does not transform the data at all
#Polynomial kernel: add a simple nonlinear transformation of the data
#Sigmoid kernel: somewhat analogous to a neural network using a sigmoid activation function
#Gaussian RBF kernel: similar to a RBF neural network. The RBF kernel performs well on many types of data and is thought to be a reasonable starting point for many learning tasks
#There is no reliable rule to match a kernel to a particular learning task. 
#The fit depends heavily on the concept to be learned as well as the amount of training data and the relationships among the feature

####Performing OCR with SVMs
letters<-read.csv("letterdata.csv")
str(letters)
dim(letters)
letters_train<-letters[1:16000,]
letters_test<-letters[16001:20000,]
#Training a model on the data
library(e1071)
library(kernlab)
#Kernel: "rbfdot"--Radial basis, "polydot"--Polynomial, "tanhdot"--hyperbolic tangent sigmoid, "vanilladot"--linear
#C: knumber of specifies the cost of violating the constraints, how big of a penalty there is for the "soft margin". Larger values will result in narrower margins
letter_classifier<-ksvm(letter~., data=letters_train, kernel="vanilladot")
letter_classifier
#Evaluating model performance
letter_predictions<-predict(letter_classifier, letters_test)
head(letter_predictions)
table(letter_predictions, letters_test$letter)
agreement<-letter_predictions==letters_test$letter
table(agreement)#Correctly classify 3357/4000
prop.table(table(agreement))
#Improving model performance
letter_classifier_rbf<-ksvm(letter~., data=letters_train, kernel="rbfdot")
letters_predictions_rbf<-predict(letter_classifier_rbf,letters_test)
agreement_rbf<-letters_predictions_rbf==letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))
