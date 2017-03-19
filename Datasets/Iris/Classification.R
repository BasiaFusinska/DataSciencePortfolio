# Split data to train and test
install.packages('caTools')
library(caTools)

tt.datasets <- split.dataset(iris, .75)
iris.train <- tt.datasets$train
iris.test <- tt.datasets$test

summary(iris.train)
summary(iris.test)

# kNN

library(class)
knnModel <- knn(iris.train[,1:4], iris.test[,1:4], iris.train[,5], k = 3, prob = TRUE)
knnModel
summary(knnModel)

install.packages("gmodels")
library(gmodels)

CrossTable(iris.test[,5], knnModel, prop.chisq = FALSE)

library(ggplot2)
library(caret) 
classification.metrics(iris.test[,5], knnModel, 3)
