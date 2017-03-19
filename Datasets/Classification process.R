install.packages('caret')
install.packages('e1071')
install.packages('class')

library(caret)
library(e1071)
library(class)

split.dataset <- function(data, data.column, split){
  trainIndex <- createDataPartition(data.column, p=split, list=FALSE)
  return(list(train = data[trainIndex,], test = data[-trainIndex,]))
}

train.model <- function(data, method, label) {
  if (method == "nb") {
    return(naiveBayes(formula(paste(label, " ~ .")), data = data))
  }
  else if (method == "knn") {
    return(data)
  }
}

predict.labels <- function(model, data, is.knn = FALSE, label.inx = -1){
  if (is.knn){
    if (label.inx == -1){
      label.inx <- length(model)
    }
    return(knn(model[, -label.inx], data[, -label.inx], model[, label.inx], k = 5, prob = TRUE))
  }
  else {
    return(predict(model, data))
  }
}
