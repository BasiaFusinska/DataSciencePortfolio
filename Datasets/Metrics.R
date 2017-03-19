install.packages('Metrics')
install.packages('caret')
install.packages('e1071')

library('Metrics')

# Relative Squared Error
rse <- function (actual, predicted)
{
  sum(se(actual, predicted)) / sum(se(actual, mean(actual)))
}

# Relative Absolute error
rae <- function (actual, predicted)
{
  sum(ae(predicted, actual)) / sum(ae(actual, mean(actual)))
}

# Macro-averaged precision
ma_aprecision <- function (cMatrix) { 
  mean(cMatrix$byClass[,'Precision'])
}

#Macro-averaged recall
ma_arecall <- function (cMatrix) {
  mean(cMatrix$byClass[,'Recall'])
}

# One-vs-All
sumAll <- function (cMatrix) {
  cm <- cMatrix$table
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  
  oneVsAll <- lapply(1 : nc,
                     function(i){
                       v = c(cm[i,i],
                             rowsums[i] - cm[i,i],
                             colsums[i] - cm[i,i],
                             n-rowsums[i] - colsums[i] + cm[i,i]);
                       return(matrix(v, nrow = 2, byrow = T))})
  
  s = matrix(0, nrow = 2, ncol = 2)
  for(i in 1 : nc){s = s + oneVsAll[[i]]}
  s
}

#Average accuracy
avg_accuracy <- function(sumAll) {
  sum(diag(sumAll)) / sum(sumAll)
}

#Micro-averaged precision
mi_precision <- function(sumAll) {
  (diag(sumAll) / apply(sumAll,1, sum))[1]
}

#Micro-averaged recall
mi_recall <- function(sumAll) {
  (diag(sumAll) / apply(sumAll,2, sum))[1]
}

classification.metrics <- function(actualLabels, predictedLabels, nlabels){
  require(ggplot2)
  require(caret)
  
  xtab <- table(actualLabels, predictedLabels)
  
  cMatrix <- confusionMatrix(xtab)
  cat("\n")
  print(cMatrix)

  # Overall Accuracy
  cat("\nOverall Accuracy: ", cMatrix$overall[['Accuracy']])
  
  # Macro-averaged precision
  cat("\nMacro-averaged precision: ", ma_aprecision(cMatrix))
  
  #Macro-averaged recall
  cat("\nMacro-averaged recall: ", ma_arecall(cMatrix))
  
  sum_All <- sumAll(cMatrix)

  #Average accuracy
  cat("\nAverage accuracy: ", avg_accuracy(sum_All))
  
  #Micro-averaged precision/recall
  cat("\nMicro-averaged precision/recall: ", mi_precision(sum_All), ", ", mi_recall(sum_All), "\n\n")
  
  # Confusion Matrix Visualisation
  confusion <- as.data.frame(cMatrix$table)
  
  colnames(confusion) <- c('ActualClass', 'PredictedClass', 'Freq')
  print(confusion)
  
  confusion$fill <- 'TN'
  confusion$fill[confusion$Freq > 0] <- 'FP'
  confusion$fill[confusion$ActualClass == confusion$PredictedClass] <- 'TP'
  
  a <- aggregate(Freq ~ ActualClass, data = confusion, sum)

  confusion$sum <- rep(a$Freq, 3)
  
  confusion$prob <- round(confusion$Freq/confusion$sum*100, digits=1)
  confusion$probText <- paste(confusion$prob, " %")
  confusion$probText[confusion$prob == 0.0] <- ''
  
  ggplot(confusion) +
   geom_tile(aes(x=ActualClass, y=PredictedClass, fill=fill)) + geom_text(aes(x=ActualClass, y=PredictedClass,label=probText)) +
    scale_fill_manual(values=c("#999999", "#FFFFFF", "#56B4E9")) +
    scale_x_discrete(name="Actual Class") + scale_y_discrete(name="Predicted Class") + 
    labs(fill="Normalized\nFrequency")
}
