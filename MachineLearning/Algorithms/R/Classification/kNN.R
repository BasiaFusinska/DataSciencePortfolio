# Implementation of kNN classification algorithm

kNN <- function(train, labels, x, k=3, distance=function(x, y) { dist(rbind(x, y))[1] }) {
  train <- as.matrix(train)
  N <- nrow(train)
  M <- ncol(train)

  if (N != length(labels)) {
    stop("Training set should have the same size as labels")
  }

  if (M != length(x)) {
    stop("Classified example should have the same size as training examples")
  }
  
  # Calculating distances
  distances <- apply(train, 1, distance, y=x)
  
  # Get the k labels with minimum distance 
  maxIndexes <- which(rank(distances) <= k)
  nNeighbours <- labels[maxIndexes]
  
  # Generate statistics - neighbours' labels and probabilities
  freqTable <- table(nNeighbours)
  freqTable <- sort(freqTable[freqTable > 0], decreasing = TRUE)
  result <- as.data.frame(freqTable)
  result$Freq <- result$Freq / k
  names(result) <- c('Label', 'Probability') 
  
  result
}
