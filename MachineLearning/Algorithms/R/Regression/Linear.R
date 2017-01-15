# Implementation of Ordinary Least Square Regression

ordinaryRegularRegression <- function(X, Y){
  n <- nrow(X)
  X <- cbind(rep(1, n), X)
  print(X)
  
  Xt = t(X)
  solve(Xt %*% X) %*% Xt %*% Y
}

solveORR <- function(X, w) {
  n <- nrow(X)
  X <- cbind(rep(1, n), X)
  
  X %*% w
}
