# -------- OSLR---------------

# Prepare input data
#(y1, y2) = (2x1 + x2 + 3, -x1 + 4x2 - 5)
X <- matrix(c(1, 2, 3, 4, 7, 1), nrow=3)
Y <- matrix(c(9, 14, 10, 10, 21, -4), nrow=3) 
X
Y

# Get model
w <- ordinaryRegularRegression(X, Y)
result <- solveORR(X, w)

# Present results
print("Weights:")
print(w)
print("Predicted values:")
print(result)
