# -------- kNN---------------

# Prepare input data
train <- data.frame(x1=1:10, x2=5:14)
labels <- c('A', 'A', 'B', 'C', 'C', 'A', 'B', 'A', 'C', 'B')

# Get results
result <- kNN(train, labels, c(4, 8))

# Present results
print(result)
sprintf("Winning class: %s", result[1,1])
