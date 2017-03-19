# Splitting dataset to train and test
tt.datasets <- split.dataset(iris, iris$Species, .75)
iris.train <- tt.datasets$train
iris.test <- tt.datasets$test

summary(iris.train)
summary(iris.test)

# Train the model

# Naive Bayes
nb.model <- train.model(iris.train, "nb", "Species")

# kNN
knn.model <- train.model(iris.train, "knn", "Species")

# Scores for labels

# Naive Bayes
lb.nb.train <- predict.labels(nb.model, iris.train)
lb.nb.test <- predict.labels(nb.model, iris.test)

# kNN
lb.knn.train <- predict.labels(knn.model, iris.train, is.knn = TRUE)
lb.knn.test <- predict.labels(knn.model, iris.test, is.knn = TRUE)

# Classification results

# Naive Bayes
classification.metrics(iris.train[,5], lb.nb.train, 3)
classification.metrics(iris.test[,5], lb.nb.test, 3)

# kNN
classification.metrics(iris.train[,5], lb.knn.train, 3)
classification.metrics(iris.test[,5], lb.knn.test, 3)
