class(iris)

head(iris)
attributes(iris)
colnames(iris)

summary(iris)

library(ggplot2)

# Distribution of the sepal and petals lenght and width

slhist <- draw.hist(iris, aes(x = Sepal.Length), "Sepal Length")
swhist <- draw.hist(iris, aes(x = Sepal.Width), "Sepal Width")
plhist <- draw.hist(iris, aes(x = Petal.Length), "Petal Length")
pwhist <- draw.hist(iris, aes(x = Petal.Width), "Petal Width")

multiplot(slhist, swhist, plhist, pwhist, cols=2)

# Density plots per sepcies

sldens <- ggplot(iris, aes(x=Sepal.Length, colour=Species)) + geom_density()
swdens <- ggplot(iris, aes(x=Sepal.Width, colour=Species)) + geom_density()
pldens <- ggplot(iris, aes(x=Petal.Length, colour=Species)) + geom_density()
pwdens <- ggplot(iris, aes(x=Petal.Width, colour=Species)) + geom_density()

multiplot(sldens, swdens, pldens, pwdens, cols=2)

# Looks like Petals and Sepals have different distributions
# Petal values seem to separate classes better

# Plot widths by Lenths
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(col=Species)) +
  ggtitle("Width by Length for Sepal")

ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(col=Species)) +
  ggtitle("Width by Length for Petal")

# Calculate correlation for the attributes
cor(iris$Sepal.Length, iris$Sepal.Width)
cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris$Sepal.Length, iris$Petal.Width)
cor(iris$Sepal.Width, iris$Petal.Length)
cor(iris$Sepal.Width, iris$Petal.Width)
cor(iris$Petal.Length, iris$Petal.Width)

# Draftman's display, Pearson's correlation, histograms, box plots and species distribution
install.packages("GGally")
library(GGally)
ggpairs(iris, aes(colour = Species, alpha=0.4))

# Statistics for the area
iris.areas <- data.frame(Species = iris$Species)
iris.areas$Sepal <- iris$Sepal.Length * iris$Sepal.Width
iris.areas$Petal <- iris$Petal.Length * iris$Petal.Width

head(iris.areas)
summary(iris.areas)

ggplot(iris.areas, aes(x=Sepal, y=Petal)) +
  geom_point(aes(col=Species)) +
  ggtitle("Petal by Sepal")

ggpairs(iris.areas, aes(colour = Species, alpha=0.4))
