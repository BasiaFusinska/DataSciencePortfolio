# Petal width and length are highly correlated
# Let's run linear regression for them

lsmodel <- lsfit(iris$Petal.Length, iris$Petal.Width)
summary(lsmodel)
lsmodel$coefficients

ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(aes(col=Species)) +
  ggtitle("Width by Length for Petal") +
  geom_abline(slope=lsmodel$coefficients[2], intercept = lsmodel$coefficients[1])

lmmodel <- lm(Petal.Width ~ Petal.Length, data=iris)
summary(lmmodel)

# Check it for the Sepal
sepal.lm <- lm(Sepal.Length ~ Sepal.Width, data=iris)
summary(sepal.lm)

sepal.lm$coefficients

ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length)) +
  geom_point(aes(col=Species)) +
  ggtitle("Width by Length for Sepal") +
  geom_abline(slope=sepal.lm$coefficients[2], intercept = sepal.lm$coefficients[1])

sepal.lm.set <- lm(Sepal.Length ~ Sepal.Width, data=iris[iris$Species=="setosa",])
sepal.lm.ver <- lm(Sepal.Length ~ Sepal.Width, data=iris[iris$Species=="versicolor",])
sepal.lm.vir <- lm(Sepal.Length ~ Sepal.Width, data=iris[iris$Species=="virginica",])
sepal.lm.set$coefficients
sepal.lm.ver$coefficients
sepal.lm.vir$coefficients

ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length)) +
  geom_point(aes(col=Species)) +
  ggtitle("Width by Length for Sepal") +
  geom_abline(slope=sepal.lm$coefficients[2], intercept = sepal.lm$coefficients[1]) +
  geom_abline(color="red", slope=sepal.lm.set$coefficients[2], intercept = sepal.lm.set$coefficients[1]) +
  geom_abline(color="green", slope=sepal.lm.ver$coefficients[2], intercept = sepal.lm.ver$coefficients[1]) +
  geom_abline(color="blue", slope=sepal.lm.vir$coefficients[2], intercept = sepal.lm.vir$coefficients[1])

sepal.lm.total <- lm(Sepal.Length ~ Sepal.Width:Species + Species - 1, data=iris)
summary(sepal.lm.total)
sepal.lm.total$coefficients

# Using step
summary(step(lm(Sepal.Length ~ Sepal.Width * Species, data=iris)))

area.lm <- lm(Petal ~ Sepal, data=iris.areas)

ggplot(iris.areas, aes(x=Sepal, y=Petal)) +
  geom_point(aes(col=Species)) +
  ggtitle("Petal by Sepal") +
  geom_abline(slope=area.lm$coefficients[2], intercept = area.lm$coefficients[1])

area.lm.set <- lm(Petal ~ Sepal, data=iris.areas[iris.areas$Species == "setosa",])
area.lm.ver <- lm(Petal ~ Sepal, data=iris.areas[iris.areas$Species == "versicolor",])
area.lm.vir <- lm(Petal ~ Sepal, data=iris.areas[iris.areas$Species == "virginica",])
area.lm.set$coefficients
area.lm.ver$coefficients
area.lm.vir$coefficients

ggplot(iris.areas, aes(x=Sepal, y=Petal)) +
  geom_point(aes(col=Species)) +
  ggtitle("Petal by Sepal") +
  geom_abline(slope=area.lm$coefficients[2], intercept = area.lm$coefficients[1]) +
  geom_abline(color="red", slope=area.lm.set$coefficients[2], intercept = area.lm.set$coefficients[1]) +
  geom_abline(color="green", slope=area.lm.ver$coefficients[2], intercept = area.lm.ver$coefficients[1]) +
  geom_abline(color="blue", slope=area.lm.vir$coefficients[2], intercept = area.lm.vir$coefficients[1])

area.lm.total <- lm(Petal ~ Sepal:Species + Species - 1, data=iris.areas)
summary(area.lm.total)
area.lm.total$coefficients
