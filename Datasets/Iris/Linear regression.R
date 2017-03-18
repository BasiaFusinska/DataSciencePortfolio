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
