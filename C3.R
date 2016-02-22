### chapter 3 ###
### 

library(ISLR)
ad.df <- data.frame(read.csv("advertising.csv"))

dev.new(width = 16, height = 12)

par(mfrow = c(1, 3))

plot(Sales ~ TV, data = ad.df, col = 'dark red', main = 'TV')
ad.lm1 <- lm(Sales ~ TV, data = ad.df)
abline(ad.lm1, col = 'dark blue')

plot(Sales ~ Radio, data = ad.df, col = 'dark red', main = 'Radio')
ad.lm2 <- lm(Sales ~ Radio, data = ad.df)
abline(ad.lm2, col = 'dark blue')

plot(Sales ~ Newspaper, data = ad.df, col = 'dark red', main = 'Newspaper')
ad.lm3 <- lm(Sales ~ Newspaper, data = ad.df)
abline(ad.lm3, col = 'dark blue')

### plot residual along the regression line
# plot scatterplot and the regression line
dev.new()
attach(ad.df)
par(mfrow = c(1, 1))
plot(Sales ~ TV, data = ad.df, xlim = c(min(TV) - 5, max(TV) + 5),
     ylim = c(min(Sales) - 5, max(Sales) + 5), col = "dark red")
abline(ad.lm1, lwd=2, col = "dark blue")

# calculate residuals and predicted values
ad.lm1.res <- signif(residuals(ad.lm1), 5)
ad.lm1.pre <- predict(ad.lm1)

# plot distances between points and the regression line
segments(TV, Sales, TV, ad.lm1.pre, col="red")

# add labels (res values) to points
library(calibrate)
textxy(TV, Sales, ad.lm1.res, cx=0.7)

### simulated data
x <- runif(1000) * 4 - 2
y <- 2 + 3 * x + rnorm(1000)
y.true <- 2 + 3 * x

xy.lm <- lm(y ~ x)

plot(y ~ x, col = 'green')
abline(xy.lm, lm, col = 'dark red')
abline(h = y.true, v = x, col = 'dark blue')

### multiple linear regression
rm(list = ls())

library(ISLR)
library(Rcmdr)
ad.df <- data.frame(read.csv("advertising.csv"))
ad.mlr <- lm(Sales ~ TV + Radio + Newspaper, data = ad.df)
# Newspaper is insignificant
ad.mlr <- lm(Sales ~ TV + Radio, data = ad.df)
attach(ad.df)
scatter3d(TV, Radio, Sales, type="p", col="red", xlab="TV", ylab="Radio", zlab="Sales", site=5, lwd=15)

### Lab
library(MASS)
library(ISLR)

fix(Boston)
names(Boston)

lm.fit <- lm(medv ~ lstat, data = Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)
summary(lm.fit)
confint(lm.fit) # confint is to calculate the confidence interval

predict(lm.fit, data.frame(lstat = (c(5, 10, 15))), 
        interval = "confidence")

plot(medv, lstat)
abline(lm.fit, col = "red")

abline(lm.fit, lwd = 3, col = "red")

plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(2, 2))
plot(lm.fit)

par(mfrow = c(1, 2))

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) # hatvalues returns the 

lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

lm.fit <- lm(medv ~ ., data = Boston)

library(car)
vif(lm.fit)

lm.fit1 <- lm(medv ~. - age, data = Boston)
summary(lm.fit1)
# another way to remove one predictor
lm.fit1 <- update(lm.fit, ~. - age)

summary(lm(medv ~ lstat * age, data = Boston))

lm.fit2 <- lm(medv ~ lstat + I(lstat ^ 2)) # how to regress on quadratic term
summary(lm.fit2)

lm.fit <- lm(medv ~ lstat)
anova(lm.fit, lm.fit2)

par(mfrow = c(2, 2))
plot(lm.fit2)