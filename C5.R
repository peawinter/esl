### chapter 5, cross validation and bootstrap
rm(list = ls())

# validation set
library(ISLR)
attach(Auto)
set.seed(1)
train = sample(392, 196)
# linear fit on training set
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
# mse of validation set
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2)

# quadratic fit on training set
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
# mse of validation set
mean((mpg - predict(lm.fit2, Auto))[-train] ^ 2)

# cubic
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train] ^ 2)

set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2)

## LOOCV
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

cv.err <- cv.glm(Auto, glm.fit)

cv.error <- rep(0, 5)
for (i in 1 : 5){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

## k-fold cv
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1 : 10){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10

## bootstrap
rm(list = ls())
library(ISLR)
library(boot)
attach(Portfolio)

alpha.fn <- function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return ((var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y)))
}

alpha.fn(Portfolio, 1 : 100)

## set seed
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))

boot(Portfolio, alpha.fn, R = 1000)

# bootstrap of linear regression
boot.fn <- function(data, index){
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
boot.fn(Auto, 1:392) # put all data in training set

set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))

boot(Auto, boot.fn, 1000)

boot.fn2 <- function(data, index){
  return(coef(lm(mpg ~ horsepower + I(horsepower ^ 2), data = data, subset = index)))
}
set.seed(1)
boot(Auto, boot.fn2, 1000)