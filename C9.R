### Chapter 9

rm(list = ls())

set.seed(1)

x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(1, 10), rep(-1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (3 - y))

dat <- data.frame(x = x, y = as.factor(y))

### svm package
library(e1071)
svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 10,
              scale = F)
summary(svmfit)

plot(svmfit, dat)

svmfit$index

svmfit <- svm(y ~., data = dat, kernel = "linear", cost = 0.1, 
              scale = F)
plot(svmfit, dat)
svmfit$index

set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10)))

bestmod <- tune.out$best.model
summary(bestmod)

xtest <- matrix(rnorm(200 * 2), ncol = 2)
ytest <- c(rep(1, 100), rep(-1, 100))
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1

dat.test <- data.frame(x = xtest, y = as.factor(ytest))

plot(svmfit, dat.test)

ypred <- predict(bestmod, dat.test)

table(predict = ypred, truth = dat.test$y)

rm(list = ls())

set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1

plot(x, col = (3 - y))

dat <- data.frame(x = x, y = as.factor(y))
library(e1071)

svmfit <- svm(y ~., data = dat, kernal = "linear", cost = 0.1, 
              scale = F)

plot(svmfit, dat)

svmfit$index
summary(svmfit)

set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

summary(tune.out)

bestmod = tune.out$best.model

summary(bestmod)

xtest = matrix(rnorm(20 * 2), ncol = 2)
ytest = sample(c(1, -1), 20, rep = T)
xtest[ytest == 1, ] = xtest[ytest == 1, ] + 1
testdat = data.frame(x = xtest, y = as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)

svmfit <- svm(y ~ ., data = dat, kernel = "linear",
              cost = 0.01, scale = F)

ypred <- predict(svmfit, testdat)

table(predict = ypred, truth = testdat$y)

x[y == 1, ] = x[y == 1, ] + 0.5
plot(x, col = (y + 5)/2, pch = 19)

dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1e+05,
              scale = F)
plot(svmfit, dat)

svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)

set.seed(1)
x = matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] = x[1:100, ] + 2
x[101:150, ] = x[101:150, ] - 2
y = c(rep(1, 150), rep(-1, 50))
dat = data.frame(x = x, y = as.factor(y))
plot(x, col = (3 - y))

train = sample(200, 100)

svmfit <- svm(y ~., data = dat[train, ], kernel = "radial", gamma = 1,
              cost = 1)

plot(svmfit, dat[train, ])
summary(svmfit)

svmfit <- svm(y ~., data = dat[train, ], kernel = "radial",
              gamma = 1, cost = 1e5)
plot(svmfit, dat[train, ])


set.seed(1)
tune.out <- tune(svm, y ~., data = dat[train, ], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                 gamma = c(0.5, 1, 2, 3, 4)))

summary(tune.out)

table(true = dat[-train, "y"], pred = predict(tune.out$best.model,
                                              newx = dat[-train,]))

library(ROCR)
rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

svmfit.opt <- svm(y ~ ., data = dat[train, ], kernel = "radial",
                  gamma = 2, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.opt, dat[train, ], 
                             decision.values = T))$decision.values
par(mfrow = c(1, 2))
rocplot(fitted, dat[train, "y"], main = "training data")
