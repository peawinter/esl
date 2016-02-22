### Chapter 2 ###
### 

### variance vs bias
### knn classifier

library(ElemStatLearn)
require(class)
x <- mixture.example$x
g <- mixture.example$y
xnew <- mixture.example$xnew
mod15 <- knn(x, xnew, g, k=15, prob=TRUE)
prob <- attr(mod15, "prob")
prob <- ifelse(mod15=="1", prob, 1-prob)
px1 <- mixture.example$px1
px2 <- mixture.example$px2
prob15 <- matrix(prob, length(px1), length(px2))
par(mar=rep(2,4))
contour(px1, px2, prob15, levels=0.5, labels="", xlab="", ylab="", main=
          "15-nearest neighbour", axes=FALSE)
points(x, col=ifelse(g==1, "coral", "cornflowerblue"))
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
box()

### lab
rm(list = ls())
x <- 1:10
y <- x
f <- outer(x, y, function(x,y) cos(y) / (1 + x ^ 2))
contour(x, y, f)

library(ISLR)
data(Auto)
fix(Auto)
dim(Auto)
head(Auto)
attach(Auto)

Auto <- na.omit(Auto)
dim(Auto)
names(Auto)
cylinders <- as.factor(cylinders)
plot(cylinders, mpg, col = "red", varwidth = T, xlab = "cylinders",
     ylab = "MPG")
hist(mpg, col = "red", breaks = 15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
identify(horsepower, mpg, name)

### assignment 

## 10
library(MASS)
data(Boston)
summary(Boston)
dim(Boston)
names(Boston)
pairs(Boston)
pairs(~)