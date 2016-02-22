### chapter 4

library(ISLR)
library(lattice)

default.df <- data.frame(Default)
head(default.df)
attach(default.df)
xyplot(income ~ balance, groups = default)
boxplot(balance ~ default, xlab = "default", ylab = "balance",
        col = c("blue", "orange"))
xyplot(default ~ balance)
default.lm <- lm(default ~ balance)
abline(default.lm)

default.logit <- glm(default ~ balance, family = "binomial")

## three D plot
require(lattice)
require(mvtnorm)
require(car)
require(scatterplot3d)
# Generate normal distribution surface
x <- seq(from=-4, to=4,by=0.1)
y <- seq(from=-4, to=4,by=0.1)

z <- matrix(dmvnorm(expand.grid(x, y) , sigma = rbind(c(3, 2), c(2, 3))),
            ncol = length(x))
s3d <- scatterplot3d(x, y, seq(min(z), max(z), length = length(x)), type = "n",
                     grid = FALSE, angle = 70)

### lab

library(ISLR)

sm.dt <- data.frame(Smarket)
head(sm.dt)
pairs(sm.dt)
cor(sm.dt)
cor(sm.dt[, -9])
cor(sm.dt[, 2 : 6])

sm.glm <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, 
              data = sm.dt)

sm.predict <- predict(sm.glm, type = "response")

contrasts(sm.dt$Direction)
sm.pred <- rep("Down", length(sm.predict))
sm.pred[sm.predict > 0.5] <- "Up"

table(sm.pred, sm.dt$Dir)
mean(sm.pred ==  sm.dt$Dir)

## training set
attach(sm.dt)
train <- (Year < 2005)
sm.dt.2005 <- sm.dt[!train, ]
dim(sm.dt.2005)

sm.glm.train <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial, 
              data = sm.dt, subset = train)
sm.probs.2005 <- predict(sm.glm.train, sm.dt.2005, type = "response")
sm.pred.2005 <- rep("Down", sum(!train))
sm.pred.2005[sm.probs.2005 > 0.5] <- "Up"
table(sm.pred.2005, sm.dt.2005$Dir)

mean(sm.pred.2005 == sm.dt.2005$Dir)

## only lag1 and lag2

sm.glm.1 <- glm(Direction ~ Lag1 + Lag2, family = binomial, subset = train)

## Linear Discriminant Analysis
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit

lda.pred <- predict(lda.fit, sm.dt.2005)

qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
qda.class <- predict(qda.fit, sm.dt.2005)$class
table(qda.class, sm.dt.2005$Direction)

## knn
library(class)
train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.Direction <- sm.dt$Dir[train]
Direction.2005 <- sm.dt$Dir[!train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(Direction.2005, knn.pred)

## caravan insurance data
dim(Caravan)
attach(Caravan)
summary(Purchase)
standardized.X <- scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])

test <- 1 : 1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(test.Y, knn.pred)
