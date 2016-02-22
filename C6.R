### chapter 6 lab ###
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))
library(leaps)
regfit.full <- regsubsets(Salary ~ . , Hitters)
regfit.full <- regsubsets(Salary ~ . , data = Hitters, nvmax = 19)

smry <- summary(regfit.full)
smry$rsq
par(mfrow = c(2, 2))
plot(smry$rss, xlab = "Number of variables", ylab = "RSS", type = "l")
plot(smry$adjr2, xlab = "Number of variables", ylab = "adj RSS",
     type = "l")
which.max(smry$adjr2)
points(11, smry$adjr2[11], col = "red", cex = 2, pch = 20)
plot(smry$cp, xlab = "Number of variables", ylab = "cp", type = "l")
which.min(smry$cp)
points(10, smry$cp[10], col = "red", cex = 2, pch = 20)
which.min(smry$bic)
plot(smry$bic, xlab = "Number of variables", ylab = "BIC",
     type = "l")
points(6, smry$cp[6], col = "red", cex = 2, pch = 20)

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")

regfit.fwd <- regsubsets(Salary ~ ., Hitters, nvmax = 19, 
                         method = "forward")
regfit.bwd <- regsubsets(Salary ~ ., Hitters, nvmax = 19,
                         method = "backward")
coef(regfit.fwd, 7)

### validation set

set.seed(1)
train <- sample(c(T, F), nrow(Hitters), rep = T)
test <- (!train)

regfit.best <- regsubsets(Salary ~ ., data = Hitters[train, ],
                          nvmax = 19)
test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])

val.errors <- rep(NA, 10)
for (i in 1 : 19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred) ^ 2)
}

which.min(val.errors)
coef(regfit.best, 10)

predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)

coef(regfit.best, 10)

k <- 10
set.seed(10)
folds <- sample(1:k, nrow(Hitters), replace = T)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1 : 19)))

for (j in 1 : k){
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds != j ,],
                         nvmax = 19)
  for (i in 1 : 19){
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <- mean((Hitters$Salary[folds == j] - pred) ^ 2)
  }
}

### 10 fold cross validation
k <- 10
set.seed(10)
folds <- sample(1 : k, nrow(Hitters), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1: 19)))

for (j in 1 : k) {
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds != j, ],
                         nvmax = 19)
  for (i in 1 : 19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <- mean((Hitters$Salary[folds == j] - pred) ^ 2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors, type = 'b')

reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)

### ridge regression and lasso

library(glmnet)
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50] ^ 2))

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60] ^ 2))

predict(ridge.mod, s = 50, type = "coefficients")

## validation
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid,
                    thresh = 1e-12)
# MSE when lambda = 4
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((ridge.pred - y.test) ^ 2)
# MSE when lambda = infty
mean((mean(y[train]) - y.test) ^ 2)
# MSE when lambda = 0
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ],
                      exact = T)
mean((ridge.pred - y.test) ^ 2)

lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact  = T, type = "coefficients")[1:20,]

set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test) ^ 2)

out <- glmnet(x, y, alpha = 0)

### lasso

lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])
mean((lasso.pred - y.test) ^ 2)

out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20, ]
lasso.coef[lasso.coef != 0]

### PCR and PLS

library(pls)
set.seed(2)
pcr.fit <- pcr(Salary ~ ., data = Hitters, scale = T,
               validation = "CV")
summary(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")

set.seed(1)
pcr.fit <- pcr(Salary ~ ., data = Hitters, subsets = train, 
               scale = T, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 7)
mean((pcr.pred - y.test) ^ 2)

pcr.fit <- pcr(y ~ x, scale = T, ncomp = 7)
summary(pcr.fit)

## pls
set.seed(1)
pls.fit <- plsr(Salary ~ ., data = Hitters, subset = train,
                scale = T, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
?validationplot

pls.pred <- predict(pls.fit, x[test, ], ncomp = 2)
mean((pls.pred - y.test) ^ 2)

pls.fit <- plsr(Salary ~ ., data = Hitters, scale = T, ncomp = 2)
summary(pls.fit)


#### Problem 8
set.seed(1)
x <- rnorm(100)
epsilon <- rnorm(100)

Beta.true <- as.matrix(c(1, 1, 2, 1))

X <- cbind(rep(1, length(x)), x, x ^ 2, x ^ 3)
colnames(X) <- c("", "X1", "X2", "X3")

y <- X %*% Beta.true + epsilon

best.fit <- regsubsets(x = X, y = y, nvmax = 3, 
                       methods = "exhaustive")