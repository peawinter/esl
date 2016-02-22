### Chapter 7 ###
### Non-linear Modeling

library(ISLR)
attach(Wage)

fit <- lm(wage ~ poly(age, 4), data = Wage)
coef(fit)
plot(fit)

fit2b <- lm(wage ~ cbind(age, age ^ 2, age ^ 3, age ^ 4), data = Wage)

agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit, newdata = list(age = age.grid), se = T)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, 
                  preds$fit - 2 * preds$se.fit)

par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)
preds2 <- predict(fit2b, newdata = list(age = age.grid), se = T)
max(abs(preds$fit - preds2$fit))

fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5)

## logistic regression

fit <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)

preds <- predict(fit, newdata = list(age = age.grid), se = T)
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit, 
                        preds$fit - 2 * preds$se.fit)

preds <- predict(fit, newdata = list(age = age.grid), type = "response",
                 se = T)

plot(age, I(wage > 250), xlim = agelims, 
     type = "n", ylim = c(0, 0.2))
points(jitter(age), I((wage > 250)/5), cex = .5, pch = "|",
       col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")

matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

table(cut(age, 4))
fit <- lm(wage ~ cut(age, 4), data = Wage)
coef(summary(fit))

# splines

library(splines)
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, col = "grey")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se, lty = "dashed")

dim(bs(age, knots = c(25, 40, 60)))

fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("smoothing spline")
fit <- smooth.spline(age, wage, df = 16)
fit2 <- smooth.spline(age, wage, cv = T)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"))

gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
library(gam)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
par(mfrow = c(1, 3))
plot(gam.m3, se = T, col = "blue")
plot(gam1, se = T, col = "red")
par(mfrow = c(1, 1))
