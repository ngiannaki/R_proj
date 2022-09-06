library(ISLR2)

df <- Wage

# Polynomial Regression
# - Basis
fit.poly <- lm(wage ~ poly(age, 4), data=df)
coef(summary(fit.poly))

# - Polynomial
fit.raw <- lm(wage ~ poly(age, 4, raw=T), data=df)
coef(summary(fit.raw))

# - Alternative
fit.raw.a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=df)
coef(summary(fit.raw.a))

# - Alternative b
fit.raw.b <- lm(wage ~ cbind(age, age^2, age^3, age^4), data=df)
coef(summary(fit.raw.b))

age.limits <- range(df$age)
age.grid <- seq(from=age.limits[1], to=age.limits[2])

preds <- predict(fit.poly, newdata=list(age=age.grid), se=T)
# pred +- 2 x SE
names(preds)
se.band <- cbind(preds$fit - 2*preds$se.fit, preds$fit + 2*preds$se.fit)

# Make the plots
plot(df$age, df$wage, xlim=age.limits,
     xlab="Age", ylab="Wage", cex=0.5, col="darkgrey")
title('Degree-4 Polynomial')
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.band, lwd=1, col="blue", lty=3)

# ANOVA: null hypothesis: A more complex model is not required
fit.1 <- lm(wage ~ age, data=df)
fit.2 <- lm(wage ~ poly(age, 2), data=df)
fit.3 <- lm(wage ~ poly(age, 3), data=df)
fit.4 <- lm(wage ~ poly(age, 4), data=df)
fit.5 <- lm(wage ~ poly(age, 5), data=df)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# Classification Setting
fit.lreg <- glm(I(wage > 250) ~ poly(age, 4), data=df, family=binomial)
log.preds <- predict(fit.lreg, newdata=list(age=age.grid), se=T)
pfit <- exp(log.preds$fit)/(1+exp(log.preds$fit))
se.bands.logit <- cbind(log.preds$fit - 2*log.preds$se.fit, log.preds$fit + 2*log.preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))

# Plot
plot(df$age, I(df$wage > 250), xlim=age.limits, xlab="Age", ylab="Pr(Wage>250|Age)",
     type="n", ylim=c(0, 0.2))
title('Degree-4 Polynomial')
points(jitter(df$age), I((df$wage > 250)/5), cex=0.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")     
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)

# Piecewise Constant
table(cut(df$age, 4))

fit.const <- lm(wage ~ cut(age, 4), data=df)
coef(summary(fit.const))

pred.const <- predict(fit.const, newdata=list(age=age.grid), se=T)
se.bands <- cbind(pred.const$fit - 2*pred.const$se.fit, pred.const$fit + 2*pred.const$se.fit)

#Make the plot
plot(df$age, df$wage, xlim=age.limits, xlab="Age", ylab="Wage", cex=0.5, col="darkgrey")
title("Piecewise Constants")
lines(age.grid, pred.const$fit, lwd=2, col="darkgreen")
matlines(age.grid, se.bands, lwd=1, col="darkgreen", lty=2)

# Classification Setting
fit.lreg <- glm(I(wage > 250) ~ cut(age, 4), data=df, family=binomial)
log.preds <- predict(fit.lreg, newdata=list(age=age.grid), se=T)
pfit <- exp(log.preds$fit)/(1+exp(log.preds$fit))
se.bands.logit <- cbind(log.preds$fit - 2*log.preds$se.fit, log.preds$fit + 2*log.preds$se.fit)
se.bands <- exp(se.bands.logit)/(1+exp(se.bands.logit))

# Plot
plot(df$age, I(df$wage > 250), xlim=age.limits, xlab="Age", ylab="Pr(Wage>250|Age)",
     type="n", ylim=c(0, 0.2))
title('Piecewise Constants')
points(jitter(df$age), I((df$wage > 250)/5), cex=0.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="darkgreen")     
matlines(age.grid, se.bands, lwd=1, col="darkgreen", lty=3)

# Splines
library(splines)

fit <- lm(wage ~ bs(age, knot=c(25, 40, 60)), data=df)
pred <- predict(fit, newdata=list(age=age.grid), se=T)
se.bands <- cbind(pred$fit - 2*pred$se.fit, pred$fit + 2*pred$se.fit)

plot(df$age, df$wage, col="grey", xlab="Age", ylab="Wage")
lines(age.grid, pred$fit, lwd=2, col="blue")     
matlines(age.grid, se.bands, lwd=1, col="blue", lty=2)

lines(c(25, 25), c(0, 350), lty=2, col="black")
lines(c(40, 40), c(0, 350), lty=2, col="black")
lines(c(60, 60), c(0, 350), lty=2, col="black")

# Natural Splines
fit2 <- lm(wage ~ ns(age, knot=c(25, 40, 60)), data=df)
pred2 <- predict(fit2, newdata=list(age=age.grid), se=T)
se.bands2 <- cbind(pred2$fit - 2*pred2$se.fit, pred2$fit + 2*pred2$se.fit)

plot(df$age, df$wage, col="grey", xlab="Age", ylab="Wage")
lines(age.grid, pred$fit, lwd=2, col="blue")     
matlines(age.grid, se.bands, lwd=1, col="blue", lty=2)
lines(age.grid, pred2$fit, lwd=2, col="red")     
matlines(age.grid, se.bands2, lwd=1, col="red", lty=2)

lines(c(25, 25), c(0, 350), lty=2, col="black")
lines(c(40, 40), c(0, 350), lty=2, col="black")
lines(c(60, 60), c(0, 350), lty=2, col="black")

legend("topright", c("Natural Cubic Spline", "Cubic Spline"), col=c("red", "blue"), lty=c(1, 1))

# Smoothing Spline
fit <- smooth.spline(df$age, df$wage, df=16)
fit2 <- smooth.spline(df$age, df$wage, cv=TRUE)

plot(df$age, df$wage, xlim=age.limits, col="grey", xlab="Age", ylab="Wage", cex=0.5)
title("Smoothing Splines")
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)

legend("topright", c("16 DF", "6.8 DF"), col=c("red", "blue"), lty=c(1, 1))

print(fit$df)
print(fit2$df)

# Local Regression
# ?loess
fit <- loess(wage ~ age, span=0.2, data=df)
pred <- predict(fit, data.frame(age=age.grid))

fit2 <- loess(wage ~ age, span=0.5, data=df)
pred2 <- predict(fit2, data.frame(age=age.grid))

plot(df$age, df$wage, xlim=age.limits, col="grey", xlab="Age", ylab="Wage", cex=0.5)
title("Local Regression")
lines(age.grid, pred, col="red", lwd=2)
lines(age.grid, pred2, col="blue", lwd=2)

legend("topright", c("Span=0.2", "Span=0.5"), col=c("red", "blue"), lty=c(1, 1))

?ns

# GAM
library(gam)

# Linear Model
gam.m1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data=df)

# Smoothing Spline
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data=df)

par(mfrow=c(1, 3))
plot(gam.m3, se=T, col="blue")


# Ploting Gam.M1
par(mfrow=c(1, 3))
plot.Gam(gam.m1, se=T, col="red")

