# Import Library
library(ISLR2)se

df <- Auto

set.seed(1)

dim(df)

?Auto

train <- sample(392, 196)
print(train)
t1 <- c(1, 2, 3)
print(t1)
print(df[-t1, ])

# mpg = b0 + b1*horsepower
lm.fit <- lm(mpg ~ horsepower, data=df, subset=train)

summary(lm.fit)

# get MSE
mean((df$mpg - predict(lm.fit, df))[-train]^2)

# Predict entire df
lm.pred <- predict(lm.fit, df)
# residuals
lm.res <- df$mpg - lm.pred
# Keep only test residuals
lm.testr <- lm.res[-train]
# R-squared
lm.r2 <- lm.testr^2
# MSE
lm.mse <- mean(lm.r2)
print(lm.mse)

mse <- function(model, df, train) {
  # Predict entire df
  lm.pred <- predict(model, df)
  # residuals
  lm.res <- df$mpg - lm.pred
  # Keep only test residuals
  lm.testr <- lm.res[-train]
  # R-squared
  lm.r2 <- lm.testr^2
  # MSE
  return(mean(lm.r2))
}

# mpg = b0 + b1*horsepower + b2*horsepower^2
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data=df, subset=train)
lm.mse2 <- mse(lm.fit2, df, train)
print(lm.mse2)

# mpg ~ poly(horsepower, 3)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data=df, subset=train)
lm.mse3 <- mse(lm.fit3, df, train)
print(lm.mse3)

## --- Seed: 2 ---
set.seed(2)
train <- sample(392, 196)

# mpg = b0 + b1*horsepower
lm.fit <- lm(mpg ~ horsepower, data=df, subset=train)
lm.mse <- mse(lm.fit, df, train)
print(lm.mse)

# mpg = b0 + b1*horsepower + b2*horsepower^2
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data=df, subset=train)
lm.mse2 <- mse(lm.fit2, df, train)
print(lm.mse2)

# mpg ~ poly(horsepower, 3)
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data=df, subset=train)
lm.mse3 <- mse(lm.fit3, df, train)
print(lm.mse3)


# -- LOOCV --
glm.fit <- glm(mpg ~ horsepower, data=df)
coef(glm.fit)

lm.fit <- lm(mpg ~ horsepower, data=df)
coef(lm.fit)

# import library: boot
library(boot)

glm.fit <- glm(mpg ~ horsepower, data=df)
cv.err <- cv.glm(df, glm.fit)
print(cv.err$delta)

cv.err <- rep(0, 10)

for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data=df)
  cv.err[i] <- cv.glm(df, glm.fit)$delta[1]
}

print(cv.err)
plot(cv.err)

# k-Fold CV
set.seed(17)

cv.err.10 <- rep(0, 10)

for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data=df)
  cv.err.10[i] <- cv.glm(df, glm.fit, K=10)$delta[1]
}

print(cv.err.10)
plot(cv.err.10, cv.err)

# --- Q&A --- #
names(df)

sum <- 0

for (col in names(df[-9])) {
  print(mean(df[[col]]))
}

names(Auto)

l <- c('mpg', 'horsepower')
for (i in l) {
  print(mean(df[, i]))
}

cat("df$", "mpg", sep = '')
