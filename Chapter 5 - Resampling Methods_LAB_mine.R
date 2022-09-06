#LAB of Chapter 5
library(ISLR2)

df <- Auto
set.seed(1)
dim(df)
names(df)
# subset
## get sample indices  we get randomly 196 indices from 392 samples
train <- sample(392, 196)

lm.fit <- lm(mpg ~ horsepower, data=df, subset = train)
summary(lm.fit)
#get mse
mean((df$mpg - predict(lm.fit, df))[-train]^2)
#predict
lm.pred <-predict(lm.fit, df)
#residuals of all data
lm.res <- df$mpg - lm.pred
# keep the test residuals
lm.testres <- lm.res[-train]
# R-squared
lm.r2 <- lm.testres^2
# MSE
lm.mse <- mean(lm.r2)

#function for mse
mse <- function(model, data, train){
  #predict
  lm.pred <-predict(model, data)
  #residuals of all data
  lm.res <- data$mpg - lm.pred
  # keep the test residuals
  lm.testres <- lm.res[-train]
  # R-squared
  lm.r2 <- lm.testres^2
  # MSE
  return(mean(lm.r2))
  }

#polynomial 2
lm.fit2 <- lm(mpg ~ poly(horsepower,2), data=df, subset = train)
lm.mse2 <- mse(lm.fit2, df, train)
print(lm.mse2)
#polynomial 3
lm.fit3 <- lm(mpg ~ poly(horsepower,3), data=df, subset = train)
lm.mse3 <- mse(lm.fit3, df, train)
print(lm.mse3)

# ------- set seed : 2 ------
set.seed(2)
train <- sample(392, 196)
#linear
lm.fit <- lm(mpg ~ horsepower, data=df, subset = train)
lm.mse <- mse(lm.fit, df, train)
print(lm.mse)
#poly 2
lm.fit2 <- lm(mpg ~ poly(horsepower,2), data=df, subset = train)
lm.mse2 <- mse(lm.fit2, df, train)
print(lm.mse2)
#poly 3
lm.fit3 <- lm(mpg ~ poly(horsepower,3), data=df, subset = train)
lm.mse3 <- mse(lm.fit3, df, train)
print(lm.mse3)

###############################################################
# ------- LOOCV --------

# to glm mporei na ginei lm allazontas to family. (family=binomial einai logistic regression)
glm.fit <- glm(mpg ~ horsepower, data=df)
coef(glm.fit)

# import library: boot
library(boot)

glm.fit <- glm(mpg ~ horsepower, data=df)
cv.error <- cv.glm(df,glm.fit)
# pare to cv error.   to ena einai ligo pio kanonikopoihmeno. mas noiazei to 1o
print(cv.error$delta)

# ----- train LOOCV 10 times in a loop ------

cv.error <- rep(0,10) #kane repeat to 0 10 fores
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data=df)
  cv.error[i] <- cv.glm(df, glm.fit)$delta[1]
}
print(cv.error)
print(mean(cv.error))
print(sd(cv.error))
plot(cv.error)

######################################################################
# ------------ k-Fold CV     ---------------------------------
set.seed(17)

cv.error.10 <- rep(0,10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data=df)
  cv.error.10[i] <- cv.glm(df, glm.fit, K=10)$delta[1]
}
print(cv.error.10)
print(mean(cv.error.10))
print(sd(cv.error.10))
plot(cv.error.10)


plot(cv.error.10, cv.error)