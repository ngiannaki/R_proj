library(MASS)

# Assign Boston to df
df <- Boston

# Get the column names
names(df)

# Visualize the data lstat vs medv
plot(df$lstat, df$medv, xlab="Lower Status %", ylab="Median Value in $1000")

# Create the linear model
lm.fit <- lm(medv ~ lstat, data=df)

# Get some statistical information about the model
summary(lm.fit)

# get the coefficients computed (b0, b1)
coef(lm.fit)

# Get the confidence intervals (95%)
confint(lm.fit, level=0.95)

# Generate test data: lstat: 5, 10, 15
test_data <- data.frame(lstat=c(5, 10, 15))
print(test_data)

# use the model to predict medv for the test_data
predict(lm.fit, test_data, interval = "confidence")
predict(lm.fit, test_data, interval = "predict")
predict(lm.fit, test_data)

# Visualize the model
plot(df$lstat, df$medv)
abline(lm.fit, col='red')

# Residual plots
par(mfrow=c(2,2))
plot(lm.fit)

# Alternatively
par(mfrow=c(1,2))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# Leverage Statistics
par(mfrow=c(1,1))
hvals <- hatvalues(lm.fit)
maxIndex <- which.max(hvals)
plot(hvals)
points(maxIndex, hvals[maxIndex], col="red")


# --- Multiple Linear Regression --- #
lm.fit <- lm(medv ~ lstat + age, data=df)
summary(lm.fit)

# use all the predictors
lm.fit <- lm(medv ~ . , data=df)
summary(lm.fit)

# remove indus, age
lm.fit <- lm(medv ~ . - age - indus , data=df)
summary(lm.fit)

# Update a model
lm.fit1 <- update(lm.fit, ~ . - age - indus)
s <- summary(lm.fit1)

# Get values from summary
s$sigma
s$r.squared
?summary.lm

# -- Interaction Terms -- 
lm.fit2 <- lm(medv ~ lstat*age, data=df)
summary(lm.fit2)


# -- Non-linear Transformation --
lm.fit3 <- lm(medv ~ I(lstat^2), data=df)
summary(lm.fit3)

# ANOVA
lm.fit0 <- lm(medv ~ lstat, data=df)
anova(lm.fit0, lm.fit3)

# Polynomial
lm.fit4 <- lm(medv ~ poly(lstat, 7), data=df)
summary(lm.fit4)

# Log
lm.fit5 <- lm(medv ~ log(lstat), data=df)
summary(lm.fit5)

# Qualitative Predictors
library(ISLR2)

df1 <- Carseats
head(df1)
tail(df1)

lm.fitCat <- lm(Sales ~ . + Income:Advertising + Price:Age, data=df1)
summary(lm.fitCat)
contrasts(df1$ShelveLoc)

# Writing Function
LoadLibraries <- function() {
  library(ISLR2)
  library(MASS)
  print("The libraries have been loaded...")
}

LoadLibraries()
