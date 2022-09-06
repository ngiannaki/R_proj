# 1. Subset Selection Methods
library(ISLR2)

df <- Hitters
?Hitters

# Columns
names(df)

# Dimensions
dim(df)

# Check for missing values
is.na(df$Salary)

# Check if any available missing value
any(is.na(df$Salary))

t <- c(TRUE, FALSE, TRUE)
sum(t)
head(df)

# Count missing values
sum(is.na(df$Salary))

for (col in names(df)) {
  nas <- sum(is.na(df[[col]]))
  if (nas > 0) {
    print( cat("Column: ", col, " has ", nas, " NAs"))
  }
}

summary(df)

df_clean <- na.omit(df)
dim(df_clean)

summary(df_clean)
sum(is.na(df_clean$Salary))

?complete.cases

# 1.1. Selecting the best model using RSS, R2, Cp and BIC
library(leaps)

# Default: 8-variables
regfit.full <- regsubsets(Salary ~ ., df)
summary(regfit.full)

# Use 19 variables
regfit.full <- regsubsets(Salary ~ ., df, nvmax = 19)
reg.summary <- summary(regfit.full)

# Summary details
names(reg.summary)

print(reg.summary$rss)

# Plot RSS
plot(reg.summary$rss, xlab="Number of Variables ", ylab="RSS", type="l")
# Plot Adj R2
plot(reg.summary$adjr2, xlab="Number of Variables ", ylab="Adjusted Rsq", type="l")
# Find index of the maximum value
max_idx <- which.max(reg.summary$adjr2)
# Add a point on the last plot to highlight the max value
points(max_idx, reg.summary$adjr2[max_idx], col="red", cex=2, pch=20)
print(max_idx)
# max(reg.summary$adjr2) == reg.summary$adjr2[max_idx]  # Returns TRUE

# 1.2. Forward and Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary ~ ., df, nvmax = 19, method = "forward")
summary(regfit.fwd)

df1 <- df_clean[,-14][,-14][,-18]
head(df1)
cor(df1)

regfit.bwd <- regsubsets(Salary ~ ., df, nvmax = 19, method = "backward")
summary(regfit.bwd)

# Coefficient comparison
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)


# 1.3. Selecting the best model using Validation Set and Cross-Validation
# 1.3.1 Validation Set
df <- df_clean
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(df), rep=TRUE)
test <- !train

# Fit
regfit.best <- regsubsets(Salary ~ ., df[train,], nvmax=19)
test.mat <- model.matrix(Salary ~ ., df[test,])

var.errors <- rep(NA, 19)

for (i in 1:19) {
  coef_i <- coef(regfit.best, i)
  keep_columns <- names(coef_i)
  pred <- test.mat[, keep_columns] %*% coef_i
  var.errors[i] <- mean((df$Salary[test] - pred)^2)
}

# Best Fit plot
plot(var.errors, xlab="Number of Variables", ylab="MSE", type="l")
# Find best point
min_idx <- which.min(var.errors)
# Plot best point
points(min_idx, var.errors[min_idx], col="red", cex=2, pch=20)
print(min_idx)
# points(min(var.errors), col="green", cex=2, pch=20)

# !Important! Now that we know that the 7-variable model is the best
#             We train our model again, using the entire data set.
regfit.best <- regsubsets(Salary ~ ., df, nvmax=7)
coef.best <- coef(regfit.best, 7)
# The coefficients of the best model
print(coef.best)

# 1.3.2 Cross Validation
set.seed(1)

k <- 10  # Number of folds

folds <- sample(1:k, nrow(df), rep=T)

print(folds)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

print(cv.errors)

predict.regsubsets <- function(object, newdata, id, ...) {
  formula <- as.formula(object$call[[2]])  # From the model, get the training formula
  mat <- model.matrix(formula, newdata) # Create X-matrix
  coef_i <- coef(object, id)  # Get the coefficients from the model for the id-model
  keep_columns <- names(coef_i)  # Keep only the columns that are in coef_i
  mat[, keep_columns] %*% coef_i  # Do the matrix multiplication
}

for (fold in 1:k) {
  best.fit <- regsubsets(Salary ~ ., df[folds!=fold,], nvmax = 19)
  for (i in 1:19) {
    pred <- predict(best.fit, df[folds==fold,], i)
    cv.errors[fold, i] <- mean((df$Salary[folds==fold] - pred)^2)
  }
}

print(cv.errors)

mean.cv.errors <- apply(cv.errors, 2, mean)

print(mean.cv.errors)

plot(mean.cv.errors, type='b', xlab="Number of Variables ", ylab="Mean MSE")
min_idx <- which.min(mean.cv.errors)
points(min_idx, mean.cv.errors[min_idx], col="red", cex=2, pch=20)

regfit.best <- regsubsets(Salary ~ ., df, nvmax = 19)
coef(regfit.best, 10)

# 2. Ridge Regression and Lasso
library(glmnet)

x <- model.matrix(Salary ~ ., df)
x <- x[, -1]
y <- df$Salary

# 2.1. Ridge Regression RSS + l*Sum(beta_j^2)
grid <- 10^seq(-2, 10, length=100)

ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge.mod))

lambda <- ridge.mod$lambda[50]
print(lambda)

coef(ridge.mod)[, 50]

lambda <- ridge.mod$lambda[60]
print(lambda)

coef(ridge.mod)[, 60]

# predict for lambda = 50
predict(ridge.mod, s=50, type = "coefficients")[1:20, ]

# Cross Validation Check
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- -train

y.test <- y[test]

# train
ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
# predict
ridge.pred <- predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred - y.test)^2)  # MSE

# Default: 10-fold cross validation
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha=0)
plot(cv.out)

best_lambda <- cv.out$lambda.min
print(best_lambda)
print(log(best_lambda))

# Predict using the best lambda
ridge.pred <- predict(ridge.mod, s=best_lambda, newx=x[test,])
mean((ridge.pred - y.test)^2)

# 2.2. Lasso RSS + l*Sum(|beta_j|)
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)
# Cross-validation
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha=1)
plot(cv.out)

# Best lambda
best_lambda <- cv.out$lambda.min
print(best_lambda)
print(log(best_lambda))

# Train full model with best lambda
out <- glmnet(x, y, alpha=1, lambda=grid)  # full model
lasso.coef <- predict(out, type="coefficients", s=best_lambda)[1:20,]
print(lasso.coef)

# 3. PCR and PLS
library(pls)
# 3.1. Principal Component Regression
set.seed(1)

pcr.fit <- pcr(Salary ~ ., data=df, scale=TRUE, validation="CV")
summary(pcr.fit)

# Validation plot
validationplot(pcr.fit, val.type="MSEP")

# Training Data
set.seed(1)
pcr.fit <- pcr(Salary ~ ., data=df, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

# Test Data
pcr.pred <- predict(pcr.fit, x[test, ], ncomp=5)
mean((pcr.pred - y.test)^2)

# 3.2. Partial Least Squares
set.seed(2)
pls.fit <- plsr(Salary ~ ., data=df, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)

# Validation Plot
validationplot(pls.fit, val.type="MSEP")

# M = 3
pls.pred <- predict(pls.fit, x[test, ], ncomp=3)
mean((pls.pred - y.test)^2)

# The full data
pls.fit <- plsr(Salary ~ ., data=df, scale=TRUE, validation="CV", ncomp=3)
summary(pls.fit)
