# File: tests/testthat/test_ridge_estimator.R
library(LinearRegression)

X_values <- as.matrix(cbind(1, iris$Sepal.Width, iris$Petal.Width))
colnames(X_values) <- c("Intercept", "Sepal.Width", "Petal.Width") 
y_values <- iris$Sepal.Length
lambda <- 0.1

test_that("ridge_estimator works correctly", {
  
  # Test valid case
  ridge_result <- ridge_estimator(X_values, y_values, lambda)
  expect_type(ridge_result, "double") 
  
  # Test invalid cases
  expect_error(ridge_estimator(data.frame(X_values), y_values, lambda), "Error: X must be a matrix.")
  expect_error(ridge_estimator(X_values, cbind(y_values), lambda), "Error: y must be a vector.")
  expect_error(ridge_estimator(X_values, y_values, -0.1), "Error: Either lambda must be greater than zero or the design matrix X must be full rank.")
})

test_that("ridge_estimator reduces to OLS when lambda = 0", {
  # OLS test with lambda = 0
  ridge_result <- ridge_estimator(X_values, y_values, lambda = 0)
  ols_result <- solve(t(X_values) %*% X_values) %*% t(X_values) %*% y_values
  expect_equal(ridge_result, ols_result, tolerance = 1e-6)
})
