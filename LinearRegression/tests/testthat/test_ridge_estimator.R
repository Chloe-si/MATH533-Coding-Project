# File: tests/testthat/test_ridge_estimator.R
library(LinearRegression)

test_that("ridge_estimator works correctly", {
  set.seed(123)
  X <- matrix(rnorm(100), nrow = 10, ncol = 10)
  y <- rnorm(10)
  lambda <- 0.1
  
  # Test valid case
  ridge_result <- ridge_estimator(X, y, lambda)
  expect_type(ridge_result, "double") # Check the output is a numeric type
  
  # Test invalid cases
  expect_error(ridge_estimator(data.frame(X), y, lambda), "Error: X must be a matrix.")
  expect_error(ridge_estimator(X, cbind(y), lambda), "Error: y must be a vector.")
  expect_error(ridge_estimator(X, y, -0.1), "Error: Either lambda must be greater than zero or the design matrix X must be full rank.")
})
