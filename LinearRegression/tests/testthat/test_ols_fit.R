##Tests for ols estimation
library(LinearRegression)
#context("OLS Estimation Tests")

## Example data
X <- matrix(c(1, 1, 1, 1, 2, 3, 4, 5), ncol = 2)
y <- c(1, 2, 3, 4)

ols_result <- ols_fit(X, y)

## Basic tests
test_that("OLS result is a list with correct components", {
  expect_type(ols_result, "list")
  expect_named(ols_result, c("coefficients", "fitted_values", "residuals", "rss"))
})

test_that("OLS coefficients are computed correctly", {
  expected_beta <- solve(t(X) %*% X) %*% t(X) %*% y
  expect_equal(ols_result$coefficients, expected_beta)
})

## Residual and RSS tests
test_that("OLS residuals and RSS are computed correctly", {
  y_hat <- X %*% ols_result$coefficients
  residuals <- y - y_hat
  rss <- sum(residuals^2)

  expect_equal(ols_result$residuals, residuals)
  expect_equal(ols_result$rss, rss)
})

## Predictive tests
test_that("OLS fitted values are computed correctly", {
  y_hat <- X %*% ols_result$coefficients
  expect_equal(ols_result$fitted_values, y_hat)
})

## Edge cases
test_that("OLS fit works for simple cases like a single predictor", {
  X_simple <- cbind(1, c(1, 2, 3, 4)) # Design matrix for simple linear regression
  y_simple <- c(2, 4, 6, 8)           # y = 2 * x
  result <- ols_fit(X_simple, y_simple)

  expect_equal(result$coefficients[2], 2) # Slope
  expect_equal(result$coefficients[1], 0) # Intercept
})

test_that("OLS raises an error for mismatched dimensions", {
  expect_error(ols_fit(X, y[1:3]), "Number of rows in X must match length of y.")
})

test_that("OLS raises an error if X is not a matrix", {
  expect_error(ols_fit(data.frame(X), y), "X must be a matrix.")
})

## Large data test
test_that("OLS works for larger datasets", {
  set.seed(123)
  n <- 100
  p <- 5
  X_large <- cbind(1, matrix(rnorm(n * p), ncol = p))
  beta <- rnorm(p + 1)
  y_large <- X_large %*% beta + rnorm(n, sd = 0.5)

  result <- ols_fit(X_large, y_large)
  expect_equal(length(result$coefficients), p + 1)
  expect_true(result$rss >= 0)
})
