##  test_file("tests/testthat/test_ols_fit.R")
library(LinearRegression)

X = as.matrix(cbind(1, iris$Sepal.Width, iris$Petal.Width))
colnames(X) = c("(Intercept)", "Sepal.Width", "Petal.Width")
y = iris$Sepal.Length

fitted_ols = ols_fit(X, y)
fitted_lm = lm(Sepal.Length~Sepal.Width + Petal.Width, data = iris)
summary_lm = summary(fitted_lm)

# coefficients beta hat test
test_that("OLS coefficients are computed correctly", {
  lm_coef = as.numeric(coef(fitted_lm))
  ols_coef = as.numeric(fitted_ols$coefficients)

  expect_equal(ols_coef, lm_coef, tolerance = 1e-6)
})

# Residual and RSS tests
test_that("OLS residuals and RSS are computed correctly", {
  lm_residuals <- as.numeric(residuals(fitted_lm))
  lm_rss <- sum(lm_residuals^2)
  ols_residuals =  as.numeric(fitted_ols$residuals)
  ols_rss = fitted_ols$rss

  expect_equal(ols_residuals, lm_residuals, tolerance = 1e-6)
  expect_equal(ols_rss, lm_rss, tolerance = 1e-6)
})

# standard error test
test_that("OLS standard errors are computed correctly", {
  lm_stderr = as.numeric(summary_lm$coefficients[, "Std. Error"])
  ols_stderr = as.numeric(fitted_ols$std_error)
  expect_equal(ols_stderr, lm_stderr, tolerance = 1e-6)
})


# Predicted value test
test_that("OLS fitted values are computed correctly", {
  lm_yhat = as.numeric(fitted(fitted_lm))
  ols_yhat = as.numeric(fitted_ols$y_fitted)
  expect_equal(ols_yhat, lm_yhat, tolerance = 1e-6)
})

# sigma value test
test_that("OLS sigma value is computed correctly", {
  lm_sigma = summary_lm$sigma
  ols_sigma = fitted_ols$sigmahat_cor
  expect_equal(ols_sigma, lm_sigma, tolerance = 1e-6)
})

# r squared value test
test_that("OLS r squared value is computed correctly", {
  lm_rsqd = summary(fitted_lm)$r.squared
  ols_rsqd = fitted_ols$r_squared
  expect_equal(ols_rsqd, lm_rsqd, tolerance = 1e-6)
})

# conf ints test
test_that("OLS's confidence intervals are computed correctly", {
  lm_confint = as.numeric(confint(fitted_lm))
  confints_table = coef_inference(fitted_ols)
  ols_lower = as.numeric(confints_table[, 2])
  ols_upper = as.numeric(confints_table[, 3])
  ols_confint = c(ols_lower,ols_upper)

  expect_equal(ols_confint, lm_confint, tolerance = 1e-6)
})

# p value test
test_that("OLS's p values are computed correctly", {
  lm_pval <- as.numeric(coef(summary_lm)[, "Pr(>|t|)"])
  confints_table = coef_inference(fitted_ols)
  ols_pval = as.numeric(confints_table[, 4])

  expect_equal(ols_pval, lm_pval, tolerance = 1e-6)
})


