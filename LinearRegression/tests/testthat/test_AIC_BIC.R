# File: tests/testthat/test_AIC_BIC.R
library(LinearRegression)

#Example uses the iris dataset, model is Sepal.Length ~ Sepal.Width

# test_that("AIC_BIC works correctly", {
#   model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#   result <- AIC_BIC(model)
#
#   expect_named(result, c("AIC", "BIC"))
#   expect_true(is.numeric(result$AIC))
#   expect_true(is.numeric(result$BIC))
# })




test_that("AIC and BIC are calculated correctly using ols_fit", {
  # Prepare the iris dataset
  data(iris)
  X <- as.matrix(cbind(1, iris$Sepal.Width, iris$Petal.Length))  # Design matrix with intercept
  y <- iris$Sepal.Length  # Response variable

  # Fit the model using ols_fit
  fitted_ols <- ols_fit(X, y)

  # Fit the model using lm for comparison
  fitted_lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)

  # Calculate AIC and BIC using the custom function
  aic_bic_ols <- AIC_BIC(fitted_ols)

  # Calculate AIC and BIC using R's built-in functions
  aic_lm <- AIC(fitted_lm)
  bic_lm <- BIC(fitted_lm)

  # Compare AIC values
  expect_equal(
    round(aic_bic_ols$AIC, 4),
    round(aic_lm, 4),
    tolerance = 1e-4,
    info = "AIC values do not match between custom and R's lm implementation."
  )

  # Compare BIC values
  expect_equal(
    round(aic_bic_ols$BIC, 4),
    round(bic_lm, 4),
    tolerance = 1e-4,
    info = "BIC values do not match between custom and R's lm implementation."
  )
})
