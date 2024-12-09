# File: tests/testthat/test-analysis_of_variance.R
library(LinearRegression)

#Example uses the iris dataset, model is Sepal.Length ~ Sepal.Width

test_that("AIC_BIC works correctly", {
  model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
  result <- AIC_BIC(model)

  expect_named(result, c("AIC", "BIC"))
  expect_true(is.numeric(result$AIC))
  expect_true(is.numeric(result$BIC))
})



