# File: tests/testthat/test-analysis_of_variance.R
library(LinearRegression)

#Example uses the iris dataset
#response is Species, covariate is Sepal.Length

test_that("analysis_of_variance calculates correctly", {
  result <- analysis_of_variance(iris, response = "Sepal.Length", covariate = "Species")
  expect_true(!is.null(result))
  expect_named(result$Summary_Table, c("Effect", "DF", "SS", "MS", "F_stat", "p_value"))
  expect_true(result$F_statistic > 0)
  expect_true(result$p_value >= 0 & result$p_value <= 1)
})
