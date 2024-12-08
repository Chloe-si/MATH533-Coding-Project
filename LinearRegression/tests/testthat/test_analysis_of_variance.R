# File: tests/testthat/test-analysis_of_variance.R
library(LinearRegression)

#Example uses the iris dataset
#response is Species, covariate is Sepal.Length

test_that("analysis_of_variance calculates correctly", {
  result <- analysis_of_variance(iris, Sepal.Length, Species)
  fit <- lm(Sepal.Length ~ Species, data = iris)
  anova_result <- anova(fit)

  expect_equal(result$TSS, sum((iris$Sepal.Length - mean(iris$Sepal.Length))^2))
  expect_equal(result$p_value, anova_result$`Pr(>F)`[1])
})
