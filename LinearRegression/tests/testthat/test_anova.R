#  test_file("tests/testthat/test_anova.R")
library(LinearRegression)

test_that("anova_ols produces correct results when p > 1", {
  X = as.matrix(cbind(1, iris$Sepal.Width, iris$Petal.Width))
  colnames(X) = c("(Intercept)", "Sepal.Width", "Petal.Width")
  y = iris$Sepal.Length

  # Fit the OLS model
  fitted_ols = ols_fit(X, y)

  # Generate ANOVA table using anova_ols
  ols_anova_result = anova_ols(fitted_ols)

  # Generate ANOVA table using aov for comparison
  fitted_lm = lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)
  aov_result = summary(aov(fitted_lm))

  # Test the df, sum of squares, mean squares, F stats and p value.
  expect_equal(ols_anova_result$`Df`, aov_result[[1]]$Df)
  expect_equal(round(ols_anova_result$`Sum_Sq`, 4), round(aov_result[[1]]$`Sum Sq`, 4))
  expect_equal(round(ols_anova_result$`Mean_Sq`, 4), round(aov_result[[1]]$`Mean Sq`, 4))
  expect_equal(round(ols_anova_result$`F_Stat`, 4), round(aov_result[[1]]$`F value`, 4))
  expect_equal(round(ols_anova_result$`P_Val`, 4), round(aov_result[[1]]$`Pr(>F)`, 4))
})


test_that("anova_ols_cat produces correct results for categorical covariate", {

  # Generate ANOVA table using anova_ols_cat
  ols_anova_result = anova_ols_cat(iris, "Sepal.Length", "Species")

  # Generate ANOVA table using aov for comparison
  fitted_lm = lm(Sepal.Length ~ Species, data = iris)
  aov_result = summary(aov(fitted_lm))

  # Test the df, sum of squares, mean squares, F stats and p value.
  expect_equal(ols_anova_result$`Df`, aov_result[[1]]$Df)
  expect_equal(round(ols_anova_result$`Sum_Sq`, 4), round(aov_result[[1]]$`Sum Sq`, 4))
  expect_equal(round(ols_anova_result$`Mean_Sq`, 4), round(aov_result[[1]]$`Mean Sq`, 4))
  expect_equal(round(ols_anova_result$`F_Stat`, 4), round(aov_result[[1]]$`F value`, 4))
  expect_equal(round(ols_anova_result$`P_Val`, 4), round(aov_result[[1]]$`Pr(>F)`, 4))
})
