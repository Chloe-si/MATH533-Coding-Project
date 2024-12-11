##  test_file("tests/testthat/test_predict.R")
library(LinearRegression)

X = as.matrix(cbind(1, iris$Sepal.Width, iris$Petal.Length))
colnames(X) = c("(Intercept)", "Sepal.Width", "Petal.Length")
y = iris$Sepal.Length

fitted_ols = ols_fit(X, y)
fitted_lm = lm(Sepal.Length~Sepal.Width + Petal.Length, data = iris)

new_data <- data.frame(Sepal.Width = c(3.5, 3.0), Petal.Length = c(4.5, 5.5))
newdata_matrix <- cbind(1, new_data$Sepal.Width, new_data$Petal.Length)


# predicted y test
test_that("predicted values are computed correctly", {
  ols_predictions <- as.numeric(ols_predict(fitted_ols, newdata_matrix))
  lm_predictions <- as.numeric(predict(fitted_lm, newdata = new_data))

  expect_equal(ols_predictions, lm_predictions, tolerance = 1e-6)
})


# predicted confidence interval test
test_that("predicted confidence intervals are computed correctly", {
  ols_ci <- ols_predict(fitted_ols, newdata_matrix, interval = "confidence", level = 0.95)
  lm_ci <- predict(fitted_lm, newdata = new_data, interval = "confidence", level = 0.95)
  ols_ci_lwr <- as.numeric(ols_ci[, 2])
  ols_ci_upr <- as.numeric(ols_ci[, 3])
  lm_ci_lwr <- as.numeric(lm_ci[, 2])
  lm_ci_upr <- as.numeric(lm_ci[, 3])

  expect_equal(c(ols_ci_lwr, ols_ci_upr), c(lm_ci_lwr, lm_ci_upr), tolerance = 1e-6)
})

