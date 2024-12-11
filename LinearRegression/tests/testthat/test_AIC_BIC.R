#  test_file("tests/testthat/test_AIC_BIC.R")
library(LinearRegression)


# Fit candidate models
X1 = as.matrix(cbind(1, iris$Sepal.Width, iris$Petal.Length))
X2 = as.matrix(cbind(1, iris$Sepal.Width, iris$Petal.Width))
X3 = as.matrix(cbind(1, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width))
y = iris$Sepal.Length
ols_model1 = ols_fit(X1, y)
ols_model2 = ols_fit(X2, y)
ols_model3 = ols_fit(X3, y)
ols_models <- list(ols_model1, ols_model2, ols_model3)

lm_model1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris)
lm_model2 <- lm(Sepal.Length ~ Sepal.Width + Petal.Width, data = iris)
lm_model3 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
lm_models <- list(lm_model1, lm_model2, lm_model3)


# Test that both implementations select the same model
test_that("AIC/BIC select the correct model", {
  lm_aic <- sapply(lm_models, AIC)
  lm_bic <- sapply(lm_models, BIC)
  lm_aic_best <- which.min(lm_aic)
  lm_bic_best <- which.min(lm_bic)

  ols_aic_bic <- sapply(ols_models, AIC_BIC)
  ols_aic = as.numeric(ols_aic_bic[1,])
  ols_bic = as.numeric(ols_aic_bic[2,])
  ols_aic_best = which.min(ols_aic)
  ols_bic_best = which.min(ols_bic)

  expect_equal(ols_aic_best, lm_aic_best)
  expect_equal(ols_bic_best, lm_bic_best)
})


