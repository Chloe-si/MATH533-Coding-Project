# TODO: hetero case for ols fit, the sigma hat estimators.


#' Fit an Ordinary Least Squares Model
#'
#' @param X A matrix of predictors (design matrix).
#' @param y A numeric vector of responses.
#' @param homo True forhomoscedasticity assumption, False for hetero
#' @return An object of class "ols" containing:
#'   - `X`: input covariate matrix X
#'   - `coefficients`: Estimated coefficients (beta_hat).
#'   - `y_fitted`: Fitted values (y_hat).
#'   - `residuals`: Residuals (e = y - y_hat).
#'   - `rss`: Residual Sum of Squares (RSS).
#'   - `r_squared`: Coefficient of determination (R^2).
#'   - `adjusted_r_squared`: Adjusted R^2.
#'   - `std_error`: Standard errors of coefficients.
#'   - `hat_matrix`: Hat matrix H
#'   - `leverages`: Diagonal elements of hat matrix H
#'   - `covariance_matrix`: Variance-Covariance matrix of beta_hat given X
#'   - `sigmahat_cor`: Corrected sigma estimator under homoscedasticity assumption
#'   - `sigmahat_naive`: Naive estimator of sigma under homoscedasticity assumption
#' @export
ols_fit = function(X, y, homo = T) {
  if (!is.matrix(X)) stop("X must be a matrix.")
  if (!is.numeric(y)) stop("y must be a numeric vector.")
  if (nrow(X) != length(y)) stop("Number of rows in X must match length of y.")

  n = nrow(X)  # Number of observations
  p = ncol(X)  # Number of covariates

  # Compute beta_hat
  beta_hat = solve(t(X) %*% X) %*% t(X) %*% y

  # Compute fitted values
  y_hat = X %*% beta_hat

  # Compute residuals
  residuals = y - y_hat

  # Residual Sum of Squares (RSS)
  rss = sum(residuals^2)

  # Standard Errors of Coefficients
  if (homo){
    sigmahat_cor = sqrt(rss / (n - p))
    sigmahat_naive = sqrt(rss / n)
    covariance_matrix = sigmahat_cor * solve(t(X) %*% X)
    std_error = sqrt(diag(covariance_matrix))
  }
  else{
    stop("Heteroscedasticity case not implemented yet.")
  }

  # Total Sum of Squares (TSS)
  tss = sum((y - mean(y))^2)

  # R^2 and Adjusted R^2
  r_squared = 1 - (rss / tss)
  adjusted_r_squared = 1 - ((1 - r_squared) * n / (n - p))

  # Hat matrix H and leverages
  hat_matrix = X %*% solve(t(X) %*% X) %*% t(X)
  leverages = diag(hat_matrix)

  # Residual degree of freedom
  df_residual = n - p

  return(
    list(
      X = X,
      coefficients = beta_hat,
      y_fitted = y_hat,
      residuals = residuals,
      rss = rss,
      r_squared = r_squared,
      adjusted_r_squared = adjusted_r_squared,
      std_error = std_error,
      hat_matrix = hat_matrix,
      leverages = leverages,
      covariance_matrix = covariance_matrix,
      sigmahat_cor = sigmahat_cor,
      sigmahat_naive = sigmahat_naive,
      df_residual = df_residual
    )
  )
}

#' Summary for OLS Objects
#'
#' @param model An object of fitted OLS.
#' @return A summary of the OLS model fit.
#' @export
summary.ols = function(model) {
  cat("OLS Regression Results\n")
  cat("------------------------------------------------\n")
  cat("Coefficients:\n")

  # Get the column names of X (or assign default names if missing)
  coef_names = colnames(model$X)
  if (is.null(coef_names)) {
    coef_names = c("(Intercept)", paste0("X", seq_along(model_ols$coefficients[-1])))
  }

  # Confidence intervals and tests for coefficients
  if (!exists("confints_tests_coefficients", mode = "function")) source("predictions.R")
  confints_table = confints_tests_coefficients(model)

  # Create a coefficients table with proper labels
  coef_table = data.frame(
    Estimate = model$coefficients,
    `Std. Error` = model$std_error,
    `CI Lower` = confints_table[, 2],
    `CI Upper` = confints_table[, 3]
  )
  rownames(coef_table) = coef_names

  # Print the coefficients table
  print(coef_table, row.names = TRUE)

  cat("------------------------------------------------\n")
  cat(sprintf("Residual standard error: %.4f on %d degrees of freedom\n",
              model$sigmahat_cor, model$df_residual))
  cat("Comparison between naive and bias corrected estimators of sigma:\n")
  cat(sprintf("Naive estimator: %.4f\n", model$sigmahat_naive))
  cat(sprintf("Bias corrected estimator: %.4f\n", model$sigmahat_cor))
  cat("------------------------------------------------\n")
  cat(sprintf("Multiple R-squared: %.4f\n", model$r_squared))
  cat(sprintf("Adjusted R-squared: %.4f\n", model$adjusted_r_squared))
  cat("\n")
}

