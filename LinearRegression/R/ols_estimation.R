#' Fit an Ordinary Least Squares Model
#'
#' Fits a linear regression model using Ordinary Least Squares (OLS).
#'
#' @param X A matrix of predictors, if intercept included, must be on p=1.
#' @param y A numeric vector of responses.
#' @param homo Logical; TRUE assumes homoscedasticity (default), FALSE assumes heteroscedasticity.
#' @return An object of class "ols" containing:
#'   \itemize{
#'     \item {X}: Input covariate matrix.
#'     \item {coefficients}: Estimated coefficients (\eqn{\beta}).
#'     \item {y_fitted}: Fitted values (\eqn{\hat{y}}).
#'     \item {residuals}: Residuals (\eqn{e = y - \hat{y}}).
#'     \item {rss}: Residual Sum of Squares.
#'     \item {r_squared}: Coefficient of determination (\eqn{R^2}).
#'     \item {adjusted_r_squared}: Adjusted \eqn{R^2}.
#'     \item {std_error}: Standard errors of coefficients.
#'     \item {hat_matrix}: Hat matrix.
#'     \item {leverages}: Diagonal elements of the hat matrix.
#'     \item {covariance_matrix}: Variance-covariance matrix of coefficients.
#'     \item {sigmahat_cor}: Corrected sigma estimator for homoscedasticity.
#'     \item {sigmahat_naive}: Naive sigma estimator.
#'   }
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
  rss = sum(as.numeric(residuals)^2)

  # Standard Errors of Coefficients
  if (homo){
    sigmahat_cor = sqrt(rss / (n - p))
    sigmahat_naive = sqrt(rss / n)
    covariance_matrix = sigmahat_cor^2 * solve(t(X) %*% X)
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
      y = y,
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
ols_summary = function(model) {
  cat("OLS Regression Results\n")
  cat("------------------------------------------------\n")
  cat("Coefficients:\n")

  # Get the column names of X (or assign default names if missing)
  coef_names = colnames(model$X)
  if (is.null(coef_names)) {
    coef_names = c("(Intercept)", paste0("X", seq_along(model$coefficients[-1])))
  }

  # Confidence intervals and tests for coefficients
  if (!exists("coef_inference", mode = "function")) source("predictions.R")
  confints_table = coef_inference(model)

  # Create a coefficients table with proper labels
  coef_table = data.frame(
    Estimate = model$coefficients,
    `Std_Error` = model$std_error,
    `CI_Lower` = confints_table[, 2],
    `CI_Upper` = confints_table[, 3],
    `P_Val` = confints_table[, 4]
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

