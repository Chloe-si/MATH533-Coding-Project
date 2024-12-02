#' Fit an Ordinary Least Squares Model
#'
#' @param X A matrix of predictors (design matrix).
#' @param y A numeric vector of responses.
#' @return An object of class "ols" containing:
#'   - `coefficients`: Estimated coefficients (beta_hat).
#'   - `fitted_values`: Fitted values (y_hat).
#'   - `residuals`: Residuals (e = y - y_hat).
#'   - `rss`: Residual Sum of Squares (RSS).
#'   - `tss`: Total Sum of Squares (TSS).
#'   - `r_squared`: Coefficient of determination (R^2).
#'   - `adjusted_r_squared`: Adjusted R^2.
#'   - `std_error`: Standard errors of coefficients.
#'   - `p_values`: p-values for coefficients.
#'   - `df_residual`: Residual degrees of freedom.
#' @export
ols_fit <- function(X, y) {
  if (!is.matrix(X)) stop("X must be a matrix.")
  if (!is.numeric(y)) stop("y must be a numeric vector.")
  if (nrow(X) != length(y)) stop("Number of rows in X must match length of y.")

  n <- nrow(X)  # Number of observations
  p <- ncol(X)  # Number of covariates

  # Compute beta_hat
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y

  # Compute fitted values
  y_hat <- X %*% beta_hat

  # Compute residuals
  residuals <- y - y_hat

  # Residual Sum of Squares (RSS)
  rss <- sum(residuals^2)

  # Standard Errors of Coefficients
  sigmahat_cor <- sqrt(rss / (n - p))
  sigmahat_naive <- sqrt(rss / n)
  covariance_matrix <- sigmahat_cor * solve(t(X) %*% X)
  std_error <- sqrt(diag(covariance_matrix))

  # Total Sum of Squares (TSS)
  tss <- sum((y - mean(y))^2)

  # R^2 and Adjusted R^2
  r_squared <- 1 - (rss / tss)
  adjusted_r_squared <- 1 - ((1 - r_squared) * n / (n - p))

  # Hat matrix H and leverages
  hat_matrix <- X %*% solve(t(X) %*% X) %*% t(X)
  leverages <- diag(hat_matrix)

  return(
    list(
      X=X,
      coefficients = beta_hat,
      y_fitted = y_hat,
      residuals = residuals,
      rss = rss,
      r_squared = r_squared,
      adjusted_r_squared = adjusted_r_squared,
      std_error = std_error,
      hat_matrix = hat_matrix,
      leverages = leverages
    )
  )
}

#' Summary for OLS Objects
#'
#' @param model An object of fitted OLS.
#' @return A summary of the OLS model fit.
#' @export
summary.ols <- function(model) {
  cat("OLS Regression Results\n")
  cat("------------------------------------------------\n")
  cat("Coefficients:\n")
  coef_table <- data.frame(
    Estimate = model$coefficients,
    Std.Error = model$std_error
  )
  print(coef_table)
  cat("------------------------------------------------\n")
  cat(sprintf("Multiple R-squared: %.4f\n", model$r_squared))
  cat(sprintf("Adjusted R-squared: %.4f\n", model$adjusted_r_squared))
  cat("\n")
}
