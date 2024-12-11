#' Predict Method for OLS Model with Optional Confidence Intervals
#'
#' Generates predictions from a fitted OLS model, with optional confidence or prediction intervals.
#'
#' @param model A fitted OLS model object returned by `ols_fit`.
#' @param newdata A matrix of new predictor values. Must have the same number of columns as the original model matrix.
#' @param interval Type of interval calculation. `"none"` (default) for no interval, `"confidence"` for confidence intervals.
#' @param level Confidence level for the intervals (default is 0.95).
#' @return A vector of predictions, or a data frame if intervals are requested.
#' @export
ols_predict <- function(model, newdata, interval = "none", level = 0.95) {
  if (!is.matrix(newdata)) stop("newdata must be a matrix.")
  if (ncol(newdata) != ncol(model$X)) stop("Number of columns in newdata must match the model's predictors.")
  if (!interval %in% c("none", "confidence")) {
    stop("Invalid interval type. Use 'none' or 'confidence'.")
  }

  newy <- as.numeric(newdata %*% model$coefficients)

  if (interval == "none") {
    # Return predictions only
    return(newy)
  }

  # If interval == "confidence", calculate confidence intervals
  alpha <- 1 - level
  t_critical <- qt(1 - alpha / 2, df = model$df_residual)

  # Variance of predictions
  pred_var <- rowSums((newdata %*% model$covariance_matrix) * newdata)

  # Standard error of predictions
  pred_se <- sqrt(pred_var)

  # Confidence interval bounds
  lower_bound <- newy - t_critical * pred_se
  upper_bound <- newy + t_critical * pred_se

  # Combine into a data frame
  results <- data.frame(
    Prediction = newy,
    Lower_CI = lower_bound,
    Upper_CI = upper_bound
  )

  return(results)
}


