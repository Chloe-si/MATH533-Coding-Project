#' Ridge Regression Estimator
#'
#' This function computes the ridge regression estimator for a given design matrix, response vector, and regularization parameter.
#'
#' @param X A numeric matrix representing the design matrix.
#' @param y A numeric vector representing the response variable.
#' @param lambda A numeric value representing the regularization parameter (\(\lambda > 0\)).
#' @return A numeric vector containing the ridge regression coefficients.
#' @examples
#'X_values <- as.matrix(cbind(1, iris$Sepal.Width, iris$Petal.Width))
#'colnames(X_values) <- c("Intercept", "Sepal.Width", "Petal.Width")
#'y_values <- iris$Sepal.Length
#'lambda <- 0.1
#' ridge_estimator(X_values, y_values, lambda)
#' @export
#' 


ridge_estimator <- function(X_values, y_values, lambda) {
  if (!is.matrix(X)) stop("Error: X must be a matrix.")
  if (!is.vector(y)) stop("Error: y must be a vector.")
  if (lambda < 0 || qr(X)$rank < ncol(X)) {
    stop("Error: Either lambda must be greater than zero or the design matrix X must be full rank.")
  }
  
  p <- ncol(X)
  
  XtX <- t(X) %*% X
  XtY <- t(X) %*% y
  ridge_beta <- solve(XtX + lambda * diag(p)) %*% XtY
  
  return(ridge_beta)
}


