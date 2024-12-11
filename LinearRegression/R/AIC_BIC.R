#' Calculate AIC and BIC for an lm Model
#'
#' This function computes the Akaike Information Criterion (AIC) and
#' Bayesian Information Criterion (BIC) for a fitted linear model created using `lm`.
#'
#' @param model A fitted model from ols_fit
#' @return A list containing:
#' \describe{
#'   \item{AIC}{The Akaike Information Criterion.}
#'   \item{BIC}{The Bayesian Information Criterion.}
#' }
#' @export


AIC_BIC <- function(model){

  # extract number of observations, number of covariates
  n <- nrow(model$X)
  p <- length(model$coefficients)
  # unbiased sigma^2 estimator
  sigma_sq <- (model$sigmahat_cor)^2

  aic <- n + n * log(2*pi*sigma_sq) + 2*p
  bic <- n + n * log(2*pi*sigma_sq) + log(n)*p

  return(
    list(AIC= aic,
       BIC = bic)
  )

}


