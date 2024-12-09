#' Calculate AIC and BIC for an lm Model
#'
#' This function computes the Akaike Information Criterion (AIC) and
#' Bayesian Information Criterion (BIC) for a fitted linear model created using `lm`.
#'
#' @param fitted_model A fitted model object created by the `lm` function.
#' @return A list containing:
#' \describe{
#'   \item{AIC}{The Akaike Information Criterion.}
#'   \item{BIC}{The Bayesian Information Criterion.}
#' }
#' @examples
#' data(iris)
#' model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#' AIC_BIC(model)
#' @export


AIC_BIC <- function(fitted_model){

  if (!inherits(fitted_model, "lm")) {
    stop("The input must be a model created by lm().")
  }

  n <- length(fitted_model$fitted.values)
  sigma_sq <- sum(resid(fitted_model)^2)/n

  #Cardinality of alpha
  c <- length(coef(fitted_model))

  aic <- n + n * log(2*pi*sigma_sq) + 2*c
  bic <- n + n * log(2*pi*sigma_sq) + log(n)*c

  list(AIC= aic,
       BIC = bic)

}


