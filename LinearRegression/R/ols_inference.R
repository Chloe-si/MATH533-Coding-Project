
# todo: confidence intervals/hypothesis tests for individual coefficients,
# hypothesis tests for multiple cols, hypothesis tests for row constraints

#' Confidence Intervals and Tests for Individual Coefficients.
#' @param model Returned object from ols_fit
#' @param alpha Significance level
#' @param b vector or scalar representing the value(s) to use for the null hypotheses.
#'   (If vector, it's expected to have the same length as the beta vector.)
#'      That is, the null hypothesis for the betaj hypothesis test is
#'      H0: betaj = b (if scalar) or H0: betaj = b[j] (if vector).
#' @param bonf whether to do Bonferroni correction for multiple comparisons
#' @return A named matrix with the left bound, right bound, p-value for test,
#'  and adjusted p-value
#' @export
coef_inference = function(model, alpha=0.05, b=0, bonf=F) {
  X = model$X
  n = nrow(X)
  p = ncol(X)
  stopifnot(length(b) == 1 || length(b) == p)
  # extract required data from fitted ols model
  betahat = model$coefficients
  sigmahat = model$sigma_cor
  se = model$std_error

  # Test statistics of betahat(_j) follows a t-distribution of df=n-p
  zalpha2 = qt(1 - alpha/2, n-p)
  if (bonf) {
    zalpha2 = qt(1 - alpha / (2 * p), n-p)
  }

  # Confidence interval for batehat(_j)
  ci_lower = betahat - zalpha2 * se
  ci_upper = betahat + zalpha2 * se
  tval = abs((betahat - b) / se)
  pval = 2 * pt(tval, n-p, lower.tail=F)
  adj_pval = pmin(1, pval * p)

  results = cbind(betahat, ci_lower, ci_upper, pval, adj_pval)
  colnames(results) = c('betahat', 'CI lower', 'CI upper',
                        'p-value', 'Adj. p-value')
  return(results)
}

#' Hypothesis test that multiple coefficients are 0.
#' @param model returned object from fit_ols_model
#' @param testeq0 vector of values from 1 to p, indicating which coefficients are being tested
#' @param alpha significance level
#' @export
multicoef_inference = function(model, testeq0, alpha=0.05) {
  X = model$X
  n = nrow(X)
  p = ncol(X)
  p2 = length(testeq0)
  y = model$y
  sigma12_sqrd = model$sigmahat_naive^2

  X1 = as.matrix(X[,-testeq0])
  if (!exists("ols_fit", mode = "function")) source("ols_estimation.R")
  nullmodel = ols_fit(X1, y)
  sigma1_sqrd = nullmodel$sigmahat_naive^2
  Fstat = ((sigma1_sqrd - sigma12_sqrd) / p2) / (sigma12_sqrd / (n-p))
  pval = pf(Fstat, p2, n-p, lower.tail=F)
  return(list(Fstat=Fstat, pval=pval))
}

#' Hypothesis test for row constraints H0: R beta = r.
#' @param model returned object from fit_ols_model
#' @param R matrix of row constraints -- must have p columns, full row rank, and no inconsistent constraints
#' @param r value of r to use in null hypothesis H0: R beta = r
#' @param alpha significance level
#' @export
test_row_constraints = function(model, R, r=0, alpha=0.05) {

}
