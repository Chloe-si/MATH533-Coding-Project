
# todo: confidence intervals/hypothesis tests for individual coefficients,
# hypothesis tests for multiple cols, hypothesis tests for row constraints

#' Confidence intervals and tests for individual coefficients.
#' @param model returned object from ols_fit
#' @param alpha significance level
#' @param b vector or scalar representing the value(s) to use for the null hypotheses.
#'   (If vector, it's expected to have the same length as the beta vector.)
#'      That is, the null hypothesis for the betaj hypothesis test is
#'      H0: betaj = b (if scalar) or H0: betaj = b[j] (if vector).
#' @param bonf whether to do Bonferroni correction for multiple comparisons
#' @return A named matrix with the left bound, right bound, p-value for test,
#'  and adjusted p-value
#' @export
confints_tests_coefficients = function(model,
                                       alpha=0.05, b=0, bonf=F) {
  X = model$X
  n = nrow(X)
  p = ncol(X)
  betahat = model$coefficients
  sigmahat = model$sigma_cor

  se = model$std_error
  zalpha2 = qt(1 - alpha/2, n-p)
  if (bonf) {
    zalpha2 = qt(1 - alpha / (2 * p), n-p)
  }
  ci_left = betahat - zalpha2 * se
  ci_right = betahat + zalpha2 * se
  tval = abs((betahat - b) / se)
  pval = 2 * pt(tval, n-p, lower.tail=F)
  adj_pval = pmin(1,pval * p)

  results = cbind(betahat, ci_left, ci_right, pval, adj_pval)
  colnames(results) = c('betahat', 'CI left', 'CI right',
                        'p-value', 'Adj. p-value')
  return(results)
}

#' Hypothesis test that multiple coefficients are 0.
#' @param model returned object from fit_ols_model
#' @param testeq0 vector of values from 1 to p, indicating which coefficients are being tested
#' @param alpha significance level
#' @export
test_multiple_coefficients = function(model, testeq0, alpha=0.05) {
  X = model$X
  p = ncol(X)
}

#' Hypothesis test for row constraints H0: R beta = r.
#' @param model returned object from fit_ols_model
#' @param R matrix of row constraints -- must have p columns, full row rank, and no inconsistent constraints
#' @param r value of r to use in null hypothesis H0: R beta = r
#' @param alpha significance level
#' @export
test_row_constraints = function(model, R, r=0, alpha=0.05) {

}
