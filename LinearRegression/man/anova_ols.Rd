% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anova.R
\name{anova_ols}
\alias{anova_ols}
\title{Analysis of Variance Function for Non-Categorical covariates.}
\usage{
anova_ols(model)
}
\arguments{
\item{model}{An object returned by the `ols_fit` function.}
}
\value{
An ANOVA table.
}
\description{
This function computes the ANOVA table for a fitted OLS model calculated by ols_fit.
Implemented a sequential sum of squares that matched with 'aov' function in R.
Works only if design matrix X has no categorical variables.
}
