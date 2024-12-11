#' Analysis of Variance Function for Categorical covariate.
#'
#' @description This function calculates a One-Way ANOVA table.
#' @param data A data frame containing the variables.
#' @param response The response variable (numeric).
#' @param covariate The grouping variable must be categorical, p=1 only.
#' @return A list with ANOVA components: TSS, WSS, BSS, F-statistic, p-value, and a summary table.
#' @export

anova_ols_cat <- function(data, response, covariate) {

  if (!is.data.frame(data)) stop("The input `data` must be a data frame.")

  if (!response %in% names(data)) stop("`response` must be a valid column name in `data`.")

  if (!covariate %in% names(data)) stop("`covariate` must be a valid column name in `data`.")


  response <- data[[response]]
  covariate <- as.factor(data[[covariate]])


  # Calculate necessities
  overall_mean <- mean(response, na.rm = T)
  group_means <- tapply(response, covariate, mean)
  group_sizes <- table(covariate)
  n <- nrow(data)
  g <- length(levels(covariate))

  #Calculate sum of squares
  tss <- sum((response - overall_mean)^2, na.rm = T)
  bss <- sum(group_sizes * (group_means - overall_mean)^2)
  wss <- tss - bss

  # Calculate F-statistic
  f_stat <- (bss/(g-1)) / (wss/(n-g))

  # Calculate p-value
  p_value <- pf(f_stat, g-1, n-g, lower.tail = FALSE)


  anova_table = data.frame(
    `Df` = c(g-1, n-g),
    `Sum_Sq` = c(bss, wss),
    `Mean_Sq` = c(bss/(g-1), wss/(n-g)),
    `F_Stat` = c(f_stat, NA),
    `P_Val` = c(p_value, NA)
  )
  rownames(anova_table) = c("Factor", "Residual")

  #cat("ANOVA Table for categorical data\n")
  #cat("------------------------------------------------\n")
  return (anova_table)

}



#' Analysis of Variance Function for Non-Categorical covariates.
#'
#' @description This function computes the ANOVA table for a fitted OLS model calculated by ols_fit.
#' Implemented a sequential sum of squares that matched with 'aov' function in R.
#' Works only if design matrix X has no categorical variables.
#'
#' @param model An object returned by the `ols_fit` function.
#' @return An ANOVA table.
#' @export
anova_ols = function(model) {
  y = model$y
  X = model$X
  n = length(y)  # Number of observations
  p = ncol(X)  # Number of covariates, including intercept

  # Name the covariates
  covariates = colnames(X)
  if (is.null(covariates)) {
    covariates = c("(Intercept)", paste0("X", seq_len(p - 1)))
  }

  ss_model = numeric(p - 1)  # Sum of squares for each covariate
  df_model = numeric(p - 1)  # Degrees of freedom for each covariate
  rss_cur = sum((y - mean(y))^2)  # Start with TSS

  for (i in 2:p) {  # Sequentially add covariates (excluding the intercept)
    X_part = X[, 1:i, drop = FALSE]  # Include covariates up to i
    ols_part = ols_fit(X_part, y)  # Fit partial model
    rss_part = ols_part$rss
    ss_model[i - 1] = rss_cur - rss_part  # Compute incremental SS
    df_model[i - 1] = 1  # One degree of freedom per covariate
    rss_cur = rss_part  # Update RSS for the next iteration
  }

  # Residual
  rss_res = rss_cur
  df_res = n - p

  # Mean Squares
  ms_model = ss_model / df_model
  ms_res = rss_res / df_res

  # F-statistics and p-values
  F_statistics = ms_model / ms_res
  p_values = pf(F_statistics, df_model, df_res, lower.tail = FALSE)

  # Combine results into a table
  anova_table = data.frame(
    Covariate = c(covariates[-1], "Residual"),
    `Df` = c(df_model, df_res),
    `Sum_Sq` = c(ss_model, rss_res),
    `Mean_Sq` = c(ms_model, ms_res),
    `F_Stat` = c(F_statistics, NA),
    `P_Val` = c(p_values, NA)
  )

  return(anova_table)
}




