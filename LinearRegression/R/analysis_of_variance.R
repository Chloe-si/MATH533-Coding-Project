#' Analysis of Variance Function
#'
#' @description This function calculates an ANOVA table and p-value.
#' @param data A data frame containing the variables.
#' @param response The response variable (numeric).
#' @param covariate The grouping variable (factor or character).
#' @return A list with ANOVA components: TSS, WSS, BSS, F-statistic, p-value, and a summary table.
#' @examples
#' analysis_of_variance(iris, Sepal.Length, Species)
#' @export

analysis_of_variance <- function(data, response, covariate) {

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


  results <- list(
    TSS = tss,
    WSS = wss,
    BSS = bss,
    F_statistic = f_stat,
    p_value = p_value,
    Summary_Table = data.frame(
      Effect = c("Factor", "Residual"),
      DF = c(g-1, n-g),
      SS = c(bss, wss),
      MS = c(bss/(g-1), wss/(n-g)),
      F_stat = c(f_stat, NA),
      p_value = c(p_value, NA)
    )
  )

  return(results)
}




