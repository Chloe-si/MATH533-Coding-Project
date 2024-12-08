

analysis_of_variance <- function(data, response, covariate) {
  
  
  response <- data[[deparse(substitute(response))]]
  covariate <- as.factor(data[[deparse(substitute(covariate))]])
  
  
  # Calculate necessities
  overall_mean <- mean(response)
  group_means <- tapply(response, covariate, mean)
  group_sizes <- table(covariate)
  n <- nrow(data)
  g <- length(levels(covariate))
  
  
  #Calculate sum of squares
  tss <- sum((response - overall_mean)^2)
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
      P_value = c(p_value, NA)
    )
  )
  
  return(results)
}




