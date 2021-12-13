#' T test function
#'
#' This function is used to compare the means of two groups
#' @param x numeric vector of data
#' @param alternative hypothesis is that the true difference is different from zero, allow value is one of "two.sided", "greater" or "less"
#' @param mu the null hypothesis value of mean
#' @keywords hypothesis
#' 
#' @return A list with the numeric test statistic, the degrees of freedom, the value of the parameter "alternative", and the p value
#' 
#' @examples
#' my_t_test(rnorm(20), "two.sided", 1)
#' my_t_test(c(1:33), "greater", 5)
#'
#' @export
my_t_test <- function(x, alternative, mu) {
  

  # calculate the t-statistics
  test_stat <- ((mean(x) - mu) / (sd(x) / sqrt(length(x))))
  
  # calculate degree of freedom
  df <- length(x) - 1

  # calculate p value by using two tail
  if(alternative == "two.sided") {
    p_val <- 2 * pt(abs(test_stat), df, lower.tail = FALSE)
  } else if (alternative == "greater") {
    p_val <- pt(abs(test_stat), df, lower.tail = FALSE)
  } else if ( alternative == "less") {
    p_val <- pt(abs(test_stat), df, lower.tail = TRUE)
  }
  
  # send an error message if the alternative is error
  if (!(alternative %in% c("two.sided", "less", "greater"))) {
    stop("Error! Please choose from two.sided, less, or greater.")
  }
  

  # t_list <- list(test_stat, df, alternative, p_val)
  result <- (list("test statistic" = test_stat,
              "degrees of freedom" = df,
              "alternative" = alternative,
              "p-value" = p_val))
  return(result)
}

