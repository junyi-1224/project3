#' Leaner model Function
#'
#' This function testing if there is a  coefficient between the model and data
#' @param formula a formula of the class object
#' @param data input data frame
#' @keywords linear model
#' 
#' @return a table with rows of each coefficient and columns of the estimate, standard error, test statistic and p-value
#'
#' @examples
#' my_lm(bill_length_mm~bill_depth_mm, my_penguins)
#' 
#' @export
my_lm <- function(formula, data) {
  
  # create a model frame 
  object <- model.frame(formula, data)
  
  # create x matrix
  X <- model.matrix(formula, data)
  
  # create y matrix
  Y <- model.response(object)

  # calculate coefficient(beta_hat)
  coef <- solve(t(X) %*% X) %*% t(X) %*% Y

  # calculate degree of freedom
  df <- NROW(object) - NROW(coef)

  # calculate the variance
  var <- sum((Y - X %*% coef) ^ 2 / df)

  # calculate the standard error
  se <- sqrt(diag(var * solve(t(X) %*% X)))

  # calculate the t value
  t_val <- coef / se

  # calculate under the curve for a t-distribution, multiply this value by 2 to get the two-sided test output.
  pr <- 2 * pt(abs(t_val), df, lower.tail = FALSE)
  
  # create table
  result_table <- data.frame(cbind(coef, se, t_val, pr))
  rownames(result_table) <- colnames(X)
  colnames(result_table) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
                                                    
  return(result_table)
}
