#' Knn (K nearest neighbors) function
#'
#' Use Knn function to predict and record the errors
#' 
#' @param train input dataframe
#' @param cl values of true class from my training data 
#' @param k_nn integer value represent the number of neighbors
#' @param k_cv integer value represent the number of folds
#' 
#' @keywords prediction
#' 
#' @return a list with objects:
#' \code{class}, a vector of the predicted class yi_hat for all observations;
#' \code{cv_err}, a numeric with the cross-validation misclassification error.
#' 
#' @examples
#' my_penguins <- my_penguins %>% na.omit
#' my_knn_cv(my_penguins[3:6], my_penguins$species, 1, 5)
#' 
#' @import class magrittr stats
#' 
#' @export
#' 
#' 
  
my_knn_cv <- function(train, cl, k_nn, k_cv){

  # create randomly folds
  fold <- sample(rep(1:k_cv, length = length(rownames(train))))

  # set folds to dataset
  data <- train %>%
    dplyr::mutate(fold = fold) 
  
  # Empty matrix to store predictions
  pred_mat <- rep(NA, length(cl))
  
  # iterate through different folds as test data
  for(i in (1:k_cv)){
    
    # split data into training and testing
    data_train <- data %>% dplyr::filter(fold != i)
    data_test <- data %>% dplyr::filter(fold == i)
    
    cl_train <- cl[fold != i] # length of the train class
    cl_test <- cl[fold == i] # length of the test class
    
    #store prediction
    pred_mat[fold == i] <- as.character(class::knn(data_train[,-ncol(data_train)],
                                               data_test[,-ncol(data_test)],
                                               cl_train, k_nn))
  }
  class <- as.character(class::knn(train,train,cl,k_nn))
  cv_err <- mean(pred_mat != cl)
  
  mylist <- list("class" = class,
                 "cv_error" = cv_err)
  return(mylist)
}
  
  