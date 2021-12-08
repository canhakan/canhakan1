# PCA

#' @title A Custom PCA Function
#' @description A custom PCA function that does PCA on train data and applies the results to test. Returns first 'n' PCs of train and test results in a list.
#' @param train training data that PCA model will be calculated on
#' @param test test data for applying the PCA results on to.
#' @param n indicates the wanted number of Principal Components to be returned
#' @return A list of first n principle components of train and test data, respectively.
#' @export

custom_pca <- function(train,
                       test,
                       n){
  # Do PCA on train data
  pca = prcomp(train)
  # Apply it to Train and Test data and take first n PCs
  pctrain = predict(pca,train)[,1:n]
  pctest  = predict(pca,test)[,1:n]
  # Return both results in a list
  return(list(pctrain,pctest))
}
