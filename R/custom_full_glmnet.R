# Custom Cv.Glmnet -> Glmnet -> Predict on Test -> Return Predictions

#' @description Uses 'glmnet' package. Uses lambda.min from cv.glmnet for doing predictions on the test data. Returns the prediction results. For seeing Weighted Mean Absolute Error and Mean Absolute Error, set output to TRUE.
#' @export
#' @title Custom GLMNET Function for simplicity
#' @param train 'x' for 'glmnet'
#' @param trainres 'y' for 'glmnet'
#' @param test matrix of test data for returning predictions
#' @param testres observations of the test data for printing WMAPE results
#' @param perf type.measure for cv.glmnet. Default = 'mae'
#' @param output set 'TRUE' for printing WMAPE results. Default = FALSE
#' @param seed for setting seed. Default = 42
#' @return The predictions of the glmnet model applied on the test data

# train, trainres, test, testres, perf(type.measure)
# for perf prefer mae
custom_full_glmnet <- function(train,
                               trainres,
                               test,
                               testres,
                               perf='mae',
                               output = FALSE,
                               seed=42) {
  set.seed(seed)
  fit1 = glmnet::cv.glmnet(as.matrix(train),
                   as.matrix(trainres),
                   type.measure = perf)

  # predicting
  pred.min = stats::predict(fit1,
                     as.matrix(test),
                     s = "lambda.min")
  pred.1se = stats::predict(fit1,
                     as.matrix(test),
                     s = "lambda.1se")
  if(output){
    # WMAPE results
    print(paste('WMAPE on test set (min):', wmape(pred.min, as.matrix(testres))))
    print(paste('WMAPEon test set (1se):', wmape(pred.1se, as.matrix(testres))))
    print('---------')
  }

  invisible(pred.min)
}
