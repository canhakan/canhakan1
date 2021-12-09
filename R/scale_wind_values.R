
#' @title Scaling Wind Speed on Train and Test Data
#' @description Scales wind speed on train data to between 0 and 1. Then applies the same coefficients to test data.
#' @param train data where the scaling coefficients are calculated and then applied
#' @param test data where scaling done on train is applied to
#' @param exclude_head the columns of data that is not subject to lagging process (e.g. date/hour). Will be merged to left-side of the lagged data.
#' @param exclude_tail the columns of data that is not subject to lagging process (e.g. production). Will be merged to right-side of the lagged data.
#' @return list of scaled train and test data respectively
#' @export


scale_wind_values <- function(train,
                              test,
                              exclude_head,
                              exclude_tail){
  # set train and test as data.frame as data.table creates troubles
  trainX = data.frame(train)
  testX = data.frame(test)
  # removing excludes
  exclude = c(exclude_head,exclude_tail)
  d1 = trainX[,-exclude]
  d2 = testX[,-exclude]
  # scaling (normalizing i guess) : divide all by maximum (as the minimum is zero as a speed)
  maximum = max(d1)
  d1 = d1/maximum
  d2 = d2/maximum
  # merge excludes back
  d1 = cbind(trainX[,exclude_head],
             d1,
             trainX[,exclude_tail])
  d2 = cbind(testX[,exclude_head],
             d2,
             testX[,exclude_tail])
  # need to set colnames again as they may change if exclude_head or tail is not a list but a number
  colnames(d1) = colnames(train)
  colnames(d2) = colnames(test) # or colnames(train)

  res = list(d1,d2)
  return(res)
}
