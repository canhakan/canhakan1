
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
  # removing excludes
  exclude = c(exclude_head,exclude_tail)
  excols = colnames(train)[exclude]
  d1 = train[,-excols]
  d2 = test[,-excols]
  # scaling (normalizing i guess) : divide all by maximum (as the minimum is zero as a speed)
  maximum = max(d1)
  d1 = d1/maximum
  d2 = d2/maximum
  # merge excludes back
  d1 = cbind(train[,..exclude_head],
             d1,
             train[,..exclude_tail])
  d2 = cbind(test[,..exclude_head],
             d2,
             test[,..exclude_tail])

  res = list(d1,d2)
  return(res)
}
