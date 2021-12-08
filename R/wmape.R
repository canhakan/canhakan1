# WMAPE

#' @title Weighted Mean Absolute Percentage Error
#' @description Calculates Weighted Mean Absolute Percentage Error
#' @param observed the observed results
#' @param predicted the predicted results
#' @return Calculated WMAPE result
#' @export
#'

wmape <- function(observed,
                  predicted){
  res = sum(abs(observed-predicted))/sum(observed)
  res = res * 100
  return(res)
}
