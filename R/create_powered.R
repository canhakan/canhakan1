#' Takes input data and adds powered values of the features, as new features
#' @export
#' @title Creates Powered Data
#' @param data input data
#' @param powers list of powers to be added. Numbers in the list should be integers > 1.
#' @param exclude_head the columns of data that is not subject to lagging process (e.g. date/hour). Will be merged to left-side of the lagged data.
#' @param exclude_tail the columns of data that is not subject to lagging process (e.g. production). Will be merged to right-side of the lagged data.

create_powered <- function(data, powers, exclude_head, exclude_tail){
  # remove excludes
  exclude = c(exclude_head,exclude_tail)
  dat = data[,-..exclude]
  # Save data for final result
  res = dat
  # create powered data and set column names. Then combine it with the saved data
  for(i in powers){
    tem.dat = dat
    for(p in c(2:i)){
      tem.dat = tem.dat*dat
    }
    colnames(tem.dat) = paste(colnames(tem.dat),paste('p',i,sep=''),sep='_')
    res = cbind(res,tem.dat)
  }
  # merge excluded columns
  res = cbind(data[,..exclude_head],res,data[,..exclude_tail])
  return(res)
}
