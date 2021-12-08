# Temporal PCA

#' @title Temporal PCA
#' @description A function that transforms a data with lag values to a format where multiplication with eigenvectors of temporal covariance is possible.
#'   Then multiplicates with the eigenvectors matrix and returns the dimension-reduced data.
#' @param data data with lag values. Columnnames must be of format (featurename)_(lag) (e.g. ne_lag3, nw_lead1, se)
#' @param lags list of lags to be used. Same as create_lagged() argument
#' @param features list of feature names without lag/lead additions
#' @param ev the temporal eigenvector matrix
#' @param n represents how many Principal Components to be returned
#' @return Time dimension reduced data. (e.g. ne_Time1, sw_Time2)
#' @export

temporal_PCA <- function(data,
                         lags,
                         features,
                         ev,
                         n){
  # set data to data.frame for a problem (temporarly. i need to read data.tables)
  data = data.frame(data)
  # seperating data with location names (feature names)
  res = data.table::data.table()
  for(f in features){
    tempdata = data.table::data.table()
    namelist = list()
    for(l in lags){
      if(l<0){
        x = paste(f,'_lag',-l,sep='')
      }else if(l>0){
        x = paste(f,'_lead',l,sep='')
      }else{
        x = f
      }
      tempdata = cbind(tempdata, data[,x])
      namelist = c(namelist, x)
    }
    # now we have a list of column names to seperate data
    #         tempdata = data[,..namelist]
    #         print(head(tempdata))
    tempdata = as.matrix(tempdata)
    # doing dimension reduction
    tempdata = tempdata %*% ev[,1:n]
    # creating new names for reduced temporal dimension
    newnames = list()
    for(times in c(1:n)){
      y = paste(f,'Time',times,sep='')
      newnames = c(newnames,y)
    }
    colnames(tempdata) = newnames
    # adding new reduced data to result
    res = cbind(res,tempdata)
  }
  invisible(res)
}
