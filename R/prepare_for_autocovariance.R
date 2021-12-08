#' @description Prepares the Data for lagged(?) autocovariance process. Creates a new dataframe with columns as lag/lead values. (e.g. Lag1, Normal, Lead1)
#' @export
#' @title Preparing for Autocovariance
#' @param data input data
#' @param lags list of wanted lags. Negative values correspond to lags while positives correspond to leads.
#' @return data with columns as Lag/Lead


prepare_for_autocovariance <- function(data,
                                       lags){
  # create 1xlength(lags) matrix to turn into a data.table
  tdata = data.table::data.table(t(rep(0,length(lags))))
  # create column names
  tcolnames = list()
  for(lag in lags){
    if(lag < 0){
      tcolnames = cbind(tcolnames,(paste('lag',abs(lag),sep='')))
    }
    else if(lag > 0){
      tcolnames = cbind(tcolnames,(paste('lead',abs(lag),sep='')))
    }
    else{
      tcolnames = cbind(tcolnames,'normal')
    }
  }
  tcolnames = as.character(tcolnames)
  # set column names and change tdata to data.table
  colnames(tdata) <- tcolnames
  tdata <- tdata[-1,]
  # get min and max for removing first/last columns (for lag purposes)
  minlag = min(0,lags)
  maxlag = max(0,lags)
  # start adding rows to tdata
  startN = 1 - minlag
  endN = nrow(data) - maxlag
  for(row in c(startN:endN)){
    fullrow = t(data[row+lags,])
    colnames(fullrow) = tcolnames
    tdata <- rbind(tdata,fullrow)
  }
  return(tdata)
}
