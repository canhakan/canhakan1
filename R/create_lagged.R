# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#' Creates a new data frame(?) with Lag/Lead values. Also one may exclude the features(columns) from "lag process" but have them with the new data frame
#' @export
#' @title Creates Lagged Data
#' @param data input data
#' @param lags list of wanted lags. Negative values correspond to lags while positives correspond to leads.
#' @param exclude_head the columns of data that is not subject to lagging process (e.g. date/hour). Will be merged to left-side of the lagged data.
#' @param exclude_tail the columns of data that is not subject to lagging process (e.g. production). Will be merged to right-side
create_lagged <- function(data,
                          lags,
                          exclude_head = NULL,
                          exclude_tail = NULL){
  # STOP IF PACKAGES DOES NOT EXIST --------------
    # if (!requireNamespace('data.table', quietly = TRUE)) {
    #   stop("Package \"data.table\" needed for this function to work. Please install it.",
    #        call. = FALSE)
    # }
  # STARTS HERE ----------------------------------
  # get min/max for removing first/last rows from data for lag/lead
  min_lag = min(0,lags)
  max_lag = max(0,lags)
  startN = 1 - minlag # for cutting the head
  endN = nrow(data) - maxlag # for cutting the tail
  # seperating excludes
  exclude = c(exclude_head, exclude_tail)
  tobe.head = data[(startN:endN),..exclude_head]
  tobe.tail = data[(startN:endN),..exclude_tail]
  # creating an empty data.table for
  # dat = data.table::data.table() # empty data table
  dat = data.frame()
  # adding lagged values one by one (column by column)
  for(i in lags){
    lagdat = data[((startN+i):(endN+i)),-..exclude]
    if(i<0){
      lagname = paste('lag',-i,sep='')
      colnames(lagdat) = paste(colnames(lagdat),lagname,sep='_')
    } else if(i>0){
      lagname = paste('lead',i,sep='')
      colnames(lagdat) = paste(colnames(lagdat),lagname,sep='_')
    }
    dat = cbind(dat,lagdat)
  }
  dat = cbind(tobe.head,dat,tobe.tail)
  return(dat)
}








