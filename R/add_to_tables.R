# SHOULD I DO GLOBAL VARIABLE STUFF??? ASK!

#' @rdname add_to_tables
#' @title Adds Models Results to Two Result Tables
#' @description Adds models results to two tables. First one is big table, where every observation and prediction is visible. Second one is small table where only model name, performance and notes are visible.
#' @param data is the full data with 'y' values and all
#' @param predictions is the model's prediction results
#' @param big_table is the existing table of all results
#' @param small_table is the existing table of summarised results
#' @param model_name is the name of the model that produced the predictions. For note taking purposes, only.
#' @param notes is additional notes for small_table
#' @return a list of tables. first element is the 'big_table', second one is the 'small_table'.
#'     Big table contains: date, year, month, hour, model name, actual results, predicted results and WMAPE
#'     Small table contains: model name, WMAPE and notes
#' @export
add_to_tables <- function(data,
                          predictions,
                          big_table,
                          small_table,
                          model_name,
                          notes='-'){
  # getting year and month
  datetxt = data$date
  yearx  = as.numeric(format(datetxt, format = "%Y"))
  monthx = as.numeric(format(datetxt, format = "%m"))
  # calculate WMAPE again
  err = wmape(predictions, as.matrix(data$production))
  # create the resulting data.table
  res = data.table::data.table(date = datetxt,
                   year = yearx,
                   month = monthx,
                   hour = data$hour,
                   model = modelname,
                   actual = data$production,
                   predicted = predictions,
                   wmape = err)
  # error at predicted and wmape : colnames are predicted.1 and wmape.1 (prolly because of others named that way)
  colnames(res)[7:8] <- c('predicted','wmape')
  big_table = rbind(big_table,res)

  res2 = data.table::data.table(model = modelname,
                   wmape = err,
                   notes = notes)
  small_table = rbind(small_table,res2)
  return(list(big_table,small_table))
}
