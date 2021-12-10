# FILTER BY MONTH (A SMALL FUNCTION)
# may be deleted

filter_month <- function(data,month_no){
  return(data[month(date) == month_no,])
}

