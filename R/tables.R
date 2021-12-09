# Table variables:

pkg.env <- new.env(parent = emptyenv())

pkg.env$big_table <- data.table::data.table(date = Sys.Date(),
                                            year = integer(1),
                                            month = integer(1),
                                            hour = integer(1),
                                            model = character(1),
                                            actual = numeric(1),
                                            predicted = numeric(1),
                                            wmape = numeric(1))
pkg.env$big_table <- pkg.env$big_table[-1,]


pkg.env$small_table <- data.table::data.table(model = character(),
                                              wmape = numeric(),
                                              notes = character())
