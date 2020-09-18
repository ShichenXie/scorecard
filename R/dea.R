# library(DataExplorer)
# library(dlookr)

var_dea = function(dt) {
  dt = setDT(copy(dt))

  sum_dt =
    data.table(
      variable = names(dt),
      class = dt[, sapply(.SD, class)],
      unique_num = dt[, sapply(.SD, function(x) uniqueN(x, na.rm = TRUE) )],
      missing_rate = dt[, sapply(.SD, function(x) mean(is.na(x)) )]
    )



  xnum = names(which(dt[, sapply(.SD, is.numeric)]))
  sum_xnum = as.data.frame(do.call(rbind, lapply(dt[, xnum, with = FALSE], summary)))
  sum_xnum$variable = row.names(sum_xnum)
  names(sum_xnum) = c('min', 'p25', 'p50', 'mean', 'p75', 'max', 'variable')
  setDT(sum_xnum)


  merge(sum_dt, sum_xnum, by = 'variable', all = TRUE, sort = FALSE)[, `:=`(
    missing_rate = round(missing_rate, 4),
    mean = round(mean, 4)
  )]
}

