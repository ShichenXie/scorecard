# library(DataExplorer)
# library(dlookr)

#' Variable Describe
#'
#' This function provides descriptive statistic for exploratory data analysis.
#'
#' @param dt A data frame.
#'
#' @examples
#' data("germancredit")
#'
#' eda = describe(germancredit)
#' eda
#'
#' @importFrom stats sd
#' @export
#'
describe = function(dt) {
  dt = setDT(copy(dt))

  sum_dt =
    data.table(
      variable = names(dt),
      class = dt[, sapply(.SD, class)],
      count = dt[, .N],
      unique_count = dt[, sapply(.SD, function(x) uniqueN(x, na.rm = TRUE) )],
      missing_count = dt[, sapply(.SD, function(x) sum(is.na(x)) )],
      missing_rate = dt[, sapply(.SD, function(x) mean(is.na(x)) )]
    )


  xnum = names(which(dt[, sapply(.SD, is.numeric)]))
  dtnum = dt[, xnum, with = FALSE]
  sum_xnum = as.data.table(do.call(rbind, lapply(dtnum, summary)))[, `:=`(
    variable = xnum,
    sd = dtnum[,sapply(.SD, sd)]
  )]
  setnames(sum_xnum, c('min', 'p25', 'p50', 'mean', 'p75', 'max', 'variable', 'sd'))
  sum_xnum = sum_xnum[,c('variable', 'mean', 'sd', 'min', 'p25', 'p50', 'p75', 'max'), with = FALSE]


  vardesc = merge(
    sum_dt, sum_xnum,
    by = 'variable', all = TRUE, sort = FALSE
  )[,(c('missing_rate', 'mean', 'sd')) := lapply(.SD, function(x) round(x,4)),
    .SDcols = c('missing_rate', 'mean', 'sd')
  ][]

  return(vardesc)
}

