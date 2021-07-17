# library(DataExplorer)
# library(dlookr)

#' Variable Describe
#'
#' This function provides descriptive statistic for exploratory data analysis.
#'
#' @param dt A data frame.
#'
#' @examples
#' library(data.table)
#'
#' data("germancredit")
#' dat = rbind(
#'   setDT(germancredit),
#'   data.table(creditability=sample(c("good","bad"),100,replace=TRUE)),
#'   fill=TRUE)
#'
#' eda = describe(dat)
#' eda
#'
#' @importFrom stats sd
#' @export
#'
describe = function(dt) {
  `NA's` = NULL
  dt = setDT(copy(dt))

  sum_dt =
    data.table(
      variable = names(dt),
      class = dt[, sapply(.SD, class)],
      count = dt[, .N],
      missing_rate = dt[, sapply(.SD, function(x) mean(is.na(x)) )],
      unique_count = dt[, sapply(.SD, function(x) uniqueN(x, na.rm = TRUE) )],
      identical_rate = dt[, sapply(.SD, function(a) fun_identical_rate(a) )]
    )


  xnum = names(which(dt[, sapply(.SD, is.numeric)]))
  dtnum = dt[, xnum, with = FALSE]
  sum_dtnum = as.data.table(do.call(rbind, lapply(dtnum, summary)))[, `:=`(
    variable = xnum,
    sd = dtnum[,sapply(.SD, function(x) sd(x, na.rm = TRUE))]
  )][, `NA's` := NULL]
  setnames(sum_dtnum, c('min', 'p25', 'p50', 'mean', 'p75', 'max', 'variable', 'sd'))
  sum_dtnum = sum_dtnum[,c('variable', 'min', 'p25', 'p50', 'p75', 'max', 'mean', 'sd'), with = FALSE][, cov := sd/mean]


  xround = c('identical_rate', 'missing_rate', 'mean', 'sd', 'cov')
  vardesc = merge(
    sum_dt, sum_dtnum,
    by = 'variable', all = TRUE, sort = FALSE
  )[,(xround) := lapply(.SD, function(x) round(x,4)),
    .SDcols = xround
  ][]

  return(vardesc)
}


fun_identical_rate = function(x) {
  pt = prop.table(table(x))
  max_rate = ifelse(length(pt) == 0, Inf, max(pt, na.rm = TRUE))
  return(max_rate)
}
