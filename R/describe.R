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
  # `NA's` = cv = NULL
  dt = setDT(copy(dt))

  # summary numeric columns
  colnum = xefun::cols_type(dt, 'num')
  dtnum = dt[, colnum, with = FALSE]
  dtsum_a = as.data.table(do.call(rbind, lapply(dtnum, summary)))[, `:=`(
    variable = colnum
  )]

  # desc idx
  dtsum_b = merge2(list(
    data.table(class = dt[, sapply(.SD, class)], keep.rownames = 'variable'),
    data.table(count = dt[, sapply(.SD, length)], keep.rownames = 'variable'),
    data.table(missing_rate = dt[, sapply(.SD, function(x) mean(is.na(x)) )], keep.rownames = 'variable'),
    data.table(unique_count = dt[, sapply(.SD, function(x) uniqueN(x, na.rm = TRUE) )], keep.rownames = 'variable'),
    data.table(identical_rate = dt[, sapply(.SD, function(a) fun_identical_rate(a) )], keep.rownames = 'variable'),
    data.table(sd = dt[,sapply(.SD, function(x) sd(x, na.rm = TRUE)), .SDcols = colnum], keep.rownames = 'variable')
  ), by = 'variable', all = T)


  dtdesc = merge(
    dtsum_b, dtsum_a,
    by = 'variable', all = TRUE, sort = FALSE
  )

  colround = setdiff(xefun::cols_type(dtdesc, 'num'), xefun::cols_type(dtdesc, 'int'))
  dtdesc = dtdesc[,(colround) := lapply(.SD, function(x) round(x,4)), .SDcols = colround][]

  return(dtdesc)
}


fun_identical_rate = function(x) {
  pt = prop.table(table(x))
  max_rate = ifelse(length(pt) == 0, Inf, max(pt, na.rm = TRUE))
  return(max_rate)
}
