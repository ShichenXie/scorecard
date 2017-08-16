#' variable filter
#'
#' This function filter variables base on their minimun iv, maximum na percentage and maximum element percentage.
#' @name var_filter
#' @param dt Name of input data
#' @param y Name of y variable.
#' @param x Name vector of x variables, default NA.
#' @param iv_limit The minimun IV of variables that are kept, default 0.02.
#' @param na_perc_limit The maximum NA percent of each kept variable, default 0.95.
#' @param ele_perc_limit The maximun element (excluding NAs) percentage in each kept variable, default 0.95.
#' @param var_rm force removed variables, default NA
#' @param var_kp force kept variables, default NA
#' @export
#' @examples
#' # Load German credit data
#' data(germancredit)
#'
#' # variable filter
#' var_filter(germancredit, y = "creditability")
#'
var_filter <- function(dt, y, xs = NA, iv_limit = 0.02, na_perc_limit = 0.95, ele_perc_limit = 0.95, var_rm = NA, var_kp = NA) {
  # 最小iv值0.02
  # 最大缺失值百分比95%

  # 单个类别最大百分比90%
  # 最大类别数95%

  # 最小变异系数0.1%


  # transfer dt to data.table
  dt <- data.table(dt)
  # x variable names
  if (anyNA(xs)) xs <- setdiff(names(dt), y)


  # -iv
  iv_list <- iv(dt, y, xs)
  # -na percentage
  na_perc <- dt[, lapply(.SD, function(x) sum(is.na(x))/length(x)), .SDcols = xs]
  # -percentage limit
  ele_perc <- dt[, lapply(.SD, function(x) max(table(x)/sum(!is.na(x)))), .SDcols = xs]


  # remove na_perc>95 | ele_perc>0.95 | iv<0.02
  var_kept <- list(
    as.character( iv_list[V1 >= iv_limit, variable] ),
    names(na_perc[,na_perc <= na_perc_limit, with=FALSE]),
    names(ele_perc[,ele_perc <= ele_perc_limit, with=FALSE])
  )
  x_selected <- intersect(var_kept[[1]], var_kept[[2]])
  x_selected <- intersect(x_selected, var_kept[[3]])

  # remove variables
  if (!anyNA(var_rm))  x_selected <- setdiff(x_selected, var_rm)
  # add kept variable
  if (!anyNA(var_kp))  x_selected <- unique(c(x_selected, var_kp))

  # return
  dt_sel <- dt[, c(x_selected, y), with=FALSE ]

  return(dt_sel)

}

