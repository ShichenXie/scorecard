#' variable filter
#'
#' This function filter variables base on their minimum iv, maximum na percentage and maximum element percentage.
#'
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name vector of x variables. Default NA. If x is NA, all variables exclude y will counted as x variables.
#' @param iv_limit The minimum IV of each kept variable, default 0.02.
#' @param na_perc_limit The maximum NA percent of each kept variable, default 0.95.
#' @param ele_perc_limit The maximum element (excluding NAs) percentage in each kept variable, default 0.95.
#' @param var_rm Name vector of force removed variables, default NA.
#' @param var_kp Name vector of force kept variables, default NA.
#'
#' @examples
#' # Load German credit data
#' data(germancredit)
#'
#' # variable filter
#' var_filter(germancredit, y = "creditability")
#'
#' @import data.table
#' @export
#'
var_filter <- function(dt, y, x = NA, iv_limit = 0.02, na_perc_limit = 0.95, ele_perc_limit = 0.95, var_rm = NA, var_kp = NA) {
  V1 = variable = NULL # no visible binding for global variable

  # transfer dt to data.table
  dt <- data.table(dt)
  dt[dt==""] <- NA
  # x variable names
  if (anyNA(x)) x <- setdiff(names(dt), y)


  # -iv
  iv_list <- iv(dt, y, x)
  # -na percentage
  na_perc <- dt[, lapply(.SD, function(a) sum(is.na(a))/length(a)), .SDcols = x]
  # -percentage limit
  ele_perc <- dt[, lapply(.SD, function(a) max(table(a)/sum(!is.na(a)))), .SDcols = x]


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

