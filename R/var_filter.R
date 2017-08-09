#' variable filter
#'
#' This function filter variables base on their iv, na percentage and length of unique elements.
#' @name var_filter
#' @param dt Name of input data
#' @param y Name of y variable.
#' @param x Name vector of x variables, default "".
#' @param iv_limit The minimun IV of variables that are kept, default 0.02.
#' @param na_perc_limit The maximum NA percent of variables that are kept, default 0.95.
#' @param uniqueone_rm Logical, default TRUE, it remove variables that have only one unique elements (including NAs).
#' @param var_rm variables need to remove force
#' @export
#' @examples
#' # Load German credit data and create good and bad series
#' data(germancredit)
#' dt <- germancredit[, c('creditability', 'credit.amount', 'age.in.years', 'present.employment.since')]
#'
#' # variable filter
#' var_filter(dt, y = "creditability")
#'
var_filter <- function(dat, y, x="", iv_limit = 0.02, na_perc_limit = 0.95, uniqueone_rm = TRUE, var_rm = "") {
  # 最小iv值0.02
  # 最大缺失值百分比95%

  # 单个类别最大百分比90%
  # 最大类别数95%

  # 最小变异系数0.1%


  # transfer dt to data.table
  dt <- data.table(dat)
  # x variable names
  if (x=="") xnames <- setdiff(names(dt), y)


  # iv
  iv_list <- iv(dt, y)
  # ivlist_select <- as.character( iv_list[V1 >= iv_limit]$variable )
  # na percentage
  na_perc <- dt[, lapply(.SD, function(x) sum(is.na(x))/length(x)), .SDcols = xnames]#t()
  # unique length
  unique_length <- dt[, lapply(.SD, function(x) length(unique(x))), .SDcols = xnames]


  # remove na_perc>95 & uniquelength==1 & iv<0.01
  var_kept <- list(
    as.character( iv_list[V1 >= iv_limit, variable] ),
    names(na_perc[,na_perc <= na_perc_limit, with=FALSE]),
    names(na_perc[,unique_length > uniqueone_rm, with=FALSE])
  )
  x_selected <- intersect(var_kept[[1]], var_kept[[2]])
  x_selected <- intersect(x_selected, var_kept[[3]])

  # remove variables
  x_selected <- setdiff(x_selected, var_rm)

  # return
  dt[, c(x_selected, y), with=FALSE ]

}

