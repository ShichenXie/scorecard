#' Variable Filter
#'
#' This function filter variables base on specified conditions, such as information value, missing rate, identical value rate.
#'
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. Default is NULL. If x is NULL, then all variables except y are counted as x variables.
#' @param iv_limit The information value of kept variables should >= iv_limit. The default is 0.02.
#' @param missing_limit The missing rate of kept variables should <= missing_limit. The default is 0.95.
#' @param identical_limit The identical value rate (excluding NAs) of kept variables should <= identical_limit. The default is 0.95.
#' @param var_rm Name of force removed variables, default is NULL.
#' @param var_kp Name of force kept variables, default is NULL.
#' @param return_rm_reason Logical, default is FALSE.
#' @param positive Value of positive class, default is "bad".
#' @return A data.table with y and selected x variables and a data.table with the reason of removed x variable if return_rm_reason == TRUE.
#'
#' @examples
#' # Load German credit data
#' data(germancredit)
#'
#' # variable filter
#' dt_sel = var_filter(germancredit, y = "creditability")
#' dim(dt_sel)
#'
#' # return the reason of varaible removed
#' dt_sel2 = var_filter(germancredit, y = "creditability", return_rm_reason = TRUE)
#' lapply(dt_sel2, dim)
#'
#' str(dt_sel2$dt)
#' str(dt_sel2$rm)
#'
#' @import data.table
#' @export
#'
var_filter = function(dt, y, x = NULL, iv_limit = 0.02, missing_limit = 0.95, identical_limit = 0.95, var_rm = NULL, var_kp = NULL, return_rm_reason = FALSE, positive="bad") {
  # start time
  start_time = proc.time()

  # no visible binding for global variable
  . = info_value = variable = rt = rm_reason = NULL

  # set dt as data.table
  dt = setDT(dt)
  # remove date/time col
  dt = rmcol_datetime_unique1(dt)
  # replace "" by NA
  dt = rep_blank_na(dt)
  # check y
  dt = check_y(dt, y, positive)
  # x variable names
  x = x_variable(dt,y,x)

  # force removed variables
  if (!is.null(var_rm))  x = setdiff(x, var_rm)
  # check force kept variables
  if (!is.null(var_kp)) {
    var_kp2 = intersect(var_kp, x)
    len_diff = length(var_kp) - length(var_kp2)
    if (len_diff > 0) {
      warning("Incorrect inputs; there are ", len_diff, " var_kp variables are not exist in input data, which are removed from var_kp. \n", setdiff(var_kp, var_kp2))
    }
    var_kp = var_kp2
  }

  # -iv
  iv_list = iv(dt, y, x)
  # -na percentage
  missing_rate = dt[, sapply(.SD, function(a) sum(is.na(a))/length(a)), .SDcols = x]
  # -element percentage
  identical_rate = dt[, sapply(.SD, function(a) max(table(a))/sum(!is.na(a)) ), .SDcols = x]

  # datatable  iv na ele
  dt_var_selector =
    iv_list[data.table(variable = names(missing_rate), missing_rate = missing_rate), on="variable"
    ][data.table(variable = names(identical_rate), identical_rate = identical_rate), on="variable"]

  # remove missing_rate>95 | identical_rate>0.95 | iv<0.02
  # variable datatable selected
  dt_var_sel = dt_var_selector[info_value >= iv_limit & missing_rate <= missing_limit & identical_rate <= identical_limit]

  # add kept variable
  x_selected = dt_var_sel[, as.character(variable)]
  if (!is.null(var_kp))  x_selected = unique(c(x_selected, var_kp))


  if (return_rm_reason) {
    # variable datatable deleted
    dt_var_rm = dt_var_selector[
      info_value < iv_limit | missing_rate > missing_limit | identical_rate > identical_limit
    ][, `:=`(
      info_value = ifelse(info_value < iv_limit, paste0("iv < ", iv_limit), ""),
      missing_rate = ifelse(missing_rate > missing_limit, paste0("miss rate > ",missing_limit), ""),
      identical_rate = ifelse(identical_rate > identical_limit, paste0("identical rate > ", identical_limit), "")
    )]

    dt_rm_reason = melt(
      dt_var_rm, id.vars = "variable", variable.name="var", value.name="rm_reason", variable.factor=TRUE
    )[rm_reason != ""][
      ,.(rm_reason=paste0(rm_reason, collapse=",")), by="variable"]

    if (!is.null(var_rm)) {
      dt_rm_reason = rbind(
        dt_rm_reason,
        data.table(variable=var_rm, rm_reason="force remove")
      )
    }
    if (!is.null(var_kp)) {
      dt_rm_reason = dt_rm_reason[!(variable %in% var_kp)]
    }

    dt_rm_reason = merge(dt_rm_reason, dt_var_selector, all.y = TRUE)[order(rm_reason)]
    # return
    rt$dt = dt[, c(x_selected, y), with=FALSE ]
    rt$rm = dt_rm_reason
  } else {
    rt = dt[, c(x_selected, y), with=FALSE ]
  }

  # running time
  rs = proc.time() - start_time
  # hms
  if (rs[3] > 10) cat(sprintf("Variable filtering on %s rows and %s columns in %s \n%s variables are removed", nrow(dt),ncol(dt),sec_to_hms(rs[3]), ncol(dt)-length(x_selected)-1),"\n")

  return(rt)
}

