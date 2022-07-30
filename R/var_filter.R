#' Variable Filter
#'
#' This function filter variables base on specified conditions, such as information value, missing rate, identical value rate.
#'
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. Defaults to NULL. If x is NULL, then all columns except y are counted as x variables.
#' @param iv_limit The information value of kept variables should >= iv_limit. The Defaults to 0.02.
#' @param missing_limit The missing rate of kept variables should <= missing_limit. The Defaults to 0.95.
#' @param identical_limit The identical value rate (excluding NAs) of kept variables should <= identical_limit. The Defaults to 0.95.
#' @param var_rm Name of force removed variables, Defaults to NULL.
#' @param var_kp Name of force kept variables, Defaults to NULL.
#' @param var_rm_reason Logical, Defaults to FALSE.
#' @param positive Value of positive class, Defaults to "bad|1".
#' @param ... Additional parameters.
#'
#' @return A data frame with columns for y and selected x variables, and a data frame with columns for remove reason if var_rm_reason is TRUE.
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
#' dt_sel2 = var_filter(germancredit, y = "creditability", var_rm_reason = TRUE)
#' lapply(dt_sel2, dim)
#'
#' str(dt_sel2$dt)
#' str(dt_sel2$rm)
#'
#' # keep columns manually, such as rowid
#' germancredit$rowid = row.names(germancredit)
#' dt_sel3 = var_filter(germancredit, y = "creditability", var_kp = 'rowid')
#'
#' # remove columns manually
#' dt_sel4 = var_filter(germancredit, y = "creditability", var_rm = 'rowid')
#'
#' @import data.table
#' @export
#'
var_filter = function(
  dt, y, x = NULL,
  iv_limit = 0.02, missing_limit = 0.95, identical_limit = 0.95,
  var_rm = NULL, var_kp = NULL, var_rm_reason = FALSE, positive = "bad|1", ... ) {
  # start time
  start_time = proc.time()
  cat('[INFO] filtering variables ... \n')

  # no visible binding for global variable
  . = info_value = variable = rt = rm_reason = NULL

  kwargs = list(...)
  return_rm_reason = kwargs$return_rm_reason
  if (!is.null(return_rm_reason)) var_rm_reason = return_rm_reason
  # set dt as data.table
  dt = setDT(copy(dt)) # copy(setDT(dt))
  if (!is.null(x)) dt = dt[, c(y,x), with=FALSE]
  # check y
  dt = check_y(dt, y, positive)
  # # remove date/time col
  # dt = rmcol_datetime_unique1(dt)
  # # replace "" by NA
  # dt = rep_blank_na(dt)
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
  missing_rate = dt[, sapply(.SD, function(a) mean(is.na(a))), .SDcols = x] # sum(is.na(a))/length(a)
  # -element percentage
  identical_rate = dt[, sapply(.SD, function(a) {
    pt = prop.table(table(a))
    max_rate = ifelse(length(pt) == 0, Inf, max(pt, na.rm = TRUE))
    return(max_rate)
  } ), .SDcols = x]

  # datatable  iv na ele
  dt_var_selector =
    iv_list[data.table(variable = names(missing_rate), missing_rate = missing_rate), on="variable"
    ][data.table(variable = names(identical_rate), identical_rate = identical_rate), on="variable"]

  # remove missing_rate>95 | identical_rate>0.95 | iv<0.02
  # variable datatable selected
  dt_var_sel = dt_var_selector[info_value >= iv_limit & missing_rate <= missing_limit & identical_rate <= identical_limit]
  x_selected = dt_var_sel[, as.character(variable)]

  # filter variables via step and vif
  lrx_rm = NULL
  if ( isTRUE(kwargs$lr) ) {
    lrx_filter1 = var_filter_step(dt[, c(x_selected,y), with=F], y = y, x = x_selected)
    x_selected = lrx_filter1$xkp
    lrx_filter2 =  var_filter_vif(dt[, c(x_selected,y), with=F], y = y, x = x_selected)
    x_selected = lrx_filter2$xkp

    lrx_rm = rbindlist(
      lapply(c(lrx_filter1$xrm, lrx_filter2$xrm), function(x) data.table(variable = x)),
      idcol = 'rm_reason'
    )
  }

  # add kept variable
  if (!is.null(var_kp))  x_selected = unique(c(x_selected, var_kp))
  rtdt = dt[, c(x_selected, y), with=FALSE ]

  # variable removed reason
  if (var_rm_reason) {
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

    # force removed or kept variable
    if (!is.null(var_rm)) {
      dt_rm_reason = rbind(
        dt_rm_reason,
        data.table(variable=var_rm, rm_reason="force remove")
      )
    }
    if (!is.null(var_kp)) {
      dt_rm_reason = dt_rm_reason[!(variable %in% var_kp)]
    }
    # removed variable via step/vif
    dt_rm_reason = rbind(dt_rm_reason, lrx_rm)
    fcols = c('info_value', 'missing_rate', 'identical_rate')
    dt_rm_reason = merge(
      dt_rm_reason, dt_var_selector, all.y = TRUE
    )[order(rm_reason)
    ][, (fcols) := lapply(.SD, function(x) round(x, 4)), .SDcols = fcols]

    # return
    rt$dt = rtdt
    rt$rm = dt_rm_reason
  } else {
    rt = rtdt
  }

  # running time
  rs = proc.time() - start_time
  # hms
  if (rs[3] > 10) cat(sprintf("[INFO] Variable filtering on %s rows and %s columns in %s \n%s variables are removed", nrow(dt),ncol(dt),sec_to_hms(rs[3]), ncol(dt)-length(x_selected)-1),"\n")

  return(rt)
}



#' @importFrom stats step
var_filter_step = function(dt, y, x=NULL, show_vif=FALSE) {
  dt = setDT(copy(dt))
  if (is.null(x)) x = setdiff(names(dt), y)
  dtxy = dt[, c(x, y), with = FALSE]

  m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dtxy)

  m_step = step(m1, direction="both", trace = FALSE)
  m2 = eval(m_step$call)

  if (show_vif) {
    df_vif = vif(m1, merge_coef = TRUE)
    print(df_vif)
  }

  xkp_step = names(coef(m2))[-1]
  xrm_step = setdiff(x, xkp_step)
  return(list(xkp = xkp_step, xrm = list(step = xrm_step)))
}


var_filter_vif = function(dt, y, x=NULL, coef_limit = 0, vif_limit = 3, p_limit = 0.05, show_vif=FALSE) {
  Estimate = variable = gvif = NULL

  dt = setDT(copy(dt))
  if (is.null(x)) x = setdiff(names(dt), y)
  dtxy = dt[, c(x, y), with = FALSE]

  m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dtxy)
  df_vif = vif(m1, merge_coef = TRUE)

  # coefficients
  while (df_vif[-1][Estimate < coef_limit, .N>0]) {
    x = setdiff(x, df_vif[-1][Estimate < 0,][order(-gvif)][1,variable])
    dtxy = dt[, c(x, y), with = FALSE]

    m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dtxy)
    df_vif = vif(m1, merge_coef = TRUE)
  }
  xkp_coef = names(coef(m1))[-1]
  xrm_coef = setdiff(x, xkp_coef)

  # vif
  x = xkp_coef
  while (df_vif[gvif >= vif_limit, .N>0]) {
    x = setdiff(x, df_vif[-1][gvif == max(gvif), variable])
    dtxy = dt[, c(x, y), with = FALSE]

    m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dtxy)
    df_vif = vif(m1, merge_coef = TRUE)
  }
  xkp_vif = names(coef(m1))[-1]
  xrm_vif = setdiff(x, xkp_vif)

  # p
  x = xkp_vif
  while (df_vif[get("Pr(>|z|)") >= p_limit, .N>0]) {
    x = setdiff(x, df_vif[-1][ get("Pr(>|z|)") == max(df_vif[-1][["Pr(>|z|)"]]), variable])
    dtxy = dt[, c(x, y), with = FALSE]

    m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dtxy)
    df_vif = vif(m1, merge_coef = TRUE)
  }
  xkp_p = names(coef(m1))[-1]
  xrm_p = setdiff(x, xkp_p)

  if (show_vif) print(df_vif)
  xrm = list(coef = xrm_coef, vif = xrm_vif, p = xrm_p)
  return(list(xkp = xkp_p, xrm = xrm))
}

