dat_rm_reason = function(rmlst, var_rm = NULL, var_kp = NULL, lims = NULL) {
  . = rm_reason = value = variable = NULL

  # lims = list(info_value = 0.02, missing_rate = 0.95, identical_rate = 0.95, coef = 0, vif = 3, p = 0.05)
  dtlims = suppressWarnings(melt(setDT(lims), variable.name = 'rm_reason'))[
    , value := as.character(value)
  ][rm_reason %in% c('missing_rate', 'identical_rate', 'vif', 'p'), value := paste0('>', value)
  ][rm_reason == 'coef', value := paste('<=', value)
  ][rm_reason == 'info_value', value := paste0('<', value)
  ][rm_reason == 'step', value := NA]

  # force removed variable
  if (!is.null(var_rm)) rmlst$force_removed = data.table(variable=var_rm)

  # variable remove reason
  rmdt = merge(
    rbindlist(rmlst, idcol = 'rm_reason'),
    dtlims, by = 'rm_reason', all.x = TRUE
  )[!is.na(value) & !is.na(rm_reason), rm_reason := paste0(rm_reason, value)
  ][, .(rm_reason = paste0(rm_reason, collapse = ', ')), by = 'variable']

  # force kept variable
  if (!is.null(var_kp)) rmdt = rmdt[!(variable %in% var_kp)]

  return(rmdt)
}

arglst_update = function(arglst, arglst0) {
  for ( n in intersect(names(arglst), names(arglst0)) ) {
    arglst0[[n]] = arglst[[n]]
  }
  arglst = arglst0
  return(arglst)
}

#' Variable Filter
#'
#' This function filter variables base on specified conditions, such as information value, missing rate, identical value rate.
#'
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. Defaults to NULL. If x is NULL, then all columns except y are counted as x variables.
#' @param lims A list of variable filters' thresholds.
#' \itemize{
#'   \item \code{info_value} The information value (iv) of kept variables should >= 0.02 by defaults.
#'   \item \code{missing_rate} The missing rate of kept variables should <= 0.95 by defaults.
#'   \item \code{identical_rate} The identical value rate (excluding NAs) of kept variables should <= 0.95 by defaults.
#' }
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
  lims = list(info_value = 0.02, missing_rate = 0.95, identical_rate = 0.95),
  var_rm = NULL, var_kp = NULL, var_rm_reason = FALSE, positive = "bad|1", ... ) {
  # start time
  start_time = proc.time()


  # no visible binding for global variable
  . = info_value = variable = rm_reason = NULL

  kwargs = list(...)
  # return_rm_reason param
  return_rm_reason = kwargs$return_rm_reason
  if (!is.null(return_rm_reason)) var_rm_reason = return_rm_reason
  # lims param
  if (is.numeric(kwargs$iv_limit)) lims$info_value = kwargs$iv_limit
  if (is.numeric(kwargs$missing_limit)) lims$missing_rate = kwargs$missing_limit
  if (is.numeric(kwargs$identical_limit)) lims$identical_rate = kwargs$identical_limit
  # lims
  lims = arglst_update(lims, arglst0 = list(info_value = 0.02, missing_rate = 0.95, identical_rate = 0.95))
  cli_inform(c(i = sprintf('Filtering variables via %s ...', paste(names(lims), collapse = ', '))))

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
  x_kp = x

  # force removed variables
  if (!is.null(var_rm))  x_kp = setdiff(x_kp, var_rm)
  # check force kept variables
  if (!is.null(var_kp)) {
    var_kp2 = intersect(var_kp, x_kp)
    len_diff = length(var_kp) - length(var_kp2)
    if (len_diff > 0) {
      warning("Incorrect inputs; there are ", len_diff, " var_kp variables are not exist in input data, which are removed from var_kp. \n", setdiff(var_kp, var_kp2))
    }
    var_kp = var_kp2
  }

  # -iv
  iv_list = iv(dt, y, x_kp)
  # -na percentage
  missing_rate = dt[, sapply(.SD, function(a) mean(is.na(a))), .SDcols = x_kp] # sum(is.na(a))/length(a)
  # -element percentage
  identical_rate = dt[, sapply(.SD, function(a) {
    pt = prop.table(table(a))
    max_rate = ifelse(length(pt) == 0, Inf, max(pt, na.rm = TRUE))
    return(max_rate)
  } ), .SDcols = x_kp]

  # datatable  iv na ele
  dt_var_selector =
    iv_list[data.table(variable = names(missing_rate), missing_rate = missing_rate), on="variable"
    ][data.table(variable = names(identical_rate), identical_rate = identical_rate), on="variable"]

  # remove missing_rate>95 | identical_rate>0.95 | iv<0.02
  # variable datatable selected
  dt_var_sel = dt_var_selector
  if ('info_value'     %in% names(lims)) dt_var_sel = dt_var_sel[info_value >= lims$info_value]
  if ('missing_rate'   %in% names(lims)) dt_var_sel = dt_var_sel[missing_rate <= lims$missing_rate]
  if ('identical_rate' %in% names(lims)) dt_var_sel = dt_var_sel[identical_rate <= lims$identical_rate]
  x_kp = dt_var_sel[, as.character(variable)]

  # add kept variable
  if (!is.null(var_kp))  x_kp = unique(c(x_kp, var_kp))
  rtdt = dt[, c(x_kp, y), with=FALSE ]

  # variable removed reason
  if (var_rm_reason) {
    rmlst = list(
      info_value = dt_var_selector[info_value < lims$info_value, .(variable)],
      missing_rate = dt_var_selector[missing_rate > lims$missing_rate, .(variable)],
      identical_rate = dt_var_selector[identical_rate > lims$identical_rate, .(variable)]
    )
    dtrm = dat_rm_reason(rmlst = rmlst, var_rm = var_rm, var_kp = var_kp, lims = lims)

    fcols = c('info_value', 'missing_rate', 'identical_rate')
    rtrm = merge(
      dtrm, dt_var_selector, all.y = TRUE
    )[order(rm_reason)
    ][, (fcols) := lapply(.SD, function(x) round(x, 4)), .SDcols = fcols][]

    # return
    rt = list(dt = rtdt, rm = rtrm)
  } else {
    rt = rtdt
  }

  # running time
  rs = proc.time() - start_time
  # hms
  cat_bullet(
    c(sprintf("Variable filtering on %s rows and %s columns in %s", nrow(dt), length(x), sec_to_hms(rs[3])),
      sprintf("%s variables are removed", length(x)-length(x_kp)) ),
    bullet = "tick", bullet_col = "green", col = 'grey'
  )

  return(rt)
}



# Variable Filter via lr
var_filter2 = function(
    dt, y, x=NULL,
    step = TRUE, lims = list(coef = 0, vif = 3, p = 0.05),
    var_rm = NULL, var_kp = NULL, var_rm_reason = FALSE, positive = "bad|1", ...) {

  # lims
  lims = arglst_update(lims, arglst0 = list(coef = 0, vif = 3, p = 0.05))

  # dt
  dt = setDT(copy(dt))
  if (!is.null(x)) dt = dt[, c(y,x), with=FALSE]
  # check y
  dt = check_y(dt, y, positive)

  x_kp = x_variable(dt,y,x)
  x_kp = setdiff(x_kp, c(var_rm, var_kp))

  # step
  if (step == TRUE) {
    dt2 = dt[, c(x_kp, y), with=FALSE ]
    lrx_filter1 = var_filter_step(dt2, y = y, x = x_kp)
    x_kp = lrx_filter1$xkp
  }

  # vif
  dt2 = dt[, c(x_kp, y), with=FALSE ]
  lrx_filter2 =  var_filter_vif(dt2, y = y, x = x_kp, lims = lims)
  x_kp = lrx_filter2$xkp

  # dat returned
  if (!is.null(var_kp))  x_kp = unique(c(x_kp, var_kp))
  rtdt = dt[, c(x_kp, y), with=FALSE ]

  # var removed reason
  if (isTRUE(var_rm_reason)) {
    vfxrm = lrx_filter2$xrm
    if (step == TRUE) vfxrm = c(lrx_filter1$xrm, vfxrm)
    rtrm = dat_rm_reason(rmlst = vfxrm, var_rm = var_rm, var_kp = var_kp, lims = lims)
    rtrm = rbind(rtrm, lrx_filter2$df_vif[-1], fill=TRUE)
    rt = list(dt = rtdt, rm = rtrm)
  } else {
    rt = rtdt
  }

  return(rt)
}

#' @importFrom stats step
var_filter_step = function(dt, y, x=NULL, show_vif=FALSE) {
  start_time = proc.time()
  cli_inform(c(i = 'Filtering variables via step ...'))

  dt = setDT(copy(dt))
  if (is.null(x)) x = setdiff(names(dt), y)
  dtxy = dt[, c(x, y), with = FALSE]

  m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dtxy)

  m_step = step(m1, direction="both", trace = FALSE)
  m2 = eval(m_step$call)

  df_vif = vif(m1, merge_coef = TRUE)
  if (show_vif) print(df_vif)

  xkp_step = names(coef(m2))[-1]
  xrm_step = list(step = data.table(variable = setdiff(x, xkp_step)))

  # running time
  rs = proc.time() - start_time
  # hms
  cat_bullet(
    c(sprintf("Variable filtering on %s rows and %s columns in %s", nrow(dt), length(x), sec_to_hms(rs[3])),
      sprintf("%s variables are removed", length(x)-length(xkp_step))),
    bullet = "tick", bullet_col = "green", col = 'grey'
  )

  return(list(xkp = xkp_step, xrm = xrm_step, df_vif = df_vif))
}


var_filter_vif = function(dt, y, x=NULL, lims = list(coef = 0, vif = 3, p = 0.05), show_vif=FALSE) {
  start_time = proc.time()
  Estimate = variable = gvif = NULL

  cli_inform(c(i = sprintf('Filtering variables via %s ...', paste(names(lims), collapse = ', '))))

  dt = setDT(copy(dt))
  if (is.null(x)) x = setdiff(names(dt), y)
  dtxy = dt[, c(x, y), with = FALSE]

  m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dtxy)
  df_vif = vif(m1, merge_coef = TRUE)

  # coefficients
  x_kp = x
  xrm_coef = NULL
  if ('coef' %in% names(lims)) {
    while (df_vif[-1][Estimate <= lims$coef, .N>0]) {
      x_kp = setdiff(x_kp, df_vif[-1][Estimate <= lims$coef,][order(-gvif)][1,variable])
      dtxy = dt[, c(x_kp, y), with = FALSE]

      m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dtxy)
      df_vif = vif(m1, merge_coef = TRUE)
    }
    xrm_coef = setdiff(x, names(coef(m1))[-1])
  }

  # vif
  x_kp = x
  xrm_vif = NULL
  if ('vif' %in% names(lims)) {
    while (df_vif[gvif > lims$vif, .N>0]) {
      x_kp = setdiff(x_kp, df_vif[-1][gvif == max(gvif), variable])
      dtxy = dt[, c(x_kp, y), with = FALSE]

      m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dtxy)
      df_vif = vif(m1, merge_coef = TRUE)
    }
    xrm_vif = setdiff(x, names(coef(m1))[-1])
  }


  # p
  x_kp = x
  xrm_p = NULL
  if ('p' %in% names(lims)) {
    while (df_vif[get("Pr(>|z|)") > lims$p, .N>0]) {
      x_kp = setdiff(x_kp, df_vif[-1][ get("Pr(>|z|)") == max(df_vif[-1][["Pr(>|z|)"]]), variable])
      dtxy = dt[, c(x_kp, y), with = FALSE]

      m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dtxy)
      df_vif = vif(m1, merge_coef = TRUE)
    }
    xrm_p = setdiff(x, names(coef(m1))[-1])
  }

  # xkp
  xkp = setdiff(x, unique(c(xrm_coef, xrm_vif, xrm_p)))

  # xrm
  if (show_vif) print(df_vif)
  xrm = list(coef = data.table(variable = xrm_coef),
             vif  = data.table(variable = xrm_vif),
             p    = data.table(variable = xrm_p))

  # running time
  rs = proc.time() - start_time
  # hms
  cat_bullet(
    c(sprintf("Variable filtering on %s rows and %s columns in %s", nrow(dt), length(x), sec_to_hms(rs[3])),
      sprintf("%s variables are removed", length(x)-length(xkp))),
    bullet = "tick", bullet_col = "green", col = 'grey'
  )

  return(list(xkp = xkp, xrm = xrm, df_vif = df_vif))
}

