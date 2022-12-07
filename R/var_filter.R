dat_rm_reason = function(rmlst, var_rm = NULL, var_kp = NULL, lims = NULL) {
  . = rm_reason = value = variable = NULL

  # lims = list(info_value = 0.02, missing_rate = 0.95, identical_rate = 0.95, coef = 0, vif = 3, p = 0.05)
  dtlims = suppressWarnings(melt(setDT(lims), variable.name = 'rm_reason'))[
    , value := as.character(value)
  ][rm_reason %in% c('missing_rate', 'identical_rate', 'vif', 'p', 'cor'), value := paste0('>', value)
  ][rm_reason == 'coef', value := paste('is not', value)
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
    if (isFALSE(arglst[[n]]) || is.null(arglst[[n]])) {
      arglst0[[n]] = NULL
      next
    }

    arglst0[[n]] = arglst[[n]]
  }

  arglst = arglst0
  return(arglst)
}

#' Variable Filter
#'
#' This function filter variables base on specified conditions, such as missing rate, identical value rate, information value.
#'
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. Defaults to NULL. If x is NULL, then all columns except y are counted as x variables.
#' @param lims A list of variable filters' thresholds.
#' \itemize{
#'   \item \code{missing_rate} The missing rate of kept variables should <= 0.95 by defaults.
#'   \item \code{identical_rate} The identical value rate (excluding NAs) of kept variables should <= 0.95 by defaults.
#'   \item \code{info_value} The information value (iv) of kept variables should >= 0.02 by defaults.
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
  lims = list(missing_rate = 0.95, identical_rate = 0.95, info_value = 0.02),
  var_rm = NULL, var_kp = NULL, var_rm_reason = FALSE, positive = "bad|1", ... ) {

  # start time
  start_time = proc.time()

  rm_reason = NULL
  kwargs = list(...)
  # return_rm_reason param
  return_rm_reason = kwargs$return_rm_reason
  if (!is.null(return_rm_reason)) var_rm_reason = return_rm_reason
  # lims param
  if (is.numeric(kwargs$iv_limit)) lims$info_value = kwargs$iv_limit
  if (is.numeric(kwargs$missing_limit)) lims$missing_rate = kwargs$missing_limit
  if (is.numeric(kwargs$identical_limit)) lims$identical_rate = kwargs$identical_limit
  # lims
  lims = arglst_update(lims, arglst0 = list(missing_rate = 0.95, identical_rate = 0.95, info_value = 0.02))
  cli_inform(c(i = sprintf('Filtering variables via %s ...', paste(names(lims), collapse = ', '))))

  # set dt as data.table
  dt = setDT(copy(dt)) # copy(setDT(dt))
  if (!is.null(x)) dt = dt[, c(y,x), with=FALSE]
  # check y
  dt = check_y(dt, y, positive)
  # x variable names
  x = x_variable(dt,y,x)
  # variable keep
  if (length(setdiff(var_kp, x)) > 0) {
    warning("Incorrect inputs; there are ", length(setdiff(var_kp, x)), " var_kp variables are not exist in input data, which are removed from var_kp. \n", setdiff(var_kp, x))
  }
  x2 = setdiff(x, c(var_rm, var_kp))

  # variable filter via lims
  x_kp = NULL
  x_rm = NULL
  x_filter = NULL
  for (lim in names(lims)) {
    x_rmkp = do.call(paste0('var_filter_', lim), args = list(dt=dt, y=y, x=x2, lim = lims[[lim]]))

    x_kp[[lim]] = x_rmkp$xkp
    x_rm[[lim]] = data.table(variable = x_rmkp$xrm)
    x_filter[[lim]] = x_rmkp$filter

    if (length(x_rmkp$xrm)>0) {
      cat_bullet(
        c(sprintf("%s variables are removed via %s", length(x_rmkp$xrm), lim)),
        bullet = "tick", bullet_col = "green", col = 'grey'
      )
    }
  }

  # return dt
  x_kp2 = unique(c(Reduce(intersect, x_kp), var_kp))
  rtdt = dt[, c(x_kp2, y), with=FALSE ]

  # x remove reason
  if (isTRUE(var_rm_reason)) {
    x_filter$rm_reason = dat_rm_reason(rmlst = x_rm, var_rm = var_rm, var_kp = var_kp, lims = lims)

    fcols = c('info_value', 'missing_rate', 'identical_rate')
    rtrm = Reduce(
      function(x,y) merge(x,y, by='variable', all=TRUE), x_filter
    )[order(rm_reason)
    ][, (fcols) := lapply(.SD, function(x) round(x, 4)), .SDcols = fcols][]

    rt = list(dt = rtdt, rm = rtrm)
  } else {
    rt = rtdt
  }

  # running time
  rs = proc.time() - start_time
  # hms
  cat_bullet(
    c(sprintf("Variable filtering on %s rows and %s columns in %s", nrow(dt), length(x), sec_to_hms(rs[3])),
      sprintf("%s variables are removed in total", length(x)-length(x_kp2)) ),
    bullet = "tick", bullet_col = "green", col = 'grey'
  )

  return(rt)
}

var_filter_missing_rate = function(dt, y, x, lim=0.95) {
  variable = missing_rate = NULL

  dtxy = setDT(dt)[, c(x,y), with=FALSE]
  x_missing_rate = data.table(
    variable = x,
    missing_rate = dtxy[, sapply(.SD, function(a) mean(is.na(a))), .SDcols = x]
  )

  x_kp = x_missing_rate[missing_rate <= lim, variable]

  return(
    list(xkp = x_kp, xrm = setdiff(x, x_kp), filter=x_missing_rate)
  )
}

var_filter_identical_rate = function(dt, y, x, lim=0.95) {
  variable = identical_rate = NULL

  dtxy = setDT(dt)[, c(x,y), with=FALSE]
  x_identical_rate = data.table(
    variable = x,
    identical_rate = dtxy[, sapply(.SD, function(a) {
      pt = prop.table(table(a))
      max_rate = ifelse(length(pt) == 0, Inf, max(pt, na.rm = TRUE))
      return(max_rate)
    } ), .SDcols = x]
  )

  x_kp = x_identical_rate[identical_rate <= lim, variable]

  return(
    list(xkp = x_kp, xrm = setdiff(x, x_kp), filter=x_identical_rate)
  )
}

var_filter_info_value = function(dt, y, x, lim=0.02) {
  variable = info_value = NULL

  dtxy = setDT(dt)[, c(x,y), with=FALSE]
  x_info_value = iv(dtxy, y, x)

  x_kp = x_info_value[info_value >= lim, variable]

  return(
    list(xkp = x_kp, xrm = setdiff(x, x_kp), filter = x_info_value)
  )
}



# Variable Filter via lr
var_filter2 = function(
    dt, y, x=NULL,
    lims = list(coef = 'positive', step=TRUE, vif = 3, cor = 0.8), var_rm_miniv = TRUE,
    var_rm = NULL, var_kp = NULL, var_rm_reason = FALSE, positive = "bad|1", ...) {

  # running time start
  start_time = proc.time()

  variable = NULL
  show_vif = list(...)['show_vif']
  # lims
  lims = arglst_update(lims, arglst0 = list(coef = 'positive', step=TRUE, vif = 3, cor = 0.8))
  cli_inform(c(i = sprintf('Filtering variables via %s ...', paste(names(lims), collapse = ', '))))

  # dt
  dt = check_y(setDT(copy(dt)), y, positive)
  x = x_variable(dt,y,x)

  # variable filter via lims
  x_kp = setdiff(x, c(var_rm, var_kp))
  x_rm = NULL
  for (lim in names(lims)) {
    x_rmkp = do.call(paste0('var_filter_', lim), args = list(dt=dt, y=y, x=x_kp, lim = lims[[lim]], var_rm_miniv = var_rm_miniv))

    x_rm[[lim]] = data.table(variable = x_rmkp$xrm)
    x_kp = x_rmkp$xkp

    if (length(x_rmkp$xrm)>0) {
      cat_bullet(
        c(sprintf("%s variables are removed via %s", length(x_rmkp$xrm), lim)),
        bullet = "tick", bullet_col = "green", col = 'grey'
      )
    }
  }

  # df_vif
  df_vif=m1_vif(dt[,c(x_kp,y),with=FALSE], y)
  if (isTRUE(show_vif)) print(df_vif)

  # dat returned
  x_kp = unique(c(x_kp, var_kp))
  rtdt = dt[, c(x_kp, y), with=FALSE ]

  # var removed reason
  if (isTRUE(var_rm_reason)) {
    rtrm = dat_rm_reason(rmlst = x_rm, var_rm = var_rm, var_kp = var_kp, lims = lims)
    rtrm = rbind(rtrm, df_vif[variable != '(Intercept)'], fill=TRUE)

    rt = list(dt = rtdt, rm = rtrm)
  } else {
    rt = rtdt
  }

  # running time end
  rs = proc.time() - start_time
  # hms
  cat_bullet(
    c(sprintf("Variable filtering on %s rows and %s columns in %s", nrow(dt), ncol(dt)-1, sec_to_hms(rs[3])),
      sprintf("%s variables are removed in total", ncol(dt)-1-length(x_kp) )),
    bullet = "tick", bullet_col = "green", col = 'grey'
  )

  return(rt)
}

#' @importFrom stats step
var_filter_step = function(dt, y, x=NULL, ...) {

  if (is.null(x)) x = setdiff(names(dt), y)
  dtxy = setDT(dt)[, c(x,y), with=FALSE]

  m1 = m1_vif(dtxy, y, mobj=TRUE)
  m_step = step(m1, direction="both", trace = FALSE)
  m2 = eval(m_step$call)


  xkp_step = setdiff(names(coef(m2)), "(Intercept)")
  return(list(xkp = xkp_step, xrm = setdiff(x, xkp_step)))
}


var_filter_coef = function(dt, y, x=NULL, lim='positive', var_rm_miniv=TRUE, ...) {
  variable = Estimate = NULL

  if (is.null(x)) x = setdiff(names(dt), y)
  dtxy = setDT(dt)[, c(x,y), with=FALSE]
  if (var_rm_miniv) xiv = iv(dtxy, y, x)

  lim = match.arg(lim, c('positive', 'negative'))

  df_vif=m1_vif(dtxy,y)
  x_kp = df_vif[variable != '(Intercept)', variable]
  if (lim == 'positive') {
    while (df_vif[variable != '(Intercept)'][Estimate <= 0, .N>0]) {
      dt_xrm = df_vif[variable != '(Intercept)'][order(Estimate)]
      if (var_rm_miniv) {
        x_rm = xrm_miniv(dt_xrm[Estimate <= 0,variable], xiv)
      } else {
        x_rm = dt_xrm[1,variable]
      }

      x_kp = setdiff(x_kp, x_rm)
      dtxy = dtxy[, c(x_kp, y), with = FALSE]

      df_vif=m1_vif(dtxy,y)
    }
  } else if (lim == 'negative'){
    while (df_vif[variable != '(Intercept)'][Estimate >= 0, .N>0]) {
      dt_xrm = df_vif[variable != '(Intercept)'][order(-Estimate)]
      if (var_rm_miniv) {
        x_rm = xrm_miniv(dt_xrm[Estimate >= 0,variable], xiv)
      } else {
        x_rm = dt_xrm[1,variable]
      }

      x_kp = setdiff(x_kp, x_rm)
      dtxy = dt[, c(x_kp, y), with = FALSE]

      df_vif=m1_vif(dtxy,y)
    }
  }

  return(list(xkp = x_kp, xrm = setdiff(x, x_kp)))
}

var_filter_p = function(dt, y, x=NULL, lim=0.05, var_rm_miniv=FALSE) {
  variable = `Pr(>|z|)` = NULL

  if (is.null(x)) x = setdiff(names(dt), y)
  dtxy = setDT(dt)[, c(x,y), with=FALSE]
  if (var_rm_miniv) xiv = iv(dtxy, y, x)

  df_vif=m1_vif(dtxy,y)
  x_kp = x
  while (df_vif[variable != '(Intercept)'][`Pr(>|z|)` > lim, .N>0]) {
    dt_xrm = df_vif[variable != '(Intercept)'][order(-`Pr(>|z|)`)]
    if (var_rm_miniv) {
      x_rm = xrm_miniv(dt_xrm[`Pr(>|z|)` > lim, variable], xiv)
    } else {
      x_rm = dt_xrm[1, variable]
    }

    x_kp = setdiff(x_kp, x_rm)
    dtxy = dtxy[, c(x_kp, y), with = FALSE]

    df_vif=m1_vif(dtxy,y)
  }

  return(list(xkp = x_kp, xrm = setdiff(x, x_kp)))
}


var_filter_vif = function(dt, y, x=NULL, lim=3, var_rm_miniv=TRUE) {
  variable = gvif = NULL

  if (is.null(x)) x = setdiff(names(dt), y)
  dtxy = setDT(dt)[, c(x,y), with=FALSE]
  if (var_rm_miniv) xiv = iv(dtxy, y, x)

  df_vif=m1_vif(dtxy,y)
  x_kp = x
  while (df_vif[variable != '(Intercept)'][gvif > lim, .N>0]) {
    dt_xrm = df_vif[variable != '(Intercept)'][order(-gvif)]
    if (var_rm_miniv) {
      x_rm = xrm_miniv(dt_xrm[gvif > lim,variable], xiv)
    } else {
      x_rm = dt_xrm[1,variable]
    }

    x_kp = setdiff(x_kp, x_rm)
    dtxy = dtxy[, c(x_kp, y), with = FALSE]

    df_vif=m1_vif(dtxy,y)
  }

  return(list(xkp = x_kp, xrm = setdiff(x, x_kp)))
}

var_filter_cor = function(dt, y, x=NULL, lim=0.6, var_rm_miniv=TRUE) {
  value = x1 = x2 = . = V1 = cid = rid = NULL

  if (is.null(x)) x = setdiff(names(dt), y)
  dtxy = setDT(dt)[, c(x,y), with=FALSE]
  if (var_rm_miniv) xiv = iv(dtxy, y, x)

  x_kp_num = cols_type(dtxy[, x, with=FALSE], 'numeric')
  x_kp_cat = setdiff(x, x_kp_num)

  dtcor = cor2(dtxy, x_kp_num, uptri = TRUE, long = TRUE)[!is.na(value) & rid != cid][order(-abs(value))][]
  while (dtcor[abs(value) > lim, .N>0]) {
    if (var_rm_miniv) {
      x_rm = xrm_miniv(dtcor[1, c(x1,x2)], xiv)
    } else {
      x_rm = rbind(
        dtcor[x1 == dtcor[1,x1], mean(abs(value)), by = .(x=x1)],
        dtcor[x2 == dtcor[1,x2], mean(abs(value)), by = .(x=x2)]
      )[order(-V1)][1,x]
    }


    x_kp_num = setdiff(x_kp_num, x_rm)
    dtcor = dtcor[!(x1 == x_rm | x2 == x_rm)]
  }

  x_kp = c(x_kp_num, x_kp_cat)
  return(
    list(xkp = x_kp, xrm = setdiff(x, x_kp))
  )
}




m1_vif = function(dt, y, mobj=FALSE) {
  m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dt)

  xrm = names(which(is.na(coef(m1))))
  while (length(xrm) > 0) {
    # print(xrm)
    dt = dt[, (xrm) := NULL]
    m1 = glm(as.formula(sprintf('%s ~ .', y)), family = "binomial", data = dt)
    xrm = names(which(is.na(coef(m1))))
  }

  if (isTRUE(mobj)) return(m1)
  df_vif = vif(m1, merge_coef = TRUE)
  return(df_vif)
}

# uptri: upper triangular
# lotri: lower triangular
# diag: diagonal
cor2 = function(dt, x=NULL, uptri = FALSE, lotri = FALSE, diag = FALSE, long=FALSE) {
  . = x1 = cid = rid = rcid = NULL

  dt = setDT(dt)
  if (is.null(x)) x = names(dt)
  numcols = names(which(dt[,sapply(.SD, is.numeric)]))

  cor_mt = cor(dt[,intersect(x,numcols),with=FALSE], use='pairwise.complete.obs')
  if (uptri) {
    cor_mt[!upper.tri(cor_mt, diag)] = NA
  } else if (lotri) {
    cor_mt[!lower.tri(cor_mt, diag)] = NA
  }

  cor_dt = setDT(as.data.frame(cor_mt), keep.rownames=TRUE)
  setnames(cor_dt, 'rn', 'x1')

  if (long) {
    cor_dt = melt(cor_dt, id.vars = 'x1', variable.name = 'x2')[
      cor_dt[,.(x1)][,rid:=.I][], on = 'x1'
    ][cor_dt[,.(x2=x1)][,cid:=.I][], on = 'x2'
    ][, rcid := sprintf('%s_%s',rid,cid)
    ]
  }

  return(cor_dt)
}

xrm_miniv = function(x, xiv) {
  variable=info_value=NULL
  xiv[variable %in% x][order(info_value)][1,variable]
}
