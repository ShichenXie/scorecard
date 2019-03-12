# coefficients in scorecard
ab = function(points0=600, odds0=1/19, pdo=50) {
  # sigmoid function
  # library(ggplot2)
  # ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = function(x) 1/(1+exp(-x)))

  # log_odds function
  # ggplot(data.frame(x = c(0, 1)), aes(x)) + stat_function(fun = function(x) log(x/(1-x)))

  # logistic function
  # p(y=1) = 1/(1+exp(-z)),
      # z = beta0+beta1*x1+...+betar*xr = beta*x
  ##==> z = log(p/(1-p)),
      # odds = p/(1-p) # bad/good <==>
      # p = odds/1+odds
  ##==> z = log(odds)
  ##==> score = a - b*log(odds)

  # two hypothesis
  # points0 = a - b*log(odds0)
  # points0 - PDO = a - b*log(2*odds0)

  b = pdo/log(2)
  a = points0 + b*log(odds0) #log(odds0/(1+odds0))

  return(list(a=a, b=b))
}
#' Creating a Scorecard
#'
#' \code{scorecard} creates a scorecard based on the results from \code{woebin} and \code{glm}.
#'
#' @param bins Binning information generated from \code{woebin} function.
#' @param model A glm model object.
#' @param points0 Target points, default 600.
#' @param odds0 Target odds, default 1/19. Odds = p/(1-p).
#' @param pdo Points to Double the Odds, default 50.
#' @param basepoints_eq0 Logical, default is FALSE. If it is TRUE, the basepoints will equally distribute to each variable.
#' @return A scorecard data frame
#'
#' @seealso \code{\link{scorecard2}} \code{\link{scorecard_ply}}
#'
#' @examples
#' \dontrun{
#' # load germancredit data
#' data("germancredit")
#'
#' # filter variable via missing rate, iv, identical value rate
#' dt_sel = var_filter(germancredit, "creditability")
#'
#' # woe binning ------
#' bins = woebin(dt_sel, "creditability")
#' dt_woe = woebin_ply(dt_sel, bins)
#'
#' # glm ------
#' m = glm(creditability ~ ., family = binomial(), data = dt_woe)
#' # summary(m)
#'
#' # Select a formula-based model by AIC
#' m_step = step(m, direction="both", trace=FALSE)
#' m = eval(m_step$call)
#' # summary(m)
#'
#' # predicted proability
#' # dt_pred = predict(m, type='response', dt_woe)
#'
#' # performace
#' # ks & roc plot
#' # perf_eva(dt_woe$creditability, dt_pred)
#'
#' # scorecard
#' # Example I # creat a scorecard
#' card = scorecard(bins, m)
#' card2 = scorecard2(bins=bins, dt=germancredit, y='creditability',
#'   x=c("status.of.existing.checking.account", "duration.in.month", "credit.history",
#'    "purpose", "credit.amount", "savings.account.and.bonds",
#'    "present.employment.since", "installment.rate.in.percentage.of.disposable.income",
#'    "personal.status.and.sex", "other.debtors.or.guarantors", "property",
#'    "age.in.years", "other.installment.plans", "housing"))
#'
#' # credit score
#' # Example I # only total score
#' score1 = scorecard_ply(dt, card)
#'
#' # Example II # credit score for both total and each variable
#' score2 = scorecard_ply(dt, card, only_total_score = F)
#' }
#' @import data.table
#' @export
scorecard = function(bins, model, points0=600, odds0=1/19, pdo=50, basepoints_eq0=FALSE) {
  # global variables or functions
  variable = var_woe = Estimate = points = woe = NULL

  # coefficients
  aabb = ab(points0, odds0, pdo)
  a = aabb$a
  b = aabb$b
  # odds = pred/(1-pred); score = a - b*log(odds)

  # bins # if (is.list(bins)) rbindlist(bins)
  if (!is.data.table(bins)) {
    if (is.data.frame(bins)) {
      bins = setDT(bins)
    } else {
      bins = rbindlist(bins, fill = TRUE)
    }
  }

  # coefficients
  coef_dt = data.table(var_woe = names(coef(model)), Estimate = coef(model))[, variable := sub("_woe$", "", var_woe) ][]

  # scorecard
  basepoints = a - b*coef_dt[1,Estimate]

  card = list()
  if (basepoints_eq0) {
    card[["basepoints"]] = data.table( variable = "basepoints", bin = NA, woe = NA, points = 0 )

    for (i in coef_dt[-1,variable]) {
      card[[i]] = bins[variable==i][, points := round(-b*coef_dt[variable==i, Estimate]*woe + basepoints/coef_dt[,.N-1])]
    }
  } else {
    card[["basepoints"]] = data.table( variable = "basepoints", bin = NA, woe = NA, points = round(basepoints) )

    for (i in coef_dt[-1,variable]) {
      card[[i]] = bins[variable==i][, points := round(-b*coef_dt[variable==i, Estimate]*woe)]
    }
  }

  return(card)
}

#' Creating a Scorecard
#'
#' \code{scorecard2} creates a scorecard based on the results from \code{woebin}. It has the same function with \code{scorecard}, but without model object input.
#'
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. If it is NULL, then all variables in bins are used. Default is NULL.
#' @param bins Binning information generated from \code{woebin} function.
#' @param points0 Target points, default 600.
#' @param odds0 Target odds, default 1/19. Odds = p/(1-p).
#' @param pdo Points to Double the Odds, default 50.
#' @param basepoints_eq0 Logical, default is FALSE. If it is TRUE, the basepoints will equally distribute to each variable.
#' @param positive Value of positive class, default "bad|1".
#' @param ... Additional parameters.
#' @return A scorecard data frame
#'
#' @seealso \code{\link{scorecard}} \code{\link{scorecard_ply}}
#'
#' @examples
#' \dontrun{
#' # load germancredit data
#' data("germancredit")
#'
#' # filter variable via missing rate, iv, identical value rate
#' dt_sel = var_filter(germancredit, "creditability")
#'
#' # woe binning ------
#' bins = woebin(dt_sel, "creditability")
#' dt_woe = woebin_ply(dt_sel, bins)
#'
#' # glm ------
#' m = glm(creditability ~ ., family = binomial(), data = dt_woe)
#' # summary(m)
#'
#' # Select a formula-based model by AIC
#' m_step = step(m, direction="both", trace=FALSE)
#' m = eval(m_step$call)
#' # summary(m)
#'
#' # predicted proability
#' # dt_pred = predict(m, type='response', dt_woe)
#'
#' # performace
#' # ks & roc plot
#' # perf_eva(dt_woe$creditability, dt_pred)
#'
#' # scorecard
#' # Example I # creat a scorecard
#' card = scorecard(bins, m)
#' card2 = scorecard2(bins=bins, dt=germancredit, y='creditability',
#'   x=c("status.of.existing.checking.account", "duration.in.month", "credit.history",
#'    "purpose", "credit.amount", "savings.account.and.bonds",
#'    "present.employment.since", "installment.rate.in.percentage.of.disposable.income",
#'    "personal.status.and.sex", "other.debtors.or.guarantors", "property",
#'    "age.in.years", "other.installment.plans", "housing"))
#'
#' # credit score
#' # Example I # only total score
#' score1 = scorecard_ply(dt, card)
#'
#' # Example II # credit score for both total and each variable
#' score2 = scorecard_ply(dt, card, only_total_score = F)
#' }
#' @import data.table
#' @export
scorecard2 = function(bins, dt, y, x=NULL, points0=600, odds0=1/19, pdo=50, basepoints_eq0=FALSE, positive='bad|1', ...) {
  variable = NULL

  dt = setDT(copy(dt))
  # bins # if (is.list(bins)) rbindlist(bins)
  if (!is.data.table(bins)) {
    if (is.data.frame(bins)) {
      bins = setDT(bins)
    } else {
      bins = rbindlist(bins, fill = TRUE)
    }
  }

  # check x and y
  dt = check_y(dt, y, positive)
  # x
  x_bins = bins[, unique(variable)]
  if (is.null(x)) x = x_bins
  x = x_variable(dt,y,x)

  # dt to woe values
  dt_woe = do.call(woebin_ply, args = c(list(dt=dt, bins=bins, print_info=FALSE), list(...)))

  # model
  model = glm(
    as.formula(paste(y, "~ .")), family = "binomial",
    data = dt_woe[,c(paste0(x,"_woe"),y), with=F])

  return(scorecard(bins = bins, model = model, points0 = points0, odds0 = odds0, pdo = pdo, basepoints_eq0 = basepoints_eq0))
}

#' Score Transformation
#'
#' \code{scorecard_ply} calculates credit score using the results from \code{scorecard}.
#'
#' @param dt A data frame, which is the rriginal dataset for training model.
#' @param card The scorecard generated from the function \code{scorecard}.
#' @param only_total_score  Logical, default is TRUE. If it is TRUE, then the output includes only total credit score; Otherwise, if it is FALSE, the output includes both total and each variable's credit score.
#' @param print_step A non-negative integer. Default is 1. If print_step>0, print variable names by each print_step-th iteration. If print_step=0, no message is print.
#' @param replace_blank_na Logical. Replace blank values with NA. Default is TRUE. This argument should be the same with \code{woebin}'s.
#' @param var_kp Name of force kept variables, such as id column. Default is NULL.
#'
#' @return A data frame in score values
#'
#' @seealso \code{\link{scorecard}} \code{\link{scorecard2}}
#'
#' @examples
#' \dontrun{
#' # load germancredit data
#' data("germancredit")
#'
#' # filter variable via missing rate, iv, identical value rate
#' dt_sel = var_filter(germancredit, "creditability")
#'
#' # woe binning ------
#' bins = woebin(dt_sel, "creditability")
#' dt_woe = woebin_ply(dt_sel, bins)
#'
#' # glm ------
#' m = glm(creditability ~ ., family = binomial(), data = dt_woe)
#' # summary(m)
#'
#' # Select a formula-based model by AIC
#' m_step = step(m, direction="both", trace=FALSE)
#' m = eval(m_step$call)
#' # summary(m)
#'
#' # predicted proability
#' # dt_pred = predict(m, type='response', dt_woe)
#'
#' # performace
#' # ks & roc plot
#' # perf_eva(dt_woe$creditability, dt_pred)
#'
#' # scorecard
#' # Example I # creat a scorecard
#' card = scorecard(bins, m)
#' card2 = scorecard2(bins=bins, dt=germancredit, y='creditability',
#'   x=c("status.of.existing.checking.account", "duration.in.month", "credit.history",
#'    "purpose", "credit.amount", "savings.account.and.bonds",
#'    "present.employment.since", "installment.rate.in.percentage.of.disposable.income",
#'    "personal.status.and.sex", "other.debtors.or.guarantors", "property",
#'    "age.in.years", "other.installment.plans", "housing"))
#'
#' # credit score
#' # Example I # only total score
#' score1 = scorecard_ply(germancredit, card)
#'
#' # Example II # credit score for both total and each variable
#' score2 = scorecard_ply(germancredit, card, only_total_score = F)
#' }
#' @import data.table
#' @export
#'
scorecard_ply = function(dt, card, only_total_score=TRUE, print_step=0L, replace_blank_na=TRUE, var_kp = NULL) {
  # global variables or functions
  variable = bin = points = . = V1 = score = dat_col_placeholder = NULL

  # set dt as data.table
  dt = setDT(copy(dt)) # copy(setDT(dt))
  # # remove date/time col
  # dt = rmcol_datetime_unique1(dt)
  # replace blank values by NA
  if (replace_blank_na) dt = rep_blank_na(dt)
  # print_step
  print_step = check_print_step(print_step)

  # card # if (is.list(card)) rbindlist(card)
  if (inherits(card, 'list') && all(sapply(card, is.data.frame))) {card = rbindlist(card)}
  card = setDT(card)

  # x variables
  xs = card[variable != "basepoints", unique(variable)]
  # length of x variables
  xs_len = length(xs)
  # initial datasets
  n = 0
  while (paste0('dat_col_placeholder',n) %in% xs) n = n+1
  dat = copy(dt)[, (paste0('dat_col_placeholder',n)) := 1][,(xs) := NULL]

  # loop on x variables
  for (i in 1:xs_len) {
    x_i = xs[i]
    # print x
    if (print_step > 0 & i %% print_step == 0) cat(paste0(format(c(i,xs_len)),collapse = "/"), x_i,"\n")

    cardx = card[variable==x_i]
    dtx = dt[, x_i, with=FALSE]

    dat = cbind(dat, woepoints_ply1(dtx, cardx, x_i, woe_points="points"))
  }


  # set basepoints
  card_basepoints = ifelse(
    card[variable == "basepoints", .N] == 1,
    card[variable == "basepoints", points], 0)


  # total score
  dat_score = dat[, paste0(xs, "_points"), with=FALSE]
  dat_score[, score := card_basepoints + rowSums(dat_score)]


  if (only_total_score) dat_score = dat_score[, .(score)]
  if (!is.null(var_kp)) dat_score = cbind(dat[,c(var_kp),with=FALSE], dat_score)
  return(dat_score)
}


