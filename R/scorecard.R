
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
      # odds = p/(1-p) # positive/negative <==>
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

# p to score
p2score = function(p, points0=600, odds0=1/19, pdo=50) {
  aabb = ab(points0, odds0, pdo)

  s = aabb$a - aabb$b * log(p/(1-p))
  return(s)
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
#' @param basepoints_eq0 Logical, Defaults to FALSE. If it is TRUE, the basepoints will equally distribute to each variable.
#' @param digits The number of digits after the decimal point for points calculation. Default 0.
#' @return A list of scorecard data frames
#'
#' @seealso \code{\link{scorecard2}} \code{\link{scorecard_ply}}
#'
#' @examples
#' \donttest{
#' # load germancredit data
#' data("germancredit")
#' # filter variable via missing rate, iv, identical value rate
#' dtvf = var_filter(germancredit, "creditability")
#' # split into train and test
#' dtlst = split_df(dtvf, y = 'creditability')
#' # binning
#' bins = woebin(dtlst$train, "creditability")
#'
#' # to woe
#' dtlst_woe = lapply(dtlst, function(d) woebin_ply(d, bins))
#'# lr
#' m = glm(creditability ~ ., family = binomial(), data = dtlst_woe$train)
#' # scorecard
#' card = scorecard(bins, m)
#' prob = predict(m, dtlst_woe$train, type='response')
#' # problst = lapply(dtlst_woe, function(x) predict(m, x, type='response'))
#'
#' }
#' @import data.table
#' @export
scorecard = function(bins, model, points0=600, odds0=1/19, pdo=50, basepoints_eq0=FALSE, digits=0) {
  # global variables or functions
  variable = var_woe = Estimate = points = woe = NULL

  # coefficients
  aabb = ab(points0, odds0, pdo)
  a = aabb$a
  b = aabb$b
  # odds = pred/(1-pred); score = a - b*log(odds)

  # bins # if (is.list(bins)) rbindlist(bins)
  if (inherits(bins, 'list') && all(sapply(bins, is.data.frame))) bins = rbindlist(bins)
  bins = setDT(bins)

  # coefficients
  coef_dt = data.table(var_woe = names(coef(model)), Estimate = coef(model))[, variable := sub("_woe$", "", var_woe) ][]

  # scorecard
  basepoints = a - b*coef_dt[1,Estimate]

  card = list()
  if (basepoints_eq0) {
    card[["basepoints"]] = data.table( variable = "basepoints", bin = NA, woe = NA, points = 0 )

    for (i in coef_dt[-1,variable]) {
      card[[i]] = bins[variable==i][, points := round(-b*coef_dt[variable==i, Estimate]*woe + basepoints/coef_dt[,.N-1], digits)]
    }
  } else {
    card[["basepoints"]] = data.table( variable = "basepoints", bin = NA, woe = NA, points = round(basepoints, digits) )

    for (i in coef_dt[-1,variable]) {
      card[[i]] = bins[variable==i][, points := round(-b*coef_dt[variable==i, Estimate]*woe, digits)]
    }
  }

  return(card)
}

#' Creating a Scorecard
#'
#' \code{scorecard2} creates a scorecard based on the results from \code{woebin}. It has the same function of \code{scorecard}, but without model object input and provided adjustment for oversampling.
#'
#' @param bins Binning information generated from \code{woebin} function.
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. If it is NULL, then all variables in bins are used. Defaults to NULL.
#' @param points0 Target points, default 600.
#' @param odds0 Target odds, default 1/19. Odds = p/(1-p).
#' @param pdo Points to Double the Odds, default 50.
#' @param basepoints_eq0 Logical, defaults to FALSE. If it is TRUE, the basepoints will equally distribute to each variable.
#' @param digits The number of digits after the decimal point for points calculation. Default 0.
#' @param return_prob Logical, defaults to FALSE. If it is TRUE, the predict probability will also return.
#' @param posprob_pop Positive probability of population. Accepted range: 0-1,  default to NULL. If it is not NULL, the model will adjust for oversampling.
#' @param posprob_sample Positive probability of sample. Accepted range: 0-1,  default to the positive probability of the input dt.
#' @param positive Value of positive class, default "bad|1".
#' @param ... Additional parameters.
#' @return A list of scorecard data frames
#'
#' @seealso \code{\link{scorecard}} \code{\link{scorecard_ply}}
#'
#' @examples
#' \donttest{
#' # load germancredit data
#' data("germancredit")
#' # filter variable via missing rate, iv, identical value rate
#' dtvf = var_filter(germancredit, "creditability")
#' # split into train and test
#' dtlst = split_df(dtvf, y = 'creditability')
#' # binning
#' bins = woebin(dtlst$train, "creditability")
#'
#' # train only
#' ## create scorecard
#' card1 = scorecard2(bins=bins, dt=dtlst$train, y='creditability')
#' ## scorecard and predicted probability
#' cardprob1 = scorecard2(bins=bins, dt=dtlst$train, y='creditability', return_prob = TRUE)
#'
#' # both train and test
#' ## create scorecard
#' card2 = scorecard2(bins=bins, dt=dtlst, y='creditability')
#' ## scorecard and predicted probability
#' cardprob2 = scorecard2(bins=bins, dt=dtlst, y='creditability', return_prob = TRUE)
#'
#' }
#' @import data.table
#' @importFrom stats predict
#' @export
scorecard2 = function(bins, dt, y, x=NULL, points0=600, odds0=1/19, pdo=50, basepoints_eq0=FALSE, digits=0, return_prob = FALSE, posprob_pop = NULL, posprob_sample = NULL, positive='bad|1', ...) {
  variable = wgts = NULL

  # data frame to list
  if (inherits(dt, 'data.frame')) dt = list(dat=dt)
  # check y column
  dt = lapply(dt, function(d) check_y(d, y, positive))
  dt0 = dt[[1]]

  # param
  kwargs = list(...)
  badprob_pop = kwargs[['kwargs']]
  if (!is.null(badprob_pop)) posprob_pop = badprob_pop

  # bind bins
  if (inherits(bins, 'list') && all(sapply(bins, is.data.frame))) bins = rbindlist(bins)
  bins = setDT(bins)

  # check xs
  x_bins = bins[, unique(variable)]
  if (is.null(x)) x = x_bins
  x = x_variable(dt0,y,x)

  # dt to woe
  dt_woe = lapply(dt, function(d) do.call(woebin_ply, args = c(list(dt=d, bins=bins, print_info=FALSE), list(...))))
  dt0_woe = dt_woe[[1]]

  # model
  if (is.numeric(posprob_pop) && posprob_pop > 0 && posprob_pop < 1) {
    # positive probability in population
    p1 = posprob_pop
    # positive probability in sample dataset
    if (is.numeric(posprob_sample) && posprob_sample > 0 && posprob_sample < 1) {
      r1 = posprob_sample
    } else {
      r1 = dt0[, table(get(y))/.N][['1']]
    }

    dt0_woe = dt0_woe[grepl(positive, get(y)), wgts := p1/r1
                  ][is.na(wgts), wgts := (1-p1)/(1-r1)
                  ][,c(paste0(x,"_woe"), "wgts", y), with=F]

    fmla = as.formula(sprintf('%s ~ %s', y, paste(paste0(x,"_woe"), collapse=" + ")))
    model = glm(fmla, family = 'quasibinomial', weights = wgts, data = dt0_woe)
  } else {

    model = glm(
      as.formula(paste(y, "~ .")), family = 'binomial',
      data = dt0_woe[,c(paste0(x,"_woe"),y), with=F])
  }

  na_coef = coef(model)[is.na(coef(model))]
  if (length(na_coef) > 0) warning(sprintf('The model coefficients for the following %s variables are NA, please remove these variables:\n%s', length(na_coef), paste(sub('_woe', '', names(na_coef)), collapse = ',')))

  # scorecard
  card = scorecard(bins = bins, model = model, points0 = points0, odds0 = odds0, pdo = pdo, basepoints_eq0 = basepoints_eq0, digits = digits)

  # returns
  if (return_prob) {
    rt = list(
      card = card,
      prob = lapply(dt_woe, function(d) predict(model, d, type='response'))
    )
  } else {
    rt = card
  }
  return(rt)
}

#' Score Transformation
#'
#' \code{scorecard_ply} calculates credit score using the results from \code{scorecard}.
#'
#' @param dt A data frame, which is the original dataset for training model.
#' @param card The scorecard generated from the function \code{scorecard}.
#' @param only_total_score  Logical, Defaults to TRUE. If it is TRUE, then the output includes only total credit score; Otherwise, if it is FALSE, the output includes both total and each variable's credit score.
#' @param print_step A non-negative integer. Defaults to 1. If print_step>0, print variable names by each print_step-th iteration. If print_step=0, no message is print.
#' @param replace_blank_na Logical. Replace blank values with NA. Defaults to TRUE. This argument should be the same with \code{woebin}'s.
#' @param var_kp Name of force kept variables, such as id column. Defaults to NULL.
#'
#' @return A data frame in score values
#'
#' @seealso \code{\link{scorecard}} \code{\link{scorecard2}}
#'
#' @examples
#' \donttest{
#' # load germancredit data
#' data("germancredit")
#' # filter variable via missing rate, iv, identical value rate
#' dtvf = var_filter(germancredit, "creditability")
#' # split into train and test
#' dtlst = split_df(dtvf, y = 'creditability')
#' # binning
#' bins = woebin(dtlst$train, "creditability")
#' # scorecard
#' card = scorecard2(bins=bins, dt=dtlst$train, y='creditability')
#'
#' # credit score
#' # Example I # only total score
#' score1 = scorecard_ply(germancredit, card)
#'
#' # Example II # credit score for both total and each variable
#' score2 = scorecard_ply(germancredit, card, only_total_score = FALSE)
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
  # bin_close_right
  bin_close_right = check_bcr(card)


  # card # if (is.list(card)) rbindlist(card)
  if (inherits(card, 'list') && all(sapply(card, is.data.frame))) {card = rbindlist(card, fill = TRUE)}
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

    dat = cbind(dat, woepoints_ply1(dtx, cardx, x_i, woe_points="points", bin_close_right=bin_close_right))
  }


  # set basepoints
  card_basepoints = ifelse(
    card[variable == "basepoints", .N] == 1,
    card[variable == "basepoints", points], 0)


  # total score
  dat_score = dat[, paste0(xs, "_points"), with=FALSE]
  dat_score[, score := card_basepoints + rowSums(dat_score)]


  if (only_total_score) dat_score = dat_score[, .(score)]

  # check force kept variables
  if (!is.null(var_kp)) {
    var_kp2 = intersect(var_kp, names(dt))
    len_diff = length(var_kp) - length(var_kp2)
    if (len_diff > 0) {
      warning("Incorrect inputs; there are ", len_diff, " var_kp variables are not exist in input data, which are removed from var_kp. \n", setdiff(var_kp, var_kp2))
    }
    var_kp = var_kp2
  }
  if (!is.null(var_kp)) dat_score = cbind(dat[,c(var_kp),with=FALSE], dat_score)
  return(dat_score)
}


