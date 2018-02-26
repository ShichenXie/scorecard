# Adjusting the Intercept
# support.sas.com/kb/22/601.html


# coefficients in scorecard
ab = function(points0=600, odds0=1/60, pdo=50) {
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
#' @param basepoints_eq0 Logical, default FALSE. If it is TRUE, the basepoints equals 0 and will equally add to variables' points.
#' @return scorecard
#'
#' @seealso \code{\link{scorecard_ply}}
#'
#' @examples
#' library(data.table)
#' library(scorecard)
#'
#' # load germancredit data
#' data("germancredit")
#'
#' # select only 5 x variables and rename creditability as y
#' dt = setDT(germancredit)[, c(1:5, 21)][, `:=`(
#'   y = ifelse(creditability == "bad", 1, 0),
#'   creditability = NULL
#' )]
#'
#' # woe binning ------
#' bins = woebin(dt, "y")
#' dt_woe = woebin_ply(dt, bins)
#'
#' # glm ------
#' m = glm( y ~ ., family = "binomial", data = dt_woe)
#' # summary(m)
#'
#' \dontrun{
#' # Select a formula-based model by AIC
#' m_step = step(m, direction="both", trace=FALSE)
#' m = eval(m_step$call)
#' # summary(m)
#'
#' # predicted proability
#' # dt_woe$pred = predict(m, type='response', dt_woe)
#'
#' # performace
#' # ks & roc plot
#' # perf_eva(dt_woe$y, dt_woe$pred)
#' }
#'
#' # scorecard
#' # Example I # creat a scorecard
#' card = scorecard(bins, m)
#'
#' \dontrun{
#' # credit score
#' # Example I # only total score
#' score1 = scorecard_ply(dt, card)
#'
#' # Example II # credit score for both total and each variable
#' score2 = scorecard_ply(dt, card, only_total_score = F)
#' }
#' @import data.table
#' @export
#'
scorecard = function(bins, model, points0=600, odds0=1/19, pdo=50, basepoints_eq0=FALSE) {
  # global variables or functions
  variable = var_woe = Estimate = points = woe = NULL

  # coefficients
  aabb = ab(points0, odds0, pdo)
  a = aabb$a; b = aabb$b;
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
  coef = data.frame(summary(model)$coefficients)
  coef$variable = row.names(coef)
  coef = setnames(setDT(coef)[,c(1,5),with=FALSE], c("Estimate", "var_woe"))[, variable := gsub("_woe$", "", var_woe) ][]


  # scorecard
  len_x = coef[-1,.N]
  basepoints = a - b*coef[1,Estimate]
  scorecard = list()

  if (basepoints_eq0) {
    scorecard[["basepoints"]] = data.table( variable = "basepoints", bin = NA, woe = NA, points = 0 )

    for (i in coef[-1,variable]) {
      scorecard[[i]] = bins[variable==i][, points := round(-b*coef[variable==i, Estimate]*woe + basepoints/len_x)]
    }
  } else {
    scorecard[["basepoints"]] = data.table( variable = "basepoints", bin = NA, woe = NA, points = round(basepoints) )

    for (i in coef[-1,variable]) {
      scorecard[[i]] = bins[variable==i][, points := round(-b*coef[variable==i, Estimate]*woe)]
    }
  }

  return(scorecard)
}


#' Application of Scorecard
#'
#' \code{scorecard_ply} calculates credit score using the results of \code{scorecard}.
#'
#' @param dt Original data
#' @param card Scorecard generated from \code{scorecard}.
#' @param only_total_score  A logical value. Default is FALSE. If it is TRUE, which means only total credit score is return; Otherwise, if it is FALSE, which means both total credit score and score points of each variables are return.
#' @param print_step A non-negative integer. Default is 1. Print variable names by print_step when print_step>0. If print_step=0, no message is printed.
#' @return Credit score
#'
#' @seealso \code{\link{scorecard}}
#'
#' @examples
#' library(data.table)
#' library(scorecard)
#'
#' # load germancredit data
#' data("germancredit")
#'
#' # select only 5 x variables and rename creditability as y
#' dt = setDT(germancredit)[, c(1:5, 21)][, `:=`(
#'   y = ifelse(creditability == "bad", 1, 0),
#'   creditability = NULL
#' )]
#'
#' # woe binning ------
#' bins = woebin(dt, "y")
#' dt_woe = woebin_ply(dt, bins)
#'
#' # glm ------
#' m = glm( y ~ ., family = "binomial", data = dt_woe)
#' # summary(m)
#'
#' \dontrun{
#' # Select a formula-based model by AIC
#' m_step = step(m, direction="both", trace=FALSE)
#' m = eval(m_step$call)
#' # summary(m)
#'
#' # predicted proability
#' # dt_woe$pred = predict(m, type='response', dt_woe)
#'
#' # performace
#' # ks & roc plot
#' # perf_eva(dt_woe$y, dt_woe$pred)
#' }
#'
#' # scorecard
#' # Example I # creat a scorecard
#' card = scorecard(bins, m)
#'
#' # credit score
#' # Example I # only total score
#' score1 = scorecard_ply(dt, card)
#'
#' \dontrun{
#' # Example II # credit score for both total and each variable
#' score2 = scorecard_ply(dt, card, only_total_score = F)
#' }
#' @import data.table
#' @export
#'
scorecard_ply = function(dt, card, only_total_score=FALSE, print_step=1L) {
  # global variables or functions
  variable = bin = points = . = V1 = score = NULL

  # set dt as data.table
  dt = copy(setDT(dt))
  # remove date/time col
  dt = rm_datetime_col(dt)
  # replace "" by NA
  dt = rep_blank_na(dt)
  # print_step
  print_step = check_print_step(print_step)

  # card # if (is.list(card)) rbindlist(card)
  if (!is.data.table(card)) {
    if (is.data.frame(card)) {
      card = setDT(card)
    } else {
      card = rbindlist(card, fill=TRUE)
    }
  }

  # x variables
  xs = card[variable != "basepoints", unique(variable)]
  # length of x variables
  xs_len = length(xs)
  # initial datasets
  dat = copy(dt)[,(xs):=NULL]

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
  return(dat_score)
}


# reference
# Population Stability Index (PSI) – Banking Case (Part 6)#: http://ucanalytics.com/blogs/population-stability-index-psi-banking-case-study/
# Weight of Evidence (WoE) Introductory Overview #: http://documentation.statsoft.com/StatisticaHelp.aspx?path=WeightofEvidence/WeightofEvidenceWoEIntroductoryOverview
# Case Study for a Credit Scorecard Analysis #: https://cn.mathworks.com/help/finance/case-study-for-a-credit-scorecard-analysis.html?requestedDomain=www.mathworks.com#zmw57dd0e33220
