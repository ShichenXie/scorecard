# scorecards ------
ab <- function(p0=600, odds0=1/60, pdo=50) {
  # sigmoid function
  # library(ggplot2)
  # ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = function(x) 1/(1+exp(-x)))

  # p(y=1) <- 1/(1+exp(-z)), # z = beta0+beta1*x1+...+betar*xr = beta*x
  #==># z <- log(p/(1-p)), # odds = p/(1-p) # bad/good
  #==># z <- log(odds)
  #==># score = a - b*log(odds)

  b <- pdo/log(2)
  a <- p0 + b*log(odds0/(1+odds0))

  return(list(a=a, b=b))
}
#' credit score & scorecards
#'
#' This function calculates credit score based on predited probability.
#' @name scorecards
#' @param dt_woe input data that converted into woe
#' @param y name of y variable.
#' @param bins Binning information generated from woebin function
#' @param model fitted glm model
#' @param p0 target points, default 600
#' @param odds0 target odds, default 1/60
#' @param pdo Points to Double the Odds, default 50
#' @return score and scorecards
#' @export
#' @examples
#' @examples
#' # # Traditional Credit Scoring Using Logistic Regression
#' # library(woebin)
#' # # load germancredit data
#' # data("germancredit")
#' #
#' # # rename creditability as y
#' # dt <- data.table(germancredit)[, `:=`(
#' #   y = ifelse(creditability == "bad", 1, 0),
#' #   creditability = NULL
#' # )]
#' #
#' # # woe binning ------
#' # bins <- woebin(dt, "y")$bins
#' # dt_woe <- woebin_ply(dt, "y", bins)
#' #
#' # # glm ------
#' # m1 <- glm( y ~ ., family = "binomial", data = dt_woe)
#' # # summary(m1)
#' #
#' # # Select a formula-based model by AIC
#' # m_step <- step(m1, direction="both")
#' # m2 <- eval(m_step$call)
#' # # summary(m2)
#' #
#' # # performance ------
#' # # predicted proability
#' # dt_woe$pred <- predict(m2, type='response', dt_woe)
#' #
#' # # performace
#' # # ks & roc plot
#' # perf_plot(dt_woe$y, dt_woe$pred)
#' #
#' # # score
#' # dt_woe$score <- scorecards(dt_woe, "y", bins, m2)$score
#' #
#' # # cards
#' # cards <- scorecards(dt_woe, "y", bins, m2)$cards
#'
scorecards <- function(dt_woe, y, bins, model, p0=600, odds0=1/60, pdo=50) {
  aabb <- ab(p0, odds0, pdo)
  a <- aabb$a; b <- aabb$b;
  # odds <- pred/(1-pred); score <- a - b*log(odds)

  # coefficients
  coef <- data.frame(summary(model)$coefficients)
  coef$var <- row.names(coef)
  coef <- setnames(data.table(coef)[,c(1,5),with=FALSE], c("Estimate", "var_woe"))[, var := gsub("_woe$", "", var_woe) ][]


  # scorecards & score
  scorecards <- list()
  dt_score <- data.table(y=dt_woe[[y]])
  scorecards[["basepoints"]] <- data.table( variable = "basepoints", bin = NA, woe = NA, points = round(a - b*coef[1,Estimate]) )

  for (i in coef[-1,var]) {
    scorecards[[i]] <- bins[[i]][ ,.(
      variable, bin, numdistr, woe, points = round(-b*coef[var==i, Estimate]*woe)
    ) ]

    dt_score[,(paste0(i,"_points")) := round(-b*coef[var==i, Estimate]*dt_woe[[paste0(i, "_woe")]])]
  }

  # total score
  # dt_score[["score"]]
  score <- scorecards[["basepoints"]][,points] + rowSums(dt_score[, paste0(coef[-1,var], "_points"), with=FALSE])
  # dt_score <- dt_woe[,c(paste0(coef[-1,var], "_points"), y, "score"), with=FALSE]

  return(list(cards = scorecards, score = score))

}


# reference
# Population Stability Index (PSI) – Banking Case (Part 6)#: http://ucanalytics.com/blogs/population-stability-index-psi-banking-case-study/
# Weight of Evidence (WoE) Introductory Overview #: http://documentation.statsoft.com/StatisticaHelp.aspx?path=WeightofEvidence/WeightofEvidenceWoEIntroductoryOverview
# Case Study for a Credit Scorecard Analysis #: https://cn.mathworks.com/help/finance/case-study-for-a-credit-scorecard-analysis.html?requestedDomain=www.mathworks.com#zmw57dd0e33220
