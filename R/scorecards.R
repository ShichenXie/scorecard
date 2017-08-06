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
#' # library(woebin)
#' # # load germancredit data
#' # data("germancredit")
#' # dt <- setnames(
#' #   data.table(germancredit), c(paste0("x",1:20), "y")
#' # )[, y:=ifelse(y=="bad", 1, 0)]
#' # # x y names
#' # y <- "y"
#' #
#' # # iv woe filter ------
#' # # variable filter I
#' # dt_filter <- var_filter(dt, y)
#' #
#' # # woe binning
#' # bins <- woebin(dt_filter, y, stop_limit = 0.1)$bins
#' # dt_woe <- woebin_ply(dt_filter, bins, y)
#' #
#' # # variable filter II
#' # dt_woe_filter <- var_filter(dt_woe, y)
#' #
#' # # # lasso filter ------
#' # # library(h2o)
#' # # # h2o data
#' # # localH2O <- h2o.init()
#' # # dth2o <- as.h2o(dt_woe_filter)
#' # #
#' # # # Breaking Data into Training and Test Sample
#' # # set.seed(345)
#' # # dt.split <- h2o.splitFrame(data=dth2o, ratios=0.6)
#' # # dt.train <- dt.split[[1]]; dt.test <- dt.split[[2]];
#' # #
#' # # # h2o.glm lasso
#' # # fit <- h2o.glm(x=names(dt_woe_filter), y, dt.train, validation_frame=dt.test, family = "binomial", nfolds = 0, alpha = 1, lambda_search = TRUE)
#' # # # summary(fit)
#' # # h2o_var <- data.table(h2o.varimp(fit))[!is.na(coefficients) & coefficients > 0]
#' # # dt_woe_lasso <- dt_woe_filter[, c(h2o_var$names, y), with=FALSE]
#' #
#' # # glm ------
#' # # Breaking Data into Training and Test Sample
#' # set.seed(1255)
#' # dat <- data.table(dt_woe_filter)[sample(nrow(dt_woe_filter))]
#' # set.seed(456)
#' # d <- sample(nrow(dat), nrow(dat)*0.6)
#' # train <- dat[d]; test <- dat[-d];
#' #
#' # # Traditional Credit Scoring Using Logistic Regression ######
#' # # a. model I
#' # # remove variables that coefficients == NEG or Pr_z > 0.1
#' # rm_var_num <- 1
#' # sel_var <- names(train) #names(xy_selected_lasso)
#' # while (rm_var_num > 0) {
#' #   print(rm_var_num)
#' #
#' #   m1 <- glm(
#' #     y ~ ., family = "binomial",
#' #     data = train[, sel_var, with=FALSE]
#' #   )
#' #
#' #   # coefficients
#' #   m1_coef <- data.frame(summary(m1)$coefficients)
#' #   m1_coef$var <- row.names(m1_coef)
#' #   m1_coef <- data.table(m1_coef)[var != "(Intercept)"]
#' #   setnames(m1_coef, c("Estimate", "Std_Error", "z_value", "Pr_z", "var"))
#' #
#' #   # selected variables
#' #   sel_var <- c(m1_coef[Estimate > 0 & Pr_z < 0.1, var], y)
#' #
#' #   # number of variables that coefficients == NEG or Pr_z > 0.1
#' #   rm_var_num <- m1_coef[Estimate <= 0 | Pr_z > 0.1][, .N]
#' # }
#' # # summary(m1)
#' #
#' # # b. model II
#' # # Select a formula-based model by AIC
#' # m_step <- step(m1, direction="both")
#' # m2 <- eval(m_step$call)
#' # # summary(m2)
#' #
#' # # score & performance ------
#' # # predicted proability
#' # train$pred <- predict(m2, type='response', train)
#' # test$pred <- predict(m2, type='response', test)
#' #
#' # # score
#' # train_score <- scorecards(train, y, bins, m2)$score
#' # test_score <- scorecards(test, y, bins, m2)$score
#' #
#' # # performace plot
#' # perf_plot(train[,.(y,pred)], y, title="train")
#' # perf_plot(test[,.(y,pred)], y, title="test")
#' #
#' # perf_psi(train_score[,.(y, score)], test_score[,.(y, score)], y)
#' #
#' # # scorecards
#' # cards <- scorecards(train, y, bins, m2)$scorecards
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
      variable, bin, woe, points = round(-b*coef[var==i, Estimate]*woe)
    ) ]

    dt_score[,(paste0(i,"_points")) := round(-b*coef[var==i, Estimate]*dt_woe[[paste0(i, "_woe")]])]
  }

  # total score
  dt_score[["score"]] <- scorecards[["basepoints"]][,points] + rowSums(dt_score[, paste0(coef[-1,var], "_points"), with=FALSE])
  # dt_score <- dt_woe[,c(paste0(coef[-1,var], "_points"), y, "score"), with=FALSE]

  return(list(scorecards=scorecards, score = dt_score))

}


# reference
# https://cn.mathworks.com/help/finance/case-study-for-a-credit-scorecard-analysis.html?requestedDomain=www.mathworks.com#zmw57dd0e33220
