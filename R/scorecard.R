# scorecard
ab <- function(p0=600, odds0=1/60, pdo=50) {
  # ab(600, 1/30, 60)

  # sigmoid function
  # library(ggplot2)
  # ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = function(x) 1/(1+exp(-x)))

  # p(y=1) <- 1/(1+exp(-z)), # z = beta0+beta1*x1+...+betar*xr = beta*x
  #==># z <- log(p/(1-p)), # odds = p/(1-p) # bad/good #==># p= odds/1+odds
  #==># z <- log(odds)
  #==># score = a - b*log(odds)

  b <- pdo/log(2)
  a <- p0 + b*log(odds0/(1+odds0))

  return(list(a=a, b=b))
}
#' create scorecard
#'
#' \code{scorecard} creates scorecard based on the results from \code{woebin} and \code{glm}.
#'
#' @param bins Binning information generated from \code{woebin} function.
#' @param model A glm model object.
#' @param p0 Target points, default 600.
#' @param odds0 Target odds, default 1/60.
#' @param pdo Points to Double the Odds, default 50.
#' @return scorecard
#' @seealso \code{\link{scorecard_ply}}
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(scorecard)
#'
#' # Traditional Credit Scoring Using Logistic Regression
#' # load germancredit data
#' data("germancredit")
#'
#' # rename creditability as y
#' dt <- setDT(germancredit)[, `:=`(
#'   y = ifelse(creditability == "bad", 1, 0),
#'   creditability = NULL
#' )]
#'
#' # woe binning ------
#' bins <- woebin(dt, "y")
#' dt_woe <- woebin_ply(dt, bins)
#'
#' # glm ------
#' m1 <- glm( y ~ ., family = "binomial", data = dt_woe)
#' # summary(m1)
#'
#' # Select a formula-based model by AIC
#' m_step <- step(m1, direction="both")
#' m2 <- eval(m_step$call)
#' # summary(m2)
#'
#' # performance ------
#' # predicted proability
#' dt_woe$pred <- predict(m2, type='response', dt_woe)
#'
#' # performace
#' # ks & roc plot
#' perf_plot(dt_woe$y, dt_woe$pred)
#'
#' # card
#' card <- scorecard(bins, m2)
#'
#' # score
#' dt_woe$score <- scorecard_ply(dt, card)
#' }
#' @import data.table
#' @export
#'
scorecard <- function(bins, model, p0=600, odds0=1/60, pdo=50) {
  variable = var_woe = Estimate = points = woe = NULL # no visible binding for global variable

  aabb <- ab(p0, odds0, pdo)
  a <- aabb$a; b <- aabb$b;
  # odds <- pred/(1-pred); score <- a - b*log(odds)

  # bins # if (is.list(bins)) rbindlist(bins)
  if (!is.data.table(bins)) {
    if (is.data.frame(bins)) {
      bins <- setDT(bins)
    } else {
      bins <- rbindlist(bins, fill = TRUE)
    }
  }

  # coefficients
  coef <- data.frame(summary(model)$coefficients)
  coef$variable <- row.names(coef)
  coef <- setnames(setDT(coef)[,c(1,5),with=FALSE], c("Estimate", "var_woe"))[, variable := gsub("_woe$", "", var_woe) ][]


  # scorecard
  scorecard <- list()
  scorecard[["basepoints"]] <- data.table( variable = "basepoints", bin = NA, woe = NA, points = round(a - b*coef[1,Estimate]) )

  for (i in coef[-1,variable]) {
    scorecard[[i]] <- bins[variable==i][, points := round(-b*coef[variable==i, Estimate]*woe)] # [ ,.( variable, bin, numdistr, woe, points = round(-b*coef[variable==i, Estimate]*woe) )]
  }

  return(scorecard)
}

#' calculates credit score
#'
#' \code{scorecard_ply} calculates credit score using the results of \code{scorecard}.
#' @param dt Original data
#' @param card Scorecard generated from \code{scorecard}.
#' @param only_total_score Logical, default TRUE. If it is TRUE, return total credit score only; if FALSE, return both total credit score and score points of each variables.
#' @return credit score points
#' @seealso \code{\link{scorecard}}
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(scorecard)
#'
#' # Traditional Credit Scoring Using Logistic Regression
#' # load germancredit data
#' data("germancredit")
#'
#' # rename creditability as y
#' dt <- setDT(germancredit)[, `:=`(
#'   y = ifelse(creditability == "bad", 1, 0),
#'   creditability = NULL
#' )]
#'
#' # woe binning ------
#' bins <- woebin(dt, "y")
#' dt_woe <- woebin_ply(dt, bins)
#'
#' # glm ------
#' m1 <- glm( y ~ ., family = "binomial", data = dt_woe)
#' # summary(m1)
#'
#' # Select a formula-based model by AIC
#' m_step <- step(m1, direction="both")
#' m2 <- eval(m_step$call)
#' # summary(m2)
#'
#' # performance ------
#' # predicted proability
#' dt_woe$pred <- predict(m2, type='response', dt_woe)
#'
#' # performace
#' # ks & roc plot
#' perf_plot(dt_woe$y, dt_woe$pred)
#'
#' # card
#' card <- scorecard(bins, m2)
#'
#' # score
#' dt_woe$score <- scorecard_ply(dt, card)
#' }
#' @import data.table
#' @export
#'
scorecard_ply <- function(dt, card, only_total_score = TRUE) {
  variable = bin = points = . = V1 = NULL # no visible binding for global variable

  kdt <- copy(setDT(dt))

  # card # if (is.list(card)) rbindlist(card)
  if (!is.data.table(card)) {
    if (is.data.frame(card)) {
      card <- setDT(card)
    } else {
      card <- rbindlist(card, fill = TRUE)
    }
  }

  # x variables
  x <- card[variable != "basepoints", unique(variable)]
  # if (anyNA(x)) {
  #   if (length(setdiff(names(kdt), y)) >= length(card[,unique(variable)])) {
  #     x <- card[,unique(variable)]
  #   } else {
  #     x <- setdiff(names(kdt), y)
  #   }
  # }

  # loop on x variables
  for (a in x) {
    print(a)
    cardx <- card[variable==a] #card[[a]]
    na_points <- cardx[bin == "missing", points]


    if (is.factor(kdt[[a]]) | is.character(kdt[[a]])) {
      # # separate_rows
      # # https://stackoverflow.com/questions/13773770/split-comma-separated-column-into-separate-rows
      # binsx[, lapply(.SD, function(x) unlist(tstrsplit(x, "%,%", fixed=TRUE))), by = bstbin, .SDcols = "bin" ][copy(binsx)[,bin:=NULL], on="bstbin"]#[!is.na(bin)]

      # return
      kdt <- setnames(
        cardx[, strsplit(as.character(bin), "%,%", fixed=TRUE), by = .(bin) ][cardx[, .(bin, points)], on="bin"][,.(V1, points)],
        c(a, paste0(a, "_points"))
      )[kdt, on=a
        ][, (a) := NULL] #[!is.na(bin)]

    } else if (is.logical(kdt[[a]]) | is.numeric(kdt[[a]])) {
      if (is.logical(kdt[[a]])) kdt[[a]] <- as.numeric(kdt[[a]]) # convert logical variable to numeric

      kdt[[a]] = cut(kdt[[a]], unique(c(-Inf, cardx[, as.numeric(sub("^\\[(.*),.+", "\\1", bin))], Inf)), right = FALSE, dig.lab = 10, ordered_result = FALSE)

      # return
      kdt <- setnames(
        cardx[,.(bin, points)], c(a, paste0(a, "_points"))
      )[kdt, on = a
      ][, (a) := NULL]

    }

    # if is.na(kdt) == missing_points
    kdt[[paste0(a, "_points")]] <- ifelse(is.na(kdt[[paste0(a, "_points")]]), na_points,  kdt[[paste0(a, "_points")]])

  }

  # dt_score[,(paste0(i,"_points")) := round(-b*coef[variable==i, Estimate]*dt_woe[[paste0(i, "_woe")]])]



  # total score
  # dt_score[["score"]]
  total_score <- card[variable == "basepoints", points] + rowSums(kdt[, paste0(x, "_points"), with=FALSE], na.rm = TRUE)
  # dt_score <- dt_woe[,c(paste0(coef[-1,variable], "_points"), y, "score"), with=FALSE]

  if (only_total_score) {
    return(total_score)
  } else {
    kdt[["score"]] <- total_score
    return(kdt)
  }

}


# reference
# Population Stability Index (PSI) – Banking Case (Part 6)#: http://ucanalytics.com/blogs/population-stability-index-psi-banking-case-study/
# Weight of Evidence (WoE) Introductory Overview #: http://documentation.statsoft.com/StatisticaHelp.aspx?path=WeightofEvidence/WeightofEvidenceWoEIntroductoryOverview
# Case Study for a Credit Scorecard Analysis #: https://cn.mathworks.com/help/finance/case-study-for-a-credit-scorecard-analysis.html?requestedDomain=www.mathworks.com#zmw57dd0e33220
