# coefficients in scorecard
ab <- function(points0=600, odds0=1/60, pdo=50) {
  # ab(600, 1/30, 60)


  # library(ggplot2)
  # sigmoid function
  # ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = function(x) 1/(1+exp(-x)))

  # log_odds function
  # ggplot(data.frame(x = c(0, 1)), aes(x)) + stat_function(fun = function(x) log(x/(1-x)))


  # logistic function
  # p(y=1) <- 1/(1+exp(-z)),
      # z = beta0+beta1*x1+...+betar*xr = beta*x
  ##==> z <- log(p/(1-p)),
      # odds = p/(1-p) # bad/good <==>
      # p = odds/1+odds
  ##==> z <- log(odds)
  ##==> score = a - b*log(odds)

  # two hypothesis
  # points0 = a - b*log(odds0)
  # points0 - PDO = a - b*log(2*odds0)

  b <- pdo/log(2)
  a <- points0 + b*log(odds0/(1+odds0))

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
#' dt <- setDT(germancredit)[, c(1:5, 21)][, `:=`(
#'   y = ifelse(creditability == "bad", 1, 0),
#'   creditability = NULL
#' )]
#'
#' # woe binning ------
#' bins <- woebin(dt, "y")
#' dt_woe <- woebin_ply(dt, bins)
#'
#' # glm ------
#' m <- glm( y ~ ., family = "binomial", data = dt_woe)
#' # summary(m)
#'
#' \dontrun{
#' # Select a formula-based model by AIC
#' m_step <- step(m, direction="both")
#' m <- eval(m_step$call)
#' # summary(m)
#'
#' # predicted proability
#' # dt_woe$pred <- predict(m, type='response', dt_woe)
#'
#' # performace
#' # ks & roc plot
#' # perf_plot(dt_woe$y, dt_woe$pred)
#' }
#'
#' # scorecard
#' # Example I # creat a scorecard
#' card <- scorecard(bins, m)
#'
#' \dontrun{
#' # credit score
#' # Example I # only total score
#' score1 <- scorecard_ply(dt, card)
#'
#' # Example II # credit score for both total and each variable
#' score2 <- scorecard_ply(dt, card, only_total_score = F)
#' }
#' @import data.table
#' @export
#'
scorecard <- function(bins, model, points0=600, odds0=1/19, pdo=50) {
  variable = var_woe = Estimate = points = woe = NULL # no visible binding for global variable

  aabb <- ab(points0, odds0, pdo)
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
  scorecard[["basepoints"]] <- setDF(
    data.table( variable = "basepoints", bin = NA, woe = NA, points = round(a - b*coef[1,Estimate]) )
  )


  for (i in coef[-1,variable]) {
    scorecard[[i]] <- setDF(
      bins[variable==i][, points := round(-b*coef[variable==i, Estimate]*woe)]
    )
    # [ ,.( variable, bin, numdistr, woe, points = round(-b*coef[variable==i, Estimate]*woe) )]
  }

  return(scorecard)
}

#' Application of Scorecard
#'
#' \code{scorecard_ply} calculates credit score using the results of \code{scorecard}.
#'
#' @param dt Original data
#' @param card Scorecard generated from \code{scorecard}.
#' @param only_total_score Logical, default TRUE. If it is TRUE, return total credit score only; if FALSE, return both total credit score and score points of each variables.
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
#' dt <- setDT(germancredit)[, c(1:5, 21)][, `:=`(
#'   y = ifelse(creditability == "bad", 1, 0),
#'   creditability = NULL
#' )]
#'
#' # woe binning ------
#' bins <- woebin(dt, "y")
#' dt_woe <- woebin_ply(dt, bins)
#'
#' # glm ------
#' m <- glm( y ~ ., family = "binomial", data = dt_woe)
#' # summary(m)
#'
#' \dontrun{
#' # Select a formula-based model by AIC
#' m_step <- step(m, direction="both")
#' m <- eval(m_step$call)
#' # summary(m)
#'
#' # predicted proability
#' # dt_woe$pred <- predict(m, type='response', dt_woe)
#'
#' # performace
#' # ks & roc plot
#' # perf_plot(dt_woe$y, dt_woe$pred)
#' }
#'
#' # scorecard
#' # Example I # creat a scorecard
#' card <- scorecard(bins, m)
#'
#' # credit score
#' # Example I # only total score
#' score1 <- scorecard_ply(dt, card)
#'
#' \dontrun{
#' # Example II # credit score for both total and each variable
#' score2 <- scorecard_ply(dt, card, only_total_score = F)
#' }
#' @import data.table
#' @export
#'
scorecard_ply <- function(dt, card, only_total_score = TRUE) {
  variable = bin = points = . = V1 = score = NULL # no visible binding for global variable

  # conditions # https://adv-r.hadley.nz/debugging
  if (!is.data.frame(dt)) stop("Incorrect inputs; dt should be a dataframe.")

  # set dt as data.table
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
  dt_score <- kdt[, paste0(x, "_points"), with=FALSE]
  dt_score[, score := card[variable == "basepoints", points] + rowSums(kdt[, paste0(x, "_points"), with=FALSE], na.rm = TRUE)]

  # total_score <- card[variable == "basepoints", points] + rowSums(kdt[, paste0(x, "_points"), with=FALSE], na.rm = TRUE)
  # dt_score <- dt_woe[,c(paste0(coef[-1,variable], "_points"), y, "score"), with=FALSE]

  if (only_total_score) {
    return( dt_score[, .(score)] )
  } else {
    return( dt_score )
  }

}


# reference
# Population Stability Index (PSI) – Banking Case (Part 6)#: http://ucanalytics.com/blogs/population-stability-index-psi-banking-case-study/
# Weight of Evidence (WoE) Introductory Overview #: http://documentation.statsoft.com/StatisticaHelp.aspx?path=WeightofEvidence/WeightofEvidenceWoEIntroductoryOverview
# Case Study for a Credit Scorecard Analysis #: https://cn.mathworks.com/help/finance/case-study-for-a-credit-scorecard-analysis.html?requestedDomain=www.mathworks.com#zmw57dd0e33220
