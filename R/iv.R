#' Information Value
#'
#' This function calculates information value (IV) for multiple x variables.
#'
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name vector of x variables. Default NA. If x is NA, all variables exclude y will counted as x variables.
#' @param positive Value of positive class, default "bad|1".
#' @param order Logical. If it is TRUE, return descending sorted iv values.
#'
#' @return Information Value
#' @details IV is a very useful concept for variable selection while developing credit scorecards. The formula for information value is shown below: \deqn{IV = \sum(DistributionBad_{i} - DistributionGood_{i})*\ln(\frac{DistributionBad_{i}}{DistributionGood_{i}}).} The log component in information value is defined as weight of evidence (WOE), which is shown as \deqn{WeightofEvidence = \ln(\frac{DistributionBad_{i}}{DistributionGood_{i}}).} The relationship between information value and predictive power is as follows: <0.02 (useless for prediction), 0.02 to 0.1 (Weak predictor), 0.1 to 0.3 (Medium predictor), 0.3 to 0.5 (Strong predictor) and >0.5 (Suspicious or too good to be true).
#'
#' @examples
#' # Load German credit data
#' data(germancredit)
#'
#' # information values
#' iv(germancredit, y = "creditability")
#'
#' @import data.table
#' @export
#'
iv <- function(dt, y, x=NA, positive="bad|1", order="TRUE") {
  good = bad = DistrBad = DistrGood = miv = V1 = . = NULL # no visible binding for global variable

  if (anyNA(x) & length(x)==1) x <- setdiff(names(dt), y)

  dt <- data.table(dt)[
    , x, with = FALSE
    ][, `:=`(
      rowid = as.integer(row.names(.SD)),
      y = ifelse(grepl(positive, dt[[y]]), 1, 0)
    )]

  ivlist <- melt( dt, id = c("rowid", "y") )[
    , .(good = sum(y==0), bad = sum(y==1), count=.N), keyby=c("variable", "value")
    ][, (c("good", "bad")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("good", "bad")# replace 0 by 0.99 in good/bad column
    ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad) ), by="variable"
    ][, `:=`(
      # woe = log(DistrBad/DistrGood),
      miv = log(DistrBad/DistrGood)*(DistrBad-DistrGood)
   )][, sum(miv), by="variable"]

  if (order==TRUE) {
    return(ivlist[order(-V1)])
  } else {
    return(ivlist)
  }

}


# #' Information Value
# #'
# #' calculating IV of total based on good and bad vectors
# #'
# #' @param good vector of good numbers
# #' @param bad vector of bad numbers
# #'
# #' @examples
# #' # iv_01(good, bad)
# #' dtm <- melt(dt, id = 'creditability')[, .(
# #' good = sum(creditability=="good"), bad = sum(creditability=="bad")
# #' ), keyby = c("variable", "value")]
# #'
# #' dtm[, .(iv = lapply(.SD, iv_01, bad)), by="variable", .SDcols# ="good"]
# #'
# #' @import data.table
iv_01 <- function(good, bad) {
  DistrBad = DistrGood = miv = NULL # no visible binding for global variable

  data.table(
    good = good, bad = bad
  )[, (c("good", "bad")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("good", "bad") # replace 0 by 0.99 in good/bad column
  ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad) )
  ][, `:=`(miv = log(DistrBad/DistrGood)*(DistrBad-DistrGood) )
  ][, sum(miv)]

}

# #' miv_01
# #'
# #' calculating IV of each bin based on good and bad vectors
# #'
# #' @param good vector of good numbers
# #' @param bad vector of bad numbers
# #'
# #' @import data.table
# #'
miv_01 <- function(good, bad) {
  DistrBad = DistrGood = miv = NULL # no visible binding for global variable

  data.table(
    good = good, bad = bad
  )[, (c("good", "bad")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("good", "bad") # replace 0 by 0.99 in good/bad column
  ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad) )
  ][, `:=`( miv = log(DistrBad/DistrGood)*(DistrBad-DistrGood) )
  ][, miv]
}

# #' woe_01
# #'
# #' calculating WOE of each bin based on good and bad vectors
# #'
# #' @param good vector of good numbers
# #' @param bad vector of bad numbers
# #'
# #' @import data.table
woe_01 <- function(good, bad) {
  DistrBad = DistrGood = woe = NULL # no visible binding for global variable

  data.table(
    good = good, bad = bad
  )[, (c("good", "bad")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("good", "bad") # replace 0 by 0.99 in good/bad column
  ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad) )
  ][, `:=`(woe = log(DistrBad/DistrGood))
  ][, woe]
}
