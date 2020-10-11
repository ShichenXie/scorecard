#' Information Value
#'
#' This function calculates information value (IV) for multiple x variables. It treats each unique value in x variables as a group. If there is a zero number of y class, it will be replaced by 0.99 to make sure woe/iv is calculable.
#'
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. Defaults to NULL. If x is NULL, then all columns except y are counted as x variables.
#' @param positive Value of positive class, Defaults to "bad|1".
#' @param order Logical, Defaults to TRUE. If it is TRUE, the output will descending order via iv.
#'
#' @return A data frame with columns for variable and info_value
#'
#' @details IV is a very useful concept for variable selection while developing credit scorecards. The formula for information value is shown below: \deqn{IV = \sum(DistributionPositive_{i} - DistributionNegative_{i})*\ln(\frac{DistributionPositive_{i}}{DistributionNegative_{i}}).} The log component in information value is defined as weight of evidence (WOE), which is shown as \deqn{WeightofEvidence = \ln(\frac{DistributionPositive_{i}}{DistributionNegative_{i}}).}
#' The relationship between information value and predictive power is as follows:
#' \tabular{rr}{
#' Information Value \tab Predictive Power \cr
#' ----------------- \tab ---------------- \cr
#'      < 0.02 \tab useless for prediction \cr
#' 0.02 to 0.1 \tab Weak predictor \cr
#'  0.1 to 0.3 \tab Medium predictor \cr
#'       > 0.3 \tab Strong predictor
#' }
#'
#' @examples
#' # Load German credit data
#' data(germancredit)
#'
#' # information values
#' info_value = iv(germancredit, y = "creditability")
#'
#' str(info_value)
#'
#' @import data.table
#' @export
#'
iv = function(dt, y, x=NULL, positive="bad|1", order=TRUE) {
  info_value = label = NULL # no visible binding for global variable

  # set dt as data.table
  dt = setDT(copy(dt)) #copy(setDT(dt))
  if (!is.null(x)) dt = dt[, c(y,x), with=FALSE]
  # check y
  dt = check_y(dt, y, positive)
  # # remove date/time col
  # dt = rmcol_datetime_unique1(dt)
  # # replace "" by NA
  # dt = rep_blank_na(dt)
  # x variable names
  x = x_variable(dt, y, x)

  # data prep
  dt = dt[
    , x, with = FALSE
  ][, `:=`(
    rowid = .I, label = dt[[y]]
  )]

  # info_value
  ivlist = dt[, sapply(.SD, iv_xy, label), .SDcols = x]

  ivlist = data.table(variable=names(ivlist), info_value=ivlist)
  if (order) ivlist = ivlist[order(-info_value)]

  return(ivlist)
}
#' @import data.table
iv_xy = function(x, y) {
  . = Distrpos = Distrneg = pos = neg = NULL

  data.table(x=x, y=y)[
    , .(neg = sum(y==0), pos = sum(y==1)), keyby="x"
    ][, (c("neg", "pos")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("neg", "pos")# replace 0 by 0.99 in neg/pos columns
    ][, `:=`(
      Distrneg = neg/sum(neg), Distrpos = pos/sum(pos)
   )][, sum((Distrpos-Distrneg)*log(Distrpos/Distrneg)) ]

}


# #' Information Value
# #'
# #' calculating IV of total based on neg and pos vectors
# #'
# #' @param neg vector of neg numbers
# #' @param pos vector of pos numbers
# #'
# #' @examples
# #' # iv_01(neg, pos)
# #' dtm = melt(dt, id = 'creditability')[, .(
# #' neg = sum(creditability=="neg"), pos = sum(creditability=="pos")
# #' ), keyby = c("variable", "value")]
# #'
# #' dtm[, .(iv = lapply(.SD, iv_01, pos)), by="variable", .SDcols# ="neg"]
# #'
# #' @import data.table
#' @import data.table
#'
iv_01 = function(neg, pos) {
  # global variables
  Distrpos = Distrneg = miv = NULL

  data.table(
    neg = neg, pos = pos
  )[, (c("neg", "pos")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("neg", "pos") # replace 0 by 0.99 in neg/pos column
  ][, `:=`(Distrneg = neg/sum(neg), Distrpos = pos/sum(pos) )
  ][, miv := (Distrpos-Distrneg)*log(Distrpos/Distrneg)
  ][, sum(miv)]

}

# #' miv_01
# #'
# #' calculating IV of each bin based on neg and pos vectors
# #'
# #' @param neg vector of neg numbers
# #' @param pos vector of pos numbers
# #'
# #' @import data.table
# #'
#' @import data.table
#'
miv_01 = function(neg, pos) {
  # global variables
  Distrpos = Distrneg = miv = NULL

  data.table(
    neg = neg, pos = pos
  )[, (c("neg", "pos")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("neg", "pos") # replace 0 by 0.99 in neg/pos column
  ][, `:=`(Distrneg = neg/sum(neg), Distrpos = pos/sum(pos) )
  ][, miv := (Distrpos-Distrneg)*log(Distrpos/Distrneg)
  ][, miv]
}

# #' woe_01
# #'
# #' calculating WOE of each bin based on neg and pos vectors
# #'
# #' @param neg vector of neg numbers
# #' @param pos vector of pos numbers
# #'
# #' @import data.table
#' @import data.table
#'
woe_01 = function(neg, pos) {
  # global variables
  Distrpos = Distrneg = woe = NULL

  data.table(
    neg = neg, pos = pos
  )[, (c("neg", "pos")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("neg", "pos") # replace 0 by 0.99 in neg/pos column
  ][, `:=`(Distrneg = neg/sum(neg), Distrpos = pos/sum(pos) )
  ][, woe := log(Distrpos/Distrneg)
  ][, woe]
}
