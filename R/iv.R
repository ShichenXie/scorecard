#' information values (iv)
#'
#' This function calculate woe and iv values.
#'
#' @name iv
#' @param dt Name of data.frame/data.table with input data.
#' @param y Name of y variable.
#' @param x Name vector of x variables.
#' @param positive The positive/bad target event, such as "bad" or 1.
#' @return List of woe and iv data tables.
#' @export
#' @examples
#' # Load German credit data and create good and bad series
#' data(germancredit)
#' dt <- germancredit[, c('creditability', 'credit.amount', 'age.in.years')]
#'
#' # iv(dt, y)
#' iv(dt, y = "creditability")
iv <- function(dt, y, x="", positive="bad|1", order="TRUE") {
  if (x=="") x <- setdiff(names(dt), y)

  dt <- data.table(dt)[
    , x, with = FALSE
    ][, `:=`(
      rowid = as.integer(row.names(.SD)),
      y = ifelse(grepl(positive, dt[[y]]), 1, 0)
    )]

  ivlist <- melt( dt, id = c("rowid", "y") )[
    , .(good = sum(y==0), bad = sum(y==1), count=.N), keyby=c("variable", "value")

    ][, (c("good", "bad")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("good", "bad")# replace 0good/bad by 0.99

      ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad) ), by="variable"
        ][, `:=`(woe = log(DistrBad/DistrGood), miv = log(DistrBad/DistrGood)*(DistrBad-DistrGood) )
          ][, sum(miv), by="variable"]

  if (order==TRUE) {
    return(ivlist[order(-V1)])
  } else {
    return(ivlist)
  }

}

# #' @rdname iv
# #' @param good vector of good numbers
# #' @param bad vector of bad numbers
# #' @return The iv of \code{good} and \code{bad}
# #' @export
# #' @examples
# #'
# #' # iv_01(good, bad)
# #' dtm <- melt(dt, id = 'creditability')[, .(
# #' good = sum(creditability=="good"), bad = sum(creditabilit=="bad")
# #' ), keyby = c("variable", "value")]
# #'
# #' dtm[, .(iv = lapply(.SD, iv_01, bad)), by="variable", .SDcols="good"]
iv_01 <- function(good, bad) {
  data.table(
    good = good, bad = bad
  )[, (c("good", "bad")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("good", "bad") # replace 0good/bad by 0.99

  ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad) )
  ][, `:=`(miv = log(DistrBad/DistrGood)*(DistrBad-DistrGood) )
  ][, sum(miv)]

}

miv_01 <- function(good, bad) {
  data.table(
    good = good, bad = bad
  )[, (c("good", "bad")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("good", "bad") # replace 0good/bad by 0.99

    ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad) )
      ][, `:=`( miv = log(DistrBad/DistrGood)*(DistrBad-DistrGood) )
        ][, miv]
}

woe_01 <- function(good, bad) {
  data.table(
    good = good, bad = bad
  )[, (c("good", "bad")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("good", "bad") # replace 0good/bad by 0.99

    ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad) )
      ][, `:=`(woe = log(DistrBad/DistrGood))
        ][, woe]
}


# reference
# Weight of Evidence (WoE) Introductory Overview
# http://ucanalytics.com/blogs/data-visualization-case-study-banking/
# http://documentation.statsoft.com/StatisticaHelp.aspx?path=WeightofEvidence/WeightofEvidenceWoEIntroductoryOverview
