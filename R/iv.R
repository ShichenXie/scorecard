#' information values
#'
#' This function calculates IV.
#'
#' @name iv
#' @param dt Name of data.frame/data.table with input data.
#' @param y Name of y variable.
#' @param x Name vector of x variables.
#' @param positive The positive/bad target event, such as "bad" or 1.
#' @return List of woe and iv data tables.
#' @export
#' @examples
#' # Load German credit data
#' data(germancredit)
#'
#' # information values
#' iv(germancredit, y = "creditability")
iv <- function(dt, y, x=NA, positive="bad|1", order="TRUE") {
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

# #' @rdname iv
# #' @param good vector of good numbers
# #' @param bad vector of bad numbers
# #' @return The iv of \code{good} and \code{bad}
# #' @export
# #' @examples
# #'
# #' # iv_01(good, bad)
# #' dtm <- melt(dt, id = 'creditability')[, .(
# #' good = sum(creditability=="good"), bad = sum(creditability=="bad")
# #' ), keyby = c("variable", "value")]
# #'
# #' dtm[, .(iv = lapply(.SD, iv_01, bad)), by="variable", .SDcols="good"]

# calculating IV of total based on good and bad vectors
iv_01 <- function(good, bad) {
  data.table(
    good = good, bad = bad
  )[, (c("good", "bad")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("good", "bad") # replace 0 by 0.99 in good/bad column
  ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad) )
  ][, `:=`(miv = log(DistrBad/DistrGood)*(DistrBad-DistrGood) )
  ][, sum(miv)]

}

# calculating IV of each bin based on good and bad vectors
miv_01 <- function(good, bad) {
  data.table(
    good = good, bad = bad
  )[, (c("good", "bad")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("good", "bad") # replace 0 by 0.99 in good/bad column
  ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad) )
  ][, `:=`( miv = log(DistrBad/DistrGood)*(DistrBad-DistrGood) )
  ][, miv]
}

# calculating WOE of each bin based on good and bad vectors
woe_01 <- function(good, bad) {
  data.table(
    good = good, bad = bad
  )[, (c("good", "bad")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("good", "bad") # replace 0 by 0.99 in good/bad column
  ][, `:=`(DistrGood = good/sum(good), DistrBad = bad/sum(bad) )
  ][, `:=`(woe = log(DistrBad/DistrGood))
  ][, woe]
}
