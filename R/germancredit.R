#' German Credit Data
#'
#' Credit data that classifies debtors described by
#' a set of attributes as good or bad credit risks.
#' See source link below for detailed information.
#'
#' @docType data
#' @keywords data
#' @name germancredit
#' @usage data(germancredit)
#' @format A data frame with 21 variables
#' (numeric and factors) and 1000 observations.
#' @source \url{https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)}
#' @examples
#' # load German credit data
#' data(germancredit)
#'
#' # structure of germancredit
#' str(germancredit)
#'
#' # summary of germancredit
#' lapply(germancredit, summary)
#'
NULL

# the dataset is modified from woebinning package



# library(scorecard)
# library(data.table)
# data("germancredit")
# dat1 = check_y(germancredit, 'creditability', 'bad|1')
#
# dat2 = data.table(creditability=sample(0:1, 50, replace=TRUE))
# dat = rbind(dat1, dat2, fill=TRUE)
#
# # y
# y = "creditability"
#
#
# # x
# ## numerical data
# x = 'age.in.years' #'number.of.existing.credits.at.this.bank' # "credit.amount" # "foreign.worker
# spl_val = c(2600, 9960, "6850%,%missing")
# breaks = c(2000, 4000, 6000)
# breaks = c('26%,%missing', 28, 35, 37)
#
#
# ## categorical data
# x = 'housing' # "job" # "credit.amount"; #
# breaks = c("own", "for free%,%rent%,%missing")
# breaks = c("own", "for free%,%rent")
#
# dat = check_y(germancredit, 'creditability', 'bad|1')
# dtm = data.table(y=dat[[y]], variable=x, value=dat[[x]])

