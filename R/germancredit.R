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
#' # Load German credit data and create subset
#' data(germancredit)
#' df = germancredit[, c('creditability', 'credit.amount', 'duration.in.month',
#'                   'savings.account.and.bonds', 'purpose')]
#' # Display structure of the subset (data frame)
#' str(df)
NULL

# source: source code of woebinning package
