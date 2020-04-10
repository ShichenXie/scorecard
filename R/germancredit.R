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
#' @source \url{http://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)}
#' @examples
#' # load German credit data
#' data(germancredit)
#'
#' # structure of germancredit
#' str(germancredit)
#'
#' # summary of germancredit
#' # lapply(germancredit, summary)
#'
NULL

# the dataset is modified from woebinning package



# download data from website
# dt = setDT(read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data'))
#
# library(rvest)
# attrs = read_html('https://archive.ics.uci.edu/ml/datasets/Statlog+(German+Credit+Data)') %>%
#   html_nodes('p.normal') %>%
#   html_text() %>%
#   .[22] %>%
#   strsplit(' *\r(\t)* *') %>%
#   .[[1]] %>%
#   sub('Attibute', 'Attribute', .)
#
# attr_no = which(grepl('^Attribute', attrs))
#
# attrdt = data.table(atr = attrs)[
#   grepl('^Attribute', atr), var := atr
# # ][which(grepl('^Attribute', atr))+1, varNam := atr
# ][, var := var[1], by = cumsum(!is.na(var))
# # ][, varNam := varNam[1], by = cumsum(!is.na(varNam))
# ][, `:=`(
#   var = paste0('V', sub('Attribute (\\d+):\\s+\\((.+)\\)', '\\1', var)),
#   typ = sub('Attribute (\\d+):\\s+\\((.+)\\)', '\\2', var)
# )][typ == 'qualitative']
#
# atrlst = lapply(split(attrdt, by = 'var'), function(x) {
#   x[-c(1:2), .(atr)
#   ][, `:=`(
#     val1 = sub('^(A\\d+).+?$', '\\1', atr),
#     val2 = sub('.*?(A\\d+) : (.+)$', '\\2', atr)
#   )]
# })
#
# varNam = attrs[attr_no+1] %>%
#   tolower() %>%
#   gsub('[^(a-z)]+', '.', .) %>%
#   c(., 'creditability')
# setnames(dt, varNam)




# library(scorecard)
# library(data.table)
# data("germancredit")
# dat1 = scorecard:::check_y(germancredit, 'creditability', 'bad|1')
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
# dat = scorecard:::check_y(germancredit, 'creditability', 'bad|1')
# dtm = data.table(y=dat[[y]], variable=x, value=dat[[x]])

