library(testthat)
library(scorecard)
# library(data.table)

test_check("scorecard")

# # Profiling
# https://support.rstudio.com/hc/en-us/articles/218221837-Profiling-with-RStudio
# data("germancredit")
# dat = rbindlist(rep(list(d = germancredit), 100))
# dat[sample(100000, 1000), sample(20, 3)] <- ''
# library(profvis)
# profvis({
#   bins = woebin(dat, 'creditability')
# })
