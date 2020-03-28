# # http://r-pkgs.had.co.nz/tests.html
# # https://testthat.r-lib.org/
# library(scorecard)
# library(data.table)
# data("germancredit")
#
# test_that('woebin, woebin_ply', {
#   # y provided
#   for (mth in c('tree', 'chimerge', 'freq', 'width')) {
#     bin1 = woebin(germancredit, y = 'creditability', method = mth, print_info=F)
#     expect_true(inherits(bin1, 'list'))
#     expect_true(inherits(bin1[[1]], 'data.frame'))
#     expect_true(inherits(rbindlist(bin1), 'data.frame'))
#
#     dat1_woe = woebin_ply(germancredit, bin1, print_info=F)
#     expect_true(inherits(dat1_woe, 'data.frame'))
#     dat2_woe = woebin_ply(germancredit, rbindlist(bin1), print_info=F)
#     expect_true(inherits(dat2_woe, 'data.frame'))
#   }
#
#   # y is not provided when method is in equal freq or width
#   for (mth in c('freq', 'width')) {
#     numeric_cols = names(germancredit)[setDT(copy(germancredit))[,sapply(.SD, is.numeric)]]
#     bin2  = woebin(germancredit, y=NULL, x=numeric_cols, print_info=F)
#     expect_true(inherits(bin2, 'list'))
#     expect_true(inherits(bin2[[1]], 'data.frame'))
#     expect_true(inherits(rbindlist(bin2), 'data.frame'))
#
#     dat1_bin = woebin_ply(germancredit[,numeric_cols], bin2, to = 'bin', print_info=F)
#     expect_true(inherits(dat1_bin, 'data.frame'))
#     dat2_bin = woebin_ply(germancredit[,numeric_cols], rbindlist(bin2), to = 'bin', print_info=F)
#     expect_true(inherits(dat2_bin, 'data.frame'))
#   }
#
#
# })
