# # Traditional Credit Scoring Using Logistic Regression
# library(scorecard)
#
# test_that('scorecard', {
#   # data prepare ------
#   # load germancredit data
#   data("germancredit")
#
#   # filter variable via missing rate, iv, identical value rate
#   dt_s = var_filter(germancredit, y="creditability")
#
#   # breaking dt into train and test
#   dt_list = split_df(dt_s, y="creditability", ratio = 0.6, seed = 30)
#   train = dt_list$train; test = dt_list$test;
#
#   # woe binning ------
#   bins = woebin(dt_s, y="creditability")
#   # woebin_plot(bins)
#
#   # binning adjustment
#   # # adjust breaks interactively
#   # breaks_adj = woebin_adj(dt_s, "creditability", bins)
#   # # or specify breaks manually
#   breaks_adj = list(
#     age.in.years=c(26, 35, 40),
#     other.debtors.or.guarantors=c("none", "co-applicant%,%guarantor"))
#   bins_adj = woebin(dt_s, y="creditability", breaks_list=breaks_adj)
#
#   # converting train and test into woe values
#   train_woe = woebin_ply(train, bins_adj)
#   test_woe = woebin_ply(test, bins_adj)
#
#   # glm ------
#   m1 = glm( creditability ~ ., family = binomial(), data = train_woe)
#   # summary(m1)
#
#   # # Adjusting for oversampling (support.sas.com/kb/22/601.html)
#   # library(data.table)
#   # p1=0.03; r1=0.3
#   # dt_woe = dt_woe[, weight := ifelse(y==1, p1/r1, (1-p1)/(1-r1) )]
#   #
#   # fmla = as.formula(paste("y ~", paste(names(dt_woe)[2:21], collapse="+")))
#   # m1 = glm(fmla, family = binomial(), data = dt_woe, weights = weight)
#
#
#   # Select a formula-based model by AIC (or by LASSO)
#   m_step = step(m1, direction="both", trace = FALSE)
#   m2 = eval(m_step$call)
#   # summary(m2)
#
#   # performance ks & roc ------
#   # predicted proability
#   train_pred = predict(m2, train_woe, type='response')
#   test_pred = predict(m2, test_woe, type='response')
#   # performance
#   train_perf = perf_eva(train$creditability, train_pred, title = "train")
#   test_perf = perf_eva(test$creditability, test_pred, title = "test")
#
#   # score ------
#   card = scorecard(bins_adj, m2)
#   # credit score
#   train_score = scorecard_ply(train, card, print_step=0)
#   test_score = scorecard_ply(test, card, print_step=0)
#
#   # psi
#   perf_psi(
#     score = list(train = train_score, test = test_score),
#     label = list(train = train$creditability, test = test$creditability)
#   )
# })
#
