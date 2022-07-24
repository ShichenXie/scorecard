library(scorecard)

# load germancredit data
data("germancredit")
# variable filter 1: missing rate, iv, identical value rate ------
dt_f1 = var_filter(germancredit, y="creditability", var_rm_reason = TRUE)
# dt_f1$rm

# split dt into train and test
dt_list = split_df(dt_f1$dt, y="creditability")
label_list = lapply(dt_list, function(x) x$creditability)

# woe binning ------
bins = woebin(dt_list$train, y="creditability")
# woebin_plot(bins)

# binning adjustment
## adjust breaks interactively
# breaks_adj = woebin_adj(dt_f1, "creditability", bins)
## or specify breaks manually
breaks_adj = list(
  age.in.years=c(26, 35, 40),
  other.debtors.or.guarantors=c("none", "co-applicant%,%guarantor"))
bins_adj = woebin(dt_list$train, y="creditability", breaks_list=breaks_adj)

# converting train and test into woe values
dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins_adj))

# variable filter 2: missing rate, iv, identical value rate, step and vif ------
dt_f2 = var_filter(dt_woe_list$train, y="creditability", var_rm_reason = TRUE, lr = TRUE)
# dt_f2$rm

# glm
m1 = glm( creditability ~ ., family = binomial(), data = dt_f2$dt)
# vif(m1, merge_coef = TRUE) # summary(m1)

# performance ------
## predicted proability
pred_list = lapply(dt_woe_list, function(x) predict(m1, x, type='response'))
## performance
perf = perf_eva(pred = pred_list, label = label_list)

## Adjusting for oversampling (support.sas.com/kb/22/601.html)
# card_prob_adj = scorecard2(bins_adj, dt=dt_list$train, y='creditability',
#                x=sub('_woe$','',names(coef(m1))[-1]), badprob_pop=0.03, return_prob=TRUE)
# perf_adj = perf_eva(pred = card_prob_adj$prob, label = label_list$train)

# score ------
## scorecard
card = scorecard(bins_adj, m1)
## credit score
score_list = lapply(dt_list, function(x) scorecard_ply(x, card))
## psi
perf_psi(score = score_list, label = label_list)

# gains_table ----
gtbl = gains_table(score_list, label_list, bin_num = 10)
library(ggplot2)
ggplot(data = gtbl) +
  geom_line(aes(x = approval_rate, y = cum_posprob, color = datset)) +
  labs(x = 'Approval Rate', y = 'Cumulative Positive Probability')


