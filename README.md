# scorecard

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/scorecard)](https://cran.r-project.org/package=scorecard)
[![](http://cranlogs.r-pkg.org/badges/grand-total/scorecard)](https://cran.r-project.org/package=scorecard)
[![Travis build status](https://travis-ci.org/ShichenXie/scorecard.svg?branch=master)](https://travis-ci.org/ShichenXie/scorecard)


The goal of `scorecard` package is to make the development of the traditional credit risk scorecard model easier and efficient by providing functions for some common tasks that summarized in below. This package can also used in the development of machine learning models on binomial classification. 

- data preparation (`split_df`, `one_hot`)
- variable selection (`var_filter`, `iv`, `vif`)
- weight of evidence (woe) binning (`woebin`, `woebin_plot`, `woebin_adj`, `woebin_ply`)
- performance evaluation (`perf_eva`, `perf_psi`)
- scorecard scaling (`scorecard`, `scorecard_ply`)
- scorecard report (`gains_table`, `report`)


## Installation

- Install the release version of `scorecard` from [CRAN](https://cran.r-project.org/package=scorecard) with:
``` r
install.packages("scorecard")
```

- Install the latest version of `scorecard` from [github](https://github.com/ShichenXie/scorecard) with:
``` r
# install.packages("devtools")
devtools::install_github("shichenxie/scorecard")
```

## Example

This is a basic example which shows you how to develop a common credit risk scorecard:

``` r
# Traditional Credit Scoring Using Logistic Regression
library(scorecard)

# data preparing ------
# load germancredit data
data("germancredit")
# filter variable via missing rate, iv, identical value rate
dt_f = var_filter(germancredit, y="creditability")
# breaking dt into train and test
dt_list = split_df(dt_f, y="creditability", ratio = 0.6, seed = 30)
label_list = lapply(dt_list, function(x) x$creditability)

# woe binning ------
bins = woebin(dt_f, y="creditability")
# woebin_plot(bins)

# binning adjustment
## adjust breaks interactively
# breaks_adj = woebin_adj(dt_f, "creditability", bins) 
## or specify breaks manually
breaks_adj = list(
  age.in.years=c(26, 35, 40),
  other.debtors.or.guarantors=c("none", "co-applicant%,%guarantor"))
bins_adj = woebin(dt_f, y="creditability", breaks_list=breaks_adj)

# converting train and test into woe values
dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins_adj))

# glm ------
m1 = glm( creditability ~ ., family = binomial(), data = dt_woe_list$train)
# vif(m1, merge_coef = TRUE) # summary(m1)
# Select a formula-based model by AIC (or by LASSO for large dataset)
m_step = step(m1, direction="both", trace = FALSE)
m2 = eval(m_step$call)
# vif(m2, merge_coef = TRUE) # summary(m2)

# # Adjusting for oversampling (support.sas.com/kb/22/601.html)
# library(data.table)
# p1=0.03 # bad probability in population 
# r1=0.3 # bad probability in sample dataset
# dt_woe = copy(dt_woe_list$train)[, weight := ifelse(creditability==1, p1/r1, (1-p1)/(1-r1) )][]
# fmla = as.formula(paste("creditability ~", paste(names(coef(m2))[-1], collapse="+")))
# m3 = glm(fmla, family = binomial(), data = dt_woe, weights = weight)

# performance ks & roc ------
## predicted proability
pred_list = lapply(dt_woe_list, function(x) predict(m2, x, type='response'))
## performance
perf = perf_eva(pred = pred_list, label = label_list)

# score ------
## scorecard
card = scorecard(bins_adj, m2)
## credit score
score_list = lapply(dt_list, function(x) scorecard_ply(x, card))
## psi
perf_psi(score = score_list, label = label_list)

```
