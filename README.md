# scorecard

[![Travis build status](https://travis-ci.org/ShichenXie/scorecard.svg?branch=master)](https://travis-ci.org/ShichenXie/scorecard)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/scorecard)](https://cran.r-project.org/package=scorecard)
[![](http://cranlogs.r-pkg.org/badges/grand-total/scorecard)](https://cran.r-project.org/package=scorecard)
[![](http://cranlogs.r-pkg.org/badges/scorecard)](https://cran.r-project.org/package=scorecard)


The goal of `scorecard` package is to make the development of the traditional credit risk scorecard model easier and efficient by providing functions for some common tasks that summarized in below. This package can also used in the development of machine learning models on binary classification. 

- data preprocessing (`split_df`, `replace_na`, `one_hot`, `var_scale`)
- weight of evidence (woe) binning (`woebin`, `woebin_plot`, `woebin_adj`, `woebin_ply`)
- variable selection (`var_filter`, `iv`, `vif`)
- performance evaluation (`perf_eva`, `perf_cv`, `perf_psi`)
- scorecard scaling (`scorecard`, `scorecard2`, `scorecard_ply`)
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
dt_list = split_df(dt_f, y="creditability", ratio = c(0.6, 0.4), seed = 30)
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

# glm / selecting variables ------
m1 = glm( creditability ~ ., family = binomial(), data = dt_woe_list$train)
# vif(m1, merge_coef = TRUE) # summary(m1)
# Select a formula-based model by AIC (or by LASSO for large dataset)
m_step = step(m1, direction="both", trace = FALSE)
m2 = eval(m_step$call)
# vif(m2, merge_coef = TRUE) # summary(m2)

# performance ks & roc ------
## predicted proability
pred_list = lapply(dt_woe_list, function(x) predict(m2, x, type='response'))
## Adjusting for oversampling (support.sas.com/kb/22/601.html)
# card_prob_adj = scorecard2(bins_adj, dt=dt_list$train, y='creditability', 
#                x=sub('_woe$','',names(coef(m2))[-1]), badprob_pop=0.03, return_prob=TRUE)
                
## performance
perf = perf_eva(pred = pred_list, label = label_list)
# perf_adj = perf_eva(pred = card_prob_adj$prob, label = label_list$train)

# score ------
## scorecard
card = scorecard(bins_adj, m2)
## credit score
score_list = lapply(dt_list, function(x) scorecard_ply(x, card))
## psi
perf_psi(score = score_list, label = label_list)

```

## Related Articles

- [A Practical Guide On Building Credit Scorecards Using R](https://rpubs.com/ngyongkad/scorecard)
- [WOE, IV and Scorecards in Credit Risk Modelling](https://rstudio-pubs-static.s3.amazonaws.com/376828_032c59adbc984b0ab892ce0026370352.html)
- [Data Exploration with Weight of Evidence and Information Value in R](https://multithreaded.stitchfix.com/blog/2015/08/13/weight-of-evidence/)

- [A Framework for Scorecard Modelling using R](https://crc.business-school.ed.ac.uk/wp-content/uploads/sites/55/2018/01/20-Gero-Szepannek.pdf)
- [Banking Risk Case Study Example](http://ucanalytics.com/blogs/category/risk-analytics/banking-risk-case-study-example/)
- [Credit Scoring](https://www.worldprogramming.com/blog/credit_scoring_development_pt1)
- [Case Study for a Credit Scorecard Analysis](https://cn.mathworks.com/help/finance/case-study-for-a-credit-scorecard-analysis.html?requestedDomain=www.mathworks.com&requestedDomain=true#zmw57dd0e33220)

- [Examining Distributional Shifts by Using Population Stability Index (PSI) for Model Validation and Diagnosis](https://www.lexjansen.com/wuss/2017/47_Final_Paper_PDF.pdf)
- [Adjusting the Oversampling/Undersampling in Logistic Regression](http://support.sas.com/kb/22/601.html)
