# scorecard

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/scorecard)](https://cran.r-project.org/package=scorecard)
[![](http://cranlogs.r-pkg.org/badges/grand-total/scorecard)](https://cran.r-project.org/package=scorecard)
[![Travis-CI Build
Status](https://travis-ci.org/shichenxie/scorecard.svg?branch=master)](https://travis-ci.org/shichenxie/scorecard)


This R package makes the development of credit risk scorecard easily and efficiently by providing functions as follows: 
- information value (iv), 
- variable filter (var_filter), 
- optimal woe binning (woebin, woebin_ply, woebin_plot, woebin_adj), 
- scorecard scaling (scorecard, scorecard_ply) 
- and performace evaluation (perf_eva, perf_psi).

## Installation

1. Install the release version of `scorecard` from [CRAN](https://cran.r-project.org/package=scorecard) with:
``` r
install.packages("scorecard")
```

2. Install the latest version of `scorecard` from [github](https://github.com/ShichenXie/scorecard) with:
``` r
# install.packages("devtools")
devtools::install_github("shichenxie/scorecard")
```

## Example

This is a basic example which shows you how to develop a common credit risk scorecard:

``` r
# Traditional Credit Scoring Using Logistic Regression
library(scorecard)

# data prepare ------
# load germancredit data
data("germancredit")

# filter variable via missing rate, iv, identical value rate
dt_s = var_filter(germancredit, y="creditability")

# breaking dt into train and test
dt_list = split_df(dt_s, y="creditability", ratio = 0.6, seed = 30)
train = dt_list$train; test = dt_list$test;

# woe binning ------
bins = woebin(dt_s, y="creditability")
# woebin_plot(bins)

# binning adjustment
# # adjust breaks interactively
# breaks_adj = woebin_adj(dt_s, "creditability", bins) 
# # or specify breaks manually
breaks_adj = list(
  age.in.years=c(26, 35, 40),
  other.debtors.or.guarantors=c("none", "co-applicant%,%guarantor"))
bins_adj = woebin(dt_s, y="creditability", breaks_list=breaks_adj)

# converting train and test into woe values
train_woe = woebin_ply(train, bins_adj)
test_woe = woebin_ply(test, bins_adj)

# glm ------
m1 = glm( creditability ~ ., family = binomial(), data = train_woe)
# summary(m1)

# # Adjusting for oversampling (support.sas.com/kb/22/601.html)
# library(data.table)
# p1=0.03; r1=0.3
# dt_woe = dt_woe[, weight := ifelse(y==1, p1/r1, (1-p1)/(1-r1) )]
# 
# fmla = as.formula(paste("y ~", paste(names(dt_woe)[2:21], collapse="+")))
# m1 = glm(fmla, family = binomial(), data = dt_woe, weights = weight)


# Select a formula-based model by AIC (or by LASSO)
m_step = step(m1, direction="both", trace = FALSE)
m2 = eval(m_step$call)
# summary(m2)

# performance ks & roc ------
# predicted proability
train_pred = predict(m2, train_woe, type='response')
test_pred = predict(m2, test_woe, type='response')
# performance
train_perf = perf_eva(train$creditability, train_pred, title = "train")
test_perf = perf_eva(test$creditability, test_pred, title = "test")

# score ------
card = scorecard(bins_adj, m2)
# credit score
train_score = scorecard_ply(train, card, print_step=0)
test_score = scorecard_ply(test, card, print_step=0)

# psi
perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train$creditability, test = test$creditability)
)


# Session info ----------------------------------------------------------
#  setting  value                       
#  version  R version 3.4.3 (2017-11-30)
#  system   x86_64, darwin15.6.0        
#  ui       RStudio (1.1.414)           
#  language (EN)                        
#  collate  en_US.UTF-8                 
#  tz       Asia/Shanghai               
#  date     2018-02-10                  
# 
# Packages --------------------------------------------------------------
#  package    * version    date       source                          
#  base       * 3.4.3      2017-12-07 local                           
#  codetools    0.2-15     2016-10-05 CRAN (R 3.4.3)                  
#  colorspace   1.3-2      2016-12-14 CRAN (R 3.3.2)                  
#  compiler     3.4.3      2017-12-07 local                           
#  data.table   1.10.4-3   2017-10-27 cran (@1.10.4-)                 
#  datasets   * 3.4.3      2017-12-07 local                           
#  devtools     1.13.4     2017-11-09 CRAN (R 3.4.2)                  
#  digest       0.6.14     2018-01-14 CRAN (R 3.4.3)                  
#  doParallel   1.0.11     2017-09-28 CRAN (R 3.4.2)                  
#  foreach      1.4.4      2017-12-12 CRAN (R 3.4.3)                  
#  ggplot2      2.2.1.9000 2018-01-06 Github (hadley/ggplot2@4ee6c94) 
#  graphics   * 3.4.3      2017-12-07 local                           
#  grDevices  * 3.4.3      2017-12-07 local                           
#  grid         3.4.3      2017-12-07 local                           
#  gridExtra    2.3        2017-09-09 CRAN (R 3.4.1)                  
#  gtable       0.2.0      2016-02-26 CRAN (R 3.2.3)                  
#  iterators    1.0.9      2017-12-12 CRAN (R 3.4.3)                  
#  lazyeval     0.2.1      2017-10-29 cran (@0.2.1)                   
#  memoise      1.1.0      2017-04-21 CRAN (R 3.3.2)                  
#  methods    * 3.4.3      2017-12-07 local                           
#  munsell      0.4.3      2016-02-13 CRAN (R 3.2.3)                  
#  parallel     3.4.3      2017-12-07 local                           
#  plyr         1.8.4      2016-06-08 cran (@1.8.4)                   
#  Rcpp         0.12.15    2018-01-20 CRAN (R 3.4.3)                  
#  rlang        0.1.6      2017-12-21 CRAN (R 3.4.3)                  
#  scales       0.5.0.9000 2017-10-20 Github (hadley/scales@d767915)  
#  scorecard  * 0.1.6      2018-02-10 local                           
#  stats      * 3.4.3      2017-12-07 local                           
#  tibble       1.3.4      2017-08-22 CRAN (R 3.4.1)                  
#  tools        3.4.3      2017-12-07 local                           
#  utils      * 3.4.3      2017-12-07 local                           
#  withr        2.1.1.9000 2018-01-06 Github (jimhester/withr@df18523)
#  yaml         2.1.16     2017-12-12 cran (@2.1.16)  
```
