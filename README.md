# scorecard

This R package makes the development of credit risk scorecard easily and efficiently by providing functions as follows: 
- information value (iv), 
- variable filter (var_filter), 
- optimal woe binning (woebin, woebin_ply, woebin_plot), 
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
library(data.table)
library(scorecard)

# load germancredit data
data("germancredit")

# rename creditability as y
dt = setDT(germancredit)[, `:=`(
  y = ifelse(creditability == "bad", 1, 0),
  creditability = NULL
)]

# filter variable via missing rate, iv, identical value rate
dt_s = var_filter(dt, "y")

# breaking dt into train and test ------
dt_list = split_df(dt_s, y="y", ratio = 0.6, seed = 30)
train = dt_list$train; test = dt_list$test;

# woe binning ------
bins = woebin(dt_s, "y", print_step = 5)
# woebin_plot(bins)

# binning adjustment
# breaks_adj = woebin_adj(bins, dt_s, "y") # adjust breaks interactively
breaks_adj = list(age.in.years=c(26, 35, 40)) # or specify breaks manually
bins_adj = woebin(dt_s,"y", breaks_list=breaks_adj, print_step = 5)

# converting train and test into woe values
train_woe = woebin_ply(train, bins_adj, print_step = 5)
test_woe = woebin_ply(test, bins_adj, print_step = 5)

# glm ------
m1 = glm( y ~ ., family = "binomial", data = train_woe)
# summary(m1)

# Select a formula-based model by AIC
m_step = step(m1, direction="both", trace = FALSE)
m2 = eval(m_step$call)
# summary(m2)

# performance ------
# predicted proability
train_pred = predict(m2, train_woe, type='response')
test_pred = predict(m2, test_woe, type='response')

# ks & roc plot
train_perf = perf_eva(train$y, train_pred, title = "train")
test_perf = perf_eva(test$y, test_pred, title = "test")

# score
card = scorecard(bins_adj, m2)

# credit score, only_total_score = TRUE
train_score = scorecard_ply(train, card, print_step = 0)
test_score = scorecard_ply(test, card, print_step = 0)

# psi
perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train$y, test = test$y),
  x_limits = c(250, 700),
  x_tick_break = 50
  )


# Session info ------
#  setting  value                       
#  version  R version 3.4.1 (2017-06-30)
#  system   x86_64, darwin15.6.0        
#  ui       X11                         
#  language (EN)                        
#  collate  C                           
#  tz       Asia/Shanghai               
# 
# Packages ------
#  package    * version date       source        
#  base       * 3.4.1   2017-07-07 local         
#  colorspace   1.3-2   2016-12-14 CRAN (R 3.3.2)
#  compiler     3.4.1   2017-07-07 local         
#  data.table * 1.10.4  2017-02-01 CRAN (R 3.4.0)
#  datasets   * 3.4.1   2017-07-07 local         
#  devtools     1.13.3  2017-08-02 CRAN (R 3.4.1)
#  digest       0.6.12  2017-01-27 CRAN (R 3.3.2)
#  ggplot2      2.2.1   2016-12-30 CRAN (R 3.4.0)
#  graphics   * 3.4.1   2017-07-07 local         
#  grDevices  * 3.4.1   2017-07-07 local         
#  grid         3.4.1   2017-07-07 local         
#  gridExtra    2.3     2017-09-09 CRAN (R 3.4.1)
#  gtable       0.2.0   2016-02-26 CRAN (R 3.2.3)
#  lazyeval     0.2.0   2016-06-12 cran (@0.2.0) 
#  memoise      1.1.0   2017-04-21 CRAN (R 3.3.2)
#  methods    * 3.4.1   2017-07-07 local         
#  munsell      0.4.3   2016-02-13 CRAN (R 3.2.3)
#  plyr         1.8.4   2016-06-08 cran (@1.8.4) 
#  Rcpp         0.12.12 2017-07-15 CRAN (R 3.4.1)
#  rlang        0.1.2   2017-08-09 CRAN (R 3.4.1)
#  scales       0.5.0   2017-08-24 CRAN (R 3.4.1)
#  scorecard  * 0.1.0   2017-09-30 local         
#  stats      * 3.4.1   2017-07-07 local         
#  tibble       1.3.4   2017-08-22 CRAN (R 3.4.1)
#  utils      * 3.4.1   2017-07-07 local         
#  withr        2.0.0   2017-07-28 CRAN (R 3.4.1)
```
