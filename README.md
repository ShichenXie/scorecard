# scorecard

This R package makes the development of credit risk scorecard easily and efficiently by providing functions as follows: 
- information value (iv), 
- variable filter (var_filter), 
- optimal woe binning (woebin, woebin_ply, woebin_plot), 
- scorecard scaling (scorecard, scorecard_ply) 
- and performace evaluation (perf_plot, perf_psi).

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
library(data.table)
library(scorecard)

# Traditional Credit Scoring Using Logistic Regression
# load germancredit data
data("germancredit")

# rename creditability as y
dt <- setDT(germancredit)[, `:=`(
  y = ifelse(creditability == "bad", 1, 0),
  creditability = NULL
)]

# breaking dt into train and test ------
set.seed(125)
dt <- dt[sample(nrow(dt))]
# rowname of train
set.seed(345)
rn <- sample(nrow(dt), nrow(dt)*0.6)
# train and test dt
dt_train <- dt[rn]; dt_test <- dt[-rn];

# woe binning ------
bins <- woebin(dt_train, "y")

# converting train and test into woe values
train <- woebin_ply(dt_train, bins)
test <- woebin_ply(dt_test, bins)

# glm ------
m1 <- glm( y ~ ., family = "binomial", data = train)
# summary(m1)

# Select a formula-based model by AIC
m_step <- step(m1, direction="both")
m2 <- eval(m_step$call)
# summary(m2)

# performance ------
# predicted proability
train_pred <- predict(m2, type='response', train)
test_pred <- predict(m2, type='response', test)

# ks & roc plot
perf_plot(train$y, train_pred, title = "train")
perf_plot(train$y, train_pred, title = "test")

# score
card <- scorecard(bins, m2)

# credit score, only_total_score = TRUE
train_score <- scorecard_ply(dt_train, card)
test_score <- scorecard_ply(dt_test, card)

# psi
perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train[,"y"], test = test[, "y"]),
  x_limits = c(150, 750),
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
