#' Split a dataset
#'
#' @param dt A data frame.
#' @param y Name of y variable, default is NULL. The input data will split based on the predictor y, if it is provide.
#' @param ratio A numeric value, default is 0.7. It indicates the ratio of total rows contained in one split, must less than 1.
#' @param seed A random seed, default is 186.
#'
#' @examples
#' # load German credit data
#' data(germancredit)
#'
#' # Example I
#' dt_list = split_df(germancredit, y="creditability")
#' train = dt_list[[1]]
#' test = dt_list[[2]]
#'
#' # dimensions of train and test datasets
#' lapply(dt_list, dim)
#'
#'
#' # Example II
#' dt_list2 = split_df(germancredit, y="creditability", ratio = c(0.5, 0.2))
#' lapply(dt_list2, dim)
#'
#' @import data.table
#' @export
split_df = function(dt, y=NULL, ratio=0.7, seed=186) {
  ind = NULL

  # set dt as data.table
  dt = setDT(dt)
  # remove date/time col
  dt = rmcol_datetime_unique1(dt)
  # replace "" by NA
  dt = rep_blank_na(dt)

  # set ratio range
  if (!is.numeric(ratio) || length(ratio) >2 || sum(ratio)>1) {
    warning("Incorrect inputs; ratio must be a numeric that length equal to 1 and less than 1. It was set to 0.7.")
    ratio = c(0.7, 0.3)
  } else {
    ratio_ = 1-sum(ratio)
    if (ratio_ > 0) ratio = c(ratio, ratio_)
  }

  # set seed and partition
  set.seed(seed)
  if (is.null(y)) {
    dt[, ind := sample(length(ratio), .N, replace=TRUE, prob=ratio)]
  } else {
    dt[, ind := sample(length(ratio), .N, replace=TRUE, prob=ratio), by=y]
  }

  # random sort
  rt = list()
  rt$train = dt[ind == 1,][, ind := NULL]
  rt$test  = dt[ind == 2,][, ind := NULL]
  return(rt)
}
