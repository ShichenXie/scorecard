#' Split a dataset
#'
#' Split a dataset into train and test
#'
#' @param dt A data frame.
#' @param y Name of y variable, Defaults to NULL. The input data will split based on the predictor y, if it is provide.
#' @param ratio A numeric value, Defaults to 0.7. It indicates the ratio of total rows contained in one split, must less than 1.
#' @param seed A random seed, Defaults to 618.
#' @param no_dfs Number of returned data frames. Defaults to 2.
#' @param name_dfs Name of returned data frames. If its length is not equal with no_dfs, then the names will seted as 'dX'. Defaults to train and test.
#'
#' @return A list of data frames
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
split_df = function(dt, y=NULL, ratio=c(0.7, 0.3), seed=618, no_dfs = 2, name_dfs = c('train', 'test')) {
  ind = NULL

  # set dt as data.table
  dt = setDT(copy(dt)) # setDT(dt)
  # remove date/time col
  # dt = rmcol_datetime_unique1(dt)
  # replace "" by NA
  # dt = rep_blank_na(dt)

  # set ratio range
  if (!is.numeric(ratio) || sum(ratio)>1) {
    warning("Incorrect inputs; ratio must be a numeric that length equal to 1 and less than 1. It was set to 0.7.")
    ratio = c(0.7, 0.3)
  } else {
    ratio_ = 1-sum(ratio)
    if (ratio_ > 0) ratio = c(ratio, ratio_)
  }
  # no_dfs
  if (!is.numeric(no_dfs)) {
    no_dfs = 2
  } else if (no_dfs > length(ratio)) no_dfs = length(ratio)
  # name_dfs
  if (length(name_dfs) != no_dfs) name_dfs = paste0('d',seq_len(no_dfs))

  # set seed and partition
  set.seed(seed)
  if (is.null(y)) {
    dt[, ind := sample(length(ratio), .N, replace=TRUE, prob=ratio)]
  } else {
    dt[, ind := sample(length(ratio), .N, replace=TRUE, prob=ratio), by=y]
  }

  # random sort
  lst_dfs = split(dt, by = 'ind', sorted = TRUE, keep.by = FALSE)[seq_len(no_dfs)]
  names(lst_dfs) = name_dfs
  return(lst_dfs)
}
