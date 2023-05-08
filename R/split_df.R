#' Split a Data Frame
#'
#' Split a data frame into multiple data sets according to the specified ratios.
#'
#' @param dt A data frame.
#' @param y Name of y variable, Defaults to NULL. The input data will split based on the predictor y, if it is provide.
#' @param ratios A numeric vector indicating the ratio of total rows contained in each split, defaults to c(0.7, 0.3).
#' @param name_dfs Name of returned data frames. Its length should equals to the ratios'. Defaults to train and test.
#' @param oot out-of-time validation data set parameters.
#' @param seed A random seed, Defaults to 618.
#' @param ... Additional parameters.
#'
#' @return A list of data frames
#'
#' @examples
#' # load German credit data
#' data(germancredit)
#'
#' # Example I
#' dt_list = split_df(germancredit, y="creditability")
#'
#' # dimensions of each split data sets
#' lapply(dt_list, dim)
#'
#'
#' # Example II
#' dt_list2 = split_df(germancredit, y="creditability",
#'   ratios = c(0.5, 0.3, 0.2),
#'   name_dfs = c('train', 'test', 'valid'))
#' lapply(dt_list2, dim)
#'
#' @import data.table
#' @export
split_df = function(dt, y=NULL, ratios=c(0.7, 0.3), name_dfs=c('train', 'test'), oot=list(order=NULL, start=NULL, ratio=NULL), seed=618, ...) {
  UseMethod('split_df')
}

#' @export
split_df.data.frame = function(dt, y=NULL, ratios=c(0.7, 0.3), name_dfs=c('train', 'test'), oot=list(order=NULL, start=NULL, ratio=NULL), seed=618, ...) {
  ind = NULL

  # set dt as data.table
  dt = setDT(copy(dt)) # setDT(dt)
  # remove date/time col
  # dt = rmcol_datetime_unique1(dt)
  # replace "" by NA
  # dt = rep_blank_na(dt)
  kwargs = list(...)
  ratio = kwargs[['ratio']]
  if (!is.null(ratio)) ratios = ratio

  # set ratios
  if (length(name_dfs) == 2 & length(ratios) == 1) {
    ratios = c(ratios, 1-ratios)
    warning(sprintf("The ratios is set to c(%s)", paste(ratios, collapse = ', ')))
  }
  if (!is.numeric(ratios) || sum(ratios)>1 || any(sapply(ratios, function(x) x<=0))) {
    warning("Incorrect inputs; ratios must be a numeric vector that between 0 and 1, and sum of which should not larger than 1. It was set to default values.")
    ratios = c(0.7, 0.3)
  } else {
    ratio_ = 1-sum(ratios)
    if (ratio_ > 0) ratios = c(ratios, ratio_)
  }

  # oot, out of time
  dt_oot = NULL
  if (!is.null(oot$order) & (is.null(oot$start) + is.null(oot$ratio) == 1)) {
    setorderv(dt, oot$order)

    if (!is.null(oot$start)) {
      dt_oot = dt[get(oot$order) >= oot$start]
      dt = dt[get(oot$order) < oot$start]
    } else if (!is.null(oot$ratio)) {
      n_oot = dt[,floor(.N*oot$ratio)]

      dt_oot = tail(dt, n_oot)
      dt = head(dt, dt[,.N-n_oot])
    }
  }

  # name_dfs
  len_ratio = length(ratios)
  if (ratio_ > 0) len_ratio = length(ratios)-1

  if (length(name_dfs) > len_ratio) { name_dfs = name_dfs[seq_len(len_ratio)]
  } else if (length(name_dfs) < len_ratio) name_dfs = as.character(seq_len(len_ratio))

  # no_dfs
  no_dfs = kwargs[['no_dfs']]
  if (is.null(no_dfs) || no_dfs != length(name_dfs)) no_dfs = length(name_dfs)

  # set seed and partition
  set.seed(seed)
  if (is.null(y)) {
    dt[, ind := sample(length(ratios), .N, replace=TRUE, prob=ratios)]
  } else {
    dt[, ind := sample(length(ratios), .N, replace=TRUE, prob=ratios), by=y]
  }

  # random sort
  lst_dfs = split(dt, by = 'ind', sorted = TRUE, keep.by = FALSE)[seq_len(no_dfs)]
  names(lst_dfs) = name_dfs

  # oot
  if (!is.null(dt_oot)) lst_dfs$oot = dt_oot

  return(lst_dfs)
}


df_split = function(dt, y=NULL, ratios=c(0.7, 0.3), name_dfs=c('train', 'test'), oot=list(order=NULL, start=NULL, ratio=NULL), seed=618, ...) {
  UseMethod('split_df')
}
