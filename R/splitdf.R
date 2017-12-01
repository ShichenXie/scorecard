#' Split a dataset
#'
#' @param dt A data frame.
#' @param y Name of y variable, defaults NULL. The dataset dt will split based on the predictor y, if it is specified.
#' @param ratio A numeric value, defaults 0.7. It indicates the ratio of total rows contained in one split, must less than 1.
#' @param seed A random seed, defaults 186. The specify seed is used for random sorting data.
#'
#' @examples
#' library(scorecard)
#' data(germancredit)
#'
#' dts = split_df(germancredit, y="creditability")
#' train = dts$train
#' test = dts$test
#'
#' @import data.table
#' @export
split_df = function(dt, y=NULL, ratio=0.7, seed=186) {
  rt = rn_train = rn_test = NULL

  # set dt as data.table
  dt = setDT(dt)
  # remove date/time col
  dt = rm_datetime_col(dt)
  # replace "" by NA
  dt = rep_blank_na(dt)

  # set ratio range
  if (!is.numeric(ratio) || length(ratio) != 1 || sum(ratio)>=1) {
    warning("Incorrect inputs; ratio must be a numeric that length equal to 1 and less than 1. It was set to 0.7.")
    ratio = 0.7
  }
  # set seed
  set.seed(seed)

  rt = list(train=NULL, test=NULL)
  if (is.null(y)) {
    rn_sel = sample(nrow(dt), round(nrow(dt)*ratio))

    rn_train = rn_sel
    rn_test = setdiff(1:nrow(dt), rn_sel)
  } else {
    # dt$y = dt[[y]]; dt[[y]] = NULL
    y_unique = table(dt[[y]])

    for (i in names(y_unique)) {
      # dti = dt[.(i)]
      rn_dti = which(dt[[y]]==i)
      rn_sel = sample(rn_dti, round(length(rn_dti)*ratio))

      rn_train = c(rn_train, rn_sel)
      rn_test  = c(rn_test, setdiff(rn_dti, rn_sel))
    }
  }

  # random sort
  rt$train = dt[sample(rn_train)]
  rt$test  = dt[sample(rn_test)]

  return(rt)
}
