#' Split a dataset
#'
#' @param dt A data frame.
#' @param y Name of y variable, defaults NULL. The dataset dt will split based on the predictor y, if it is specified.
#' @param ratio A numeric value, defults 0.7. It indicates the ratio of total rows contained in one split, must less than 1.
#' @param seed A random seed, defaults 186. The specify seed is used for random sorting data.
#' @param positive Value of positive class, default "bad|1".
#'
#' @examples
#' library(scorecard)
#' data(germancredit)
#'
#' dts <- split_df(germancredit, y="creditability")
#' train <- dts$train
#' test <- dts$test
#'
#' @import data.table
#' @export
split_df <- function(dt, y=NULL, ratio=0.7, seed=186, positive="bad|1") {
  rt = rn_train = rn_test = NULL

  # inputs check
  if (!is.data.frame(dt)) stop("Incorrect inputs; dt should be a dataframe.")
  if (!is.numeric(ratio) || length(ratio) != 1 || sum(ratio)>=1) {
    warning("Incorrect inputs; ratio must be a numeric that length equal to 1 and less than 1. It was set to 0.7.")
    ratio <- 0.7
  }

  # set dt as data.table
  dt <- setDT(dt)
  if (!is.null(y)) {
    # check y
    if ( anyNA(dt[[y]]) ) {
      warning(paste0("Incorrect inputs; there are NAs in the label variable ", y, ". The rows with NA in ", y, " were removed from input dataset."))
      y_sel <- !is.na(dt[[y]]); dt <- dt[y_sel]
    }
    if (length(unique(dt[[y]])) == 2) {
      if (!identical(unique(dt[[y]]), c(0,1))) {
        warning(paste0("Incorrect inputs; the label variable ", y, " should take only two values, 0 and 1. The positive value was replaced by 1 and negative value by 0."))
        if (any(grepl(positive, dt[[y]])==TRUE)) {
          dt[[y]] <- ifelse(grepl(positive, dt[[y]]), 1, 0)
        } else {
          stop(paste0("Incorrect inputs; the positive value in the label variable ", y, " is not specified"))
        }
      }
    } else {
      stop(paste0("Incorrect inputs; the length of unique values in label variable ",y , " != 2."))
    }
  }

  # replace "" by NA
  if ( any(dt == '') ) {
    warning("Incorrect inputs; there is a blank character (\"\") in the columns of ", paste0(names(dt)[dt[,sapply(.SD, function(x) "" %in% x)]], collapse = ",") ,". It was replaced by NA.")
    dt[dt == ""] <- NA
  }

  # set seed
  set.seed(seed)

  rt <- list(train=NULL, test=NULL)
  if (is.null(y)) {
    rn_sel <- sample(nrow(dt), round(nrow(dt)*ratio))

    rn_train <- rn_sel
    rn_test <- setdiff(1:nrow(dt), rn_sel)
  } else {
    # dt$y <- dt[[y]]; dt[[y]] <- NULL
    y_unique <- table(dt[[y]])

    for (i in as.integer(names(y_unique))) {
      # dti <- dt[.(i)]
      rn_dti <- which(dt[[y]]==i)
      rn_sel <- sample(rn_dti, round(length(rn_dti)*ratio))

      rn_train <- c(rn_train, rn_sel)
      rn_test  <- c(rn_test, setdiff(rn_dti, rn_sel))
    }
  }

  # random sort
  rt$train <- dt[sample(rn_train)]
  rt$test  <- dt[sample(rn_test)]

  return(rt)
}
