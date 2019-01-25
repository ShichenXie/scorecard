#' One Hot Encoding
#'
#' One-hot encoding on categorical variables and replace missing values.  It is not needed when creating a standard scorecard model, but required in models that without doing woe transformation.
#'
#' @param dt A data frame.
#' @param var_skip Name of categorical variables that will skip for one-hot encoding. Default is NULL.
#' @param var_encode Name of categorical variables to be one-hot encoded, default is NULL. If it is NULL, then all categorical variables except in var_skip are counted.
#' @param nacol_rm Logical. One-hot encoding on categorical variable contains missing values, whether to remove the column generated to indicate the presence of NAs. Default is FALSE.
#' @param replace_na Replace missing values with a specified value such as -1, or the mean/median value for numeric variable and mode value for categorical variable. Default is NULL, which means no missing values will be replaced.
#'
#' @return A data frame
#'
#' @examples
#' # load germancredit data
#' data(germancredit)
#'
#' library(data.table)
#' dat = rbind(
#'   germancredit[, c(sample(20,3),21)],
#'   data.table(creditability=sample(c("good","bad"),10,replace=TRUE)),
#'   fill=TRUE)
#'
#' # one hot encoding
#' ## keep na columns from categorical variable
#' dat_onehot1 = one_hot(dat, var_skip = 'creditability', nacol_rm = FALSE) # default
#' str(dat_onehot1)
#' ## remove na columns from categorical variable
#' dat_onehot2 = one_hot(dat, var_skip = 'creditability', nacol_rm = TRUE)
#' str(dat_onehot2)
#'
#' ## one hot and replace NAs
#' dat_onehot3 = one_hot(dat, var_skip = 'creditability', replace_na = -1)
#' str(dat_onehot3)
#'
#'
#' # replace missing values only
#' ## replace with -1
#' dat_repna1 = one_hot(dat, var_skip = names(dat), replace_na = -1)
#' ## replace with median for numeric, and mode for categorical
#' dat_repna2 = one_hot(dat, var_skip = names(dat), replace_na = 'median')
#' ## replace with to mean for numeric, and mode for categorical
#' dat_repna3 = one_hot(dat, var_skip = names(dat), replace_na = 'mean')
#'
#'
#' @export
one_hot = function(dt, var_skip = NULL, var_encode = NULL, nacol_rm = FALSE, replace_na = NULL) {
  value = variable = NULL

  dt = setDT(copy(dt)) # copy(setDT(dt))

  # # factor columns to integer
  # if (factor_to_integer) {
  #   cols_factor = names(which(sapply(dt, is.factor)))
  #   if (!is.null(var_skip)) cols_factor = setdiff(cols_factor, var_skip)
  #   dt[, (cols_factor) := lapply(.SD, as.integer), .SDcols = cols_factor]
  # }

  # columns encoding
  if ( is.null(var_encode)) {
    var_encode = names(which(sapply(dt, function(x) !is.numeric(x) & !isdatetime(x) )))
  } else {
    var_encode = x_variable(dt, y=var_skip, x=var_encode)
  }
  # columns skip
  if (!is.null(var_skip)) var_encode = setdiff(var_encode, var_skip)


  # one hot encoding
  if (is.null(var_encode) || length(var_encode)==0) {
    # if there is no categorical column
    dt_new = dt
  } else {

    temp_dt = dt[, var_encode, with = FALSE][, rowid := .I]
    melted_dt = melt(temp_dt, id='rowid', na.rm = FALSE)[, value := paste(variable, value, sep='_')]
    dcast_dt  = dcast(melted_dt, rowid ~ value, fun.aggregate = length)
    # setnames( dcast_dt, gsub('[^[:alnum:]]', '_', names(dcast_dt)) )

    # remove nacols
    if (nacol_rm) {
      nacols = names(dcast_dt)[grepl('_NA', names(dcast_dt))]
      nacols = setdiff(nacols, names(dt))
      dcast_dt[, (nacols) := NULL]
    }
    # merge data frames
    dt_new = cbind(dt, dcast_dt)[, (c(var_encode, 'rowid')) := NULL]
  }

  # replace missing values with fillna
  if (!is.null(replace_na)) {
    names_fillna = names(dt_new)
    # if (!is.null(var_skip)) names_fillna = setdiff(names_fillna, var_skip)
    dt_new = dt_new[, (names_fillna) := lapply(.SD, function(x) {
      if (anyNA(x)) {
        # class of x is numeric
        xisnum = all(class(x) %in% c('numeric', 'integer'))

        # fillna values
        if (is.numeric(replace_na)) {
          fillna = replace_na
        } else if ( replace_na %in% c('mean', 'median')) {
          if (xisnum) {
            fillna = do.call(replace_na, list(x, na.rm=TRUE))
          } else {
            fillna = names(which.max(table(x)))
          }
        } else {
          fillna = -1
        }
        # set fill as character if x is not numeric
        if (!xisnum) {
          fillna = as.character(fillna)
        }

        # replace missing values in x
        if (is.factor(x)) {
          # https://stackoverflow.com/questions/39126537/replace-na-in-a-factor-column
          x = `levels<-`(addNA(x), c(levels(x), fillna))
        } else {
          x[is.na(x)] <- fillna
        }
      }
      return(x)
    }), .SDcols = names_fillna]
  }

  return(dt_new[])
}
