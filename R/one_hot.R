#' One Hot Encoding
#'
#' One-hot encoding on categorical variables and replace missing values.  It is not needed when creating a standard scorecard model, but required in models that without doing woe transformation.
#'
#' @param dt A data frame.
#' @param var_skip Name of categorical variables that will skip for one-hot encoding. Defaults to NULL.
#' @param var_encode Name of categorical variables to be one-hot encoded, Defaults to NULL. If it is NULL, then all categorical variables except in var_skip are counted.
#' @param nacol_rm Logical. One-hot encoding on categorical variable contains missing values, whether to remove the column generated to indicate the presence of NAs. Defaults to FALSE.
#' @param ... Additional parameters.
#'
#' @return A data frame
#'
#' @examples
#' # load germancredit data
#' data(germancredit)
#'
#' library(data.table)
#' dat = rbind(
#'   setDT(germancredit)[, c(sample(20,3),21)],
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
#'
#' @export
one_hot = function(dt, var_skip = NULL, var_encode = NULL, nacol_rm = FALSE, ...) {
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
    var_encode = names(which(sapply(dt, function(x) !is.numeric(x) & !inherits(x, c("Date","POSIXlt","POSIXct","POSIXt")) )))
    # xefun:::is.datetime(x)
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
  if (!is.null(list(...)[['replace_na']])) dat_new = replace_na(dat_new, list(...)[['replace_na']])

  return(dt_new[])
}



#' @export
replace_na.default = function(dt, repl) {
  # fillna
  fillna = repl
  #
  if (repl %in% c('mean', 'median')) {
    if (inherits(dt, c('integer', 'numeric', 'logical'))) {
      fillna = do.call(repl, list(dt, na.rm=TRUE))
    } else {
      fillna = names(which.max(table(dt)))
    }
  }
  # set fill as character if dt is not numeric
  if (inherits(dt, c('character', 'factor'))) {
    fillna = as.character(fillna)
  } else {
    fillna = as.numeric(fillna)
  }


  # replace missing values
  if (is.factor(dt)) {
    # https://stackoverflow.com/questions/39126537/replace-na-in-a-factor-column
    dt = `levels<-`(addNA(dt), c(levels(dt), fillna))
  } else {
    dt[is.na(dt)] <- fillna
  }

  return(dt)
}

#' @export
replace_na.data.frame = function(dt, repl) {
  dt = setDT(copy(dt))

  cols_na = names(dt)[sapply(dt, anyNA)]
  if (length(cols_na) > 0) {
    dt = dt[, (cols_na) := lapply(.SD, function(x) {
      replace_na.default(x, repl)
    }), .SDcols = cols_na]
  }

  return(dt)
}
#' Replace Missing Values
#'
#' Replace missing values with a specified value or mean/median value.
#'
#' @param dt A data frame or vector.
#' @param repl Replace missing values with a specified value such as -1, or the mean/median value for numeric variable and mode value for categorical variable if repl is mean or median.
#'
#' @examples
#' # load germancredit data
#' data(germancredit)
#'
#' library(data.table)
#' dat = rbind(
#'   setDT(germancredit)[, c(sample(20,3),21)],
#'   data.table(creditability=sample(c("good","bad"),10,replace=TRUE)),
#'   fill=TRUE)
#'
#' ## replace with -1
#' dat_repna1 = replace_na(dat, repl = -1)
#' ## replace with median for numeric, and mode for categorical
#' dat_repna2 = replace_na(dat, repl = 'median')
#' ## replace with mean for numeric, and mode for categorical
#' dat_repna3 = replace_na(dat, repl = 'mean')
#'
#' @export
replace_na = function(dt, repl) {
  UseMethod('replace_na')
}


#' @export
var_scale.default = function(dt, var_skip=NULL, type='standard', ...) {
  kwargs = list(...)

  if (type == 'standard') {
    center = TRUE
    if ('center' %in% names(kwargs)) center = kwargs[['center']]
    scale = TRUE
    if ('scale' %in% names(kwargs)) scale = kwargs[['scale']]

    dt_scale = as.vector(do.call('scale', list(x=dt, center=center, scale=scale)))
  } else if (type == 'minmax') {
    dt_scale = (dt - min(dt, na.rm=TRUE)) / diff(range(dt, na.rm=TRUE))
    new_rng = kwargs[['new_range']]
    if (!is.null(new_rng)) dt_scale = dt_scale*diff(new_rng) + min(new_rng)
  } else dt_scale = dt
  return(dt_scale)
}
#' @export
var_scale.data.frame = function(dt, var_skip=NULL, type='standard', ...) {
  dt = setDT(copy(dt))
  cols_num = names(dt)[sapply(dt, is.numeric)]
  cols_num = setdiff(cols_num, var_skip)

  if (length(cols_num) > 0) {
    dt = dt[, (cols_num) := lapply(.SD, function(x) {
      do.call( 'var_scale.default', c(list(dt=x, type=type), list(...)) )
    }), .SDcols = cols_num]
  }
  return(dt)
}

#' Variable Scaling
#'
#' scaling variables using standardization or normalization
#'
#' @param dt a data frame or vector
#' @param var_skip Name of variables that will skip for scaling Defaults to NULL.
#' @param type type of scaling method, including standard or minmax.
#' @param ... Additional parameters.
#'
#' @examples
#' data("germancredit")
#'
#' # standardization
#' dts1 = var_scale(germancredit, type = 'standard')
#'
#' # normalization/minmax
#' dts2 = var_scale(germancredit, type = 'minmax')
#' dts2 = var_scale(germancredit, type = 'minmax', new_range = c(-1, 1))
#'
#' @export
var_scale = function(dt, var_skip=NULL, type='standard', ...) {
  type = match.arg(type, c('standard', 'minmax'))
  UseMethod('var_scale')
}


# clusterSim::data.Normalization
# n1 - standardization ((x-mean)/sd)
# n2 - positional standardization ((x-median)/mad)
# n3 - unitization ((x-mean)/range)
# n3a - positional unitization ((x-median)/range)
# n4 - unitization with zero minimum ((x-min)/range)
# n5 - normalization in range <-1,1> ((x-mean)/max(abs(x-mean)))
# n5a - positional normalization in range <-1,1> ((x-median)/max(abs(x-median)))
# n6 - quotient transformation (x/sd)
# n6a - positional quotient transformation (x/mad)
# n7 - quotient transformation (x/range)
# n8 - quotient transformation (x/max)
# n9 - quotient transformation (x/mean)
# n9a - positional quotient transformation (x/median)
# n10 - quotient transformation (x/sum)
# n11 - quotient transformation (x/sqrt(SSQ))
# n12 - normalization ((x-mean)/sqrt(sum((x-mean)^2)))
# n12a - positional normalization ((x-median)/sqrt(sum((x-median)^2)))
# n13 - normalization with zero being the central point ((x-midrange)/(range/2))

# box-cox transformation
#
