# conditions # https://adv-r.hadley.nz/debugging

# remove date time
rm_datetime_col = function(dt) {
  dt = setDT(dt)

  isdatetime = function(x) (class(x)[1] %in% c("date","POSIXlt","POSIXct","POSIXt")) == TRUE
  datetime_col = names(which(dt[,sapply(.SD, isdatetime)]))

  if (length(datetime_col) > 0) {
    warning(paste0("The date/times columns (",paste0(datetime_col,collapse = ","),") are removed from input dataset."))

    dt = dt[,(datetime_col) := NULL]
  }

  return(dt)
}

# replace blank by NA
#' @import data.table
#'
rep_blank_na = function(dt) {
  dt = setDT(dt)

  if (any(dt == "", na.rm = TRUE)) {
    warning("There are blank characters in the columns of \"", paste0(names(which(dt[,sapply(.SD, function(x) any(x=="",na.rm = T))])), collapse = ",") ,"\", which were replaced by NAs.")

    dt[dt == ""] = NA
  }

  return(dt)
}

# check y
#' @import data.table
#'
check_y = function(dt, y, positive){
  dt = setDT(dt)

  # ncol of dt
  if (ncol(dt) <=1 & !is.null(ncol(dt))) stop("Incorrect inputs; dt should have at least two columns.")

  # y ------
  if (!(y %in% names(dt))) stop(paste0("Incorrect inputs; there is no \"", y, "\" column in dt."))

  # length of y == 1
  if (length(y) != 1) stop("Incorrect inputs; the length of \"",y,"\" != 1.")

  # remove na in y
  if ( anyNA(dt[[y]]) ) {
    warning(paste0("There are NAs in ", y, ". The rows with NA in \"", y, "\" were removed from input data."))
    y_sel = !is.na(dt[[y]]); dt = dt[y_sel]
  }

 # length of unique values in y
  if (length(unique(dt[[y]])) == 2) {
    if ( any(c(0,1) %in% unique(dt[[y]]) == FALSE) ) {

      if (any(grepl(positive, dt[[y]])==TRUE)) {
        warning(paste0("The positive value in \"", y,"\" was replaced by 1 and negative value by 0."))
        dt[[y]] = ifelse(grepl(positive, dt[[y]]), 1, 0)
      } else {
        stop(paste0("Incorrect inputs; the positive value in \"", y, "\" is not specified"))
      }

    }
  } else {
    stop(paste0("Incorrect inputs; the length of unique values in \"",y , "\" != 2."))
  }

  return(dt)
}

# check print_step
#' @import data.table
#'
check_print_step = function(print_step) {
  if (!is.numeric(print_step) || print_step<0) {
    warning("Incorrect inputs; print_step should be a non-negative integer. It was set to 1L.")
    print_step = 1L
  }

  return(print_step)
}

# x variable
x_variable = function(dt, y, x) {
  x_all = setdiff(names(dt), y)

  if (is.null(x)) {
    x = x_all
  }

  if ( length(setdiff(x,x_all)) > 0 ) {
    warning(paste0("Incorrect inputs; the variables \n\"", paste0(setdiff(x,x_all),collapse = ","), "\"\n are not exist in input data, which are removed."))
    x = intersect(x, x_all)
  }

  return(x)
}
