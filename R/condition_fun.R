# conditions # https://adv-r.hadley.nz/debugging

# replace blank by NA
#' @import data.table
#'
rep_blank_na <- function(dt) {
  dt <- setDT(dt)

  if ( any(dt == '', na.rm=TRUE) ) {
    warning("Incorrect inputs; there is a blank character (\"\") in the columns of ", paste0(names(dt)[dt[,sapply(.SD, function(x) "" %in% x)]], collapse = ",") ,". It was replaced by NA.")
    dt[dt == ""] <- NA
  }

  return(dt)
}

# check y
#' @import data.table
#'
check_y <- function(dt, y, positive){
  dt <- setDT(dt)

  # ncol of dt
  if (ncol(dt) <=1 & !is.null(ncol(dt))) stop("Incorrect inputs; dt should have at least two columns.")

  # y ------
  if (!(y %in% names(dt))) stop(paste0("Incorrect inputs; there is no \"", y, "\" column in dt."))

  # length of y == 1
  if (length(y) != 1) stop("Incorrect inputs; the length of y is not equal 1.")

  # remove na in y
  if ( anyNA(dt[[y]]) ) {
    warning(paste0("Incorrect inputs; there are NAs in ", y, ". The rows with NA in ", y, " were removed from input dataset."))
    y_sel <- !is.na(dt[[y]]); dt <- dt[y_sel]
  }

 # length of unique values in y
  if (length(unique(dt[[y]])) == 2) {
    if (!(1 %in% unique(dt[[y]]) & 0 %in% unique(dt[[y]]))) {
      warning(paste0("Incorrect inputs; ", y, " should take only two values, 0 and 1. The positive value was replaced by 1 and negative value by 0."))
      if (any(grepl(positive, dt[[y]])==TRUE)) {
        dt[[y]] <- ifelse(grepl(positive, dt[[y]]), 1, 0)
      } else {
        stop(paste0("Incorrect inputs; the positive value in ", y, " is not specified"))
      }
    }
  } else {
    stop(paste0("Incorrect inputs; the length of unique values in ",y , " != 2."))
  }

  return(dt)
}

# check print_step
#' @import data.table
#'
check_print_step <- function(print_step) {
  if (!is.numeric(print_step) || print_step<0) {
    warning("Incorrect inputs; print_step should be a non-negative integer. It was set to 1L.")
    print_step <- 1L
  }

  return(print_step)
}

# x variable
x_variable <- function(dt, y, x) {
  x_all <- setdiff(names(dt), y)

  if (is.null(x)) {
    x <- x_all
  }

  if ( length(setdiff(x,x_all)) > 0 ) {
    warning(paste0("Incorrect inputs; there is no \"", paste0(setdiff(x,x_all),collapse = ","), "\" column in dt. It was removed from x variables vector."))
    x <- intersect(x, x_all)
  }

  return(x)
}
