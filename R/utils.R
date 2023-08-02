# condition & helper functions # https://adv-r.hadley.nz/debugging

# remove constant columns
#' @import xefun
check_const_cols = function(dt) {
  setDT(dt)
  # constant columns
  const_cols = cols_const(dt)
  if (length(const_cols) > 0) {
    warning( sprintf('There were %s constant columns removed from input dataset,\n%s', length(const_cols), paste0(const_cols, collapse=', ')) )
    dt = dt[, (const_cols) := NULL]
  }
  return(dt)
}

# remove date time columns
#' @import xefun
check_datetime_cols = function(dt) {
  setDT(dt)
  # datatime columns
  datetime_cols = cols_type(dt, 'datetime')
  if (length(datetime_cols) > 0) {
    warning( sprintf('There were %s date/time columns removed from input dataset,\n%s', length(datetime_cols), paste0(datetime_cols, collapse=', ')) )
    dt = dt[,(datetime_cols) := NULL]
  }
  return(dt)
}

# check categorical columns' unique values
check_cateCols_uniqueValues = function(dt, var_skip = NULL) {
  setDT(dt)
  # categorical columns
  cate_cols = cols_type(dt, c('character', 'factor'))
  cate_cols = setdiff(cate_cols, var_skip)

  if (length(cate_cols) > 0) {
    # have more than 50 unique values
    cateCols_uniVal50 = names(which(dt[, sapply(.SD, function(x) length(unique(x)) > 50), .SDcols = cate_cols]))
    # double check
    if (length(cateCols_uniVal50) > 0) {
      if (menu(c('yes','no'), title = sprintf('There are %s categorical columns that have more than 50 unique values, which might cause the binning process to be slow. Please double check the following columns:\n%s \n\nContinue the binning process?', length(cateCols_uniVal50), paste0(cateCols_uniVal50, collapse = ", "))) == 2 ) stop()
    }
  }
}

# replace blank by NA
#' @importFrom stringi stri_isempty stri_trim_left
rep_blank_na = function(dt) {
  dt = setDT(dt)

  # replace black values
  char_cols = cols_type(dt, c('character', 'factor'))
  if (length(char_cols)>0) {
    # columns have blank value
    blank_cols = names(which(dt[, sapply(.SD, function(x) any(stri_isempty(stri_trim_left(x)))), .SD = char_cols])) # grep('^\\s*$', x)
    # repalce by NA
    if (length(blank_cols)>0) {
      warning(sprintf('The blank values are replaced with NAs in the following columns:\n%s', paste(blank_cols, collapse = ", ")))

      dt[, (blank_cols) := lapply(.SD, function(x) {
        x[stri_isempty(stri_trim_left(x))] = NA # grep('^\\s*$', x)
        return(x)
      }), .SD = blank_cols]#[dt == ""] = NA
    }
  }


  # repalce infinite or NaN values
  cols_num = setdiff(names(dt), char_cols)
  if (length(cols_num) >0) {
    cols_inf_nan = names(which(dt[
      , sapply(.SD, function(x) any(is.infinite(x) | is.nan(x)))
      , .SD = cols_num
      ]))
    if (length(cols_inf_nan) > 0) {
      warning(sprintf('The Infinite or NaN values are replaced with -999 in the following columns:\n%s', paste(cols_inf_nan, collapse = ", ")))

      dt[, (cols_inf_nan) := lapply(.SD, function(x) {
        x[is.nan(x)] = NA
        x[is.infinite(x)] = -999
        return(x)
      }), .SD = cols_inf_nan]
    }
  }


  return(dt)
}

# check y
check_y = function(dt, y, positive="bad|1") {
  dt = setDT(dt)
  positive = unlist(strsplit(as.character(positive), '\\|'))

  # number of columns >= 2
  if (ncol(dt) <=1 & !is.null(ncol(dt))) stop("Incorrect inputs; dt should have at least two columns.")
  # length of y == 1
  if (length(y) != 1) stop("Incorrect inputs; the length of \"y\" != 1.")
  # exist of y column
  if (!(y %in% names(dt))) stop(paste0("Incorrect inputs; there is no \"", y, "\" column in dt."))

  # dt[[y]]  = as.character(dt[[y]])
  y1 = dt[[y]]
  y_class = class(y1)

  # remove rows have missing values in y
  if (anyNA(y1)) {
    warning(sprintf("There are NAs in %s. The rows with NAs in \"%s\" are removed from input data.", y, y))
    dt = dt[!is.na(y1)]
  }

  # numeric to integer # factor to character
  if (y_class == "numeric") {
    dt[, (y) := lapply(.SD, as.integer), .SDcols = y]
  } else if (y_class == "factor") {
    dt[, (y) := lapply(.SD, as.character), .SDcols = y]
  }

  # length of unique values in y
  uniqy = unique(y1)
  if (length(uniqy) == 2) {
    if (any(uniqy %in% positive)) {
      y2 = as.integer(y1 %in% positive)
      if (any(y1 != y2)) {
        dt[[y]] = y2
        # warning(paste0("The positive value in \"", y, "\" was replaced by 1 and negative value by 0."))
      }
    } else {
      stop(paste0("Incorrect inputs; the positive value in \"", y, "\" is not specified"))
    }
  } else {
    stop(paste0("Incorrect inputs; the length of unique values in \"", y, "\" != 2."))
  }

  return(dt)
}

# check print_step
check_print_step = function(print_step) {
  if (!is.numeric(print_step) || print_step<0) {
    warning("Incorrect inputs; print_step should be a non-negative integer. It was set to 1L.")
    print_step = 1L
  }

  return(print_step)
}

# x variable
x_variable = function(dt, y, x, var_skip=NULL, method = NULL) {
  x_all = setdiff(names(dt), c(y, var_skip))

  if (is.null(x)) x = x_all

  if ( length(setdiff(x,x_all)) > 0 ) {
    warning(sprintf('Incorrect inputs; there are %s variables that do not exist in the input data frame, which are removed from x. \n%s', length(setdiff(x, x_all)), paste(setdiff(x, x_all), collapse = ', ')) )
    x = intersect(x, x_all)
  }

  if (any(method %in% c('freq', 'width'))) {
    x_num = setdiff(names(dt)[sapply(dt, is.numeric)], c(y, var_skip))
    x = intersect(x, x_num)
  }

  return(x)
}


# check break_list
check_breaks_list = function(breaks_list, xs) {
  if (!is.null(breaks_list)) {
    if (is.character(breaks_list)) {
      breaks_list = eval(parse(text = breaks_list))
    }
    if (!inherits(breaks_list, 'list')) {
      stop("Incorrect inputs; breaks_list should be a list.")

    } else {
      # remove missing from breakpoints
      breaks_list = lapply(breaks_list, function(x) {
        x=setdiff(x, 'missing')
        if (length(x)==0) x = NULL
        return(x)
      })

      # check variable names
      xs_breakslist = names(breaks_list)
      if (!identical(xs_breakslist, xs)) {

        names_bl_x = setdiff(xs_breakslist, xs)
        if (length(names_bl_x) > 0) {
          warning(paste0("Incorrect inputs; the variables \n", paste0(names_bl_x, collapse = ","), "\n specified in breaks_list do not exist in x."))
        }

        names_x_bl = setdiff(xs, xs_breakslist)
        if (length(names_x_bl) >0) {
          warning("There are ",length(names_x_bl)," x variables that are not specified in breaks_list, and instead are using optimal binning.")
        }
      }
    }
  }
  return(breaks_list)
}


# check special_values
check_special_values = function(special_values, xs) {
  if (!is.null(special_values)) {
    if (is.vector(special_values) & !is.list(special_values)) {
      warning("The special_values should be a list. Make sure special values are exactly the same in all variables if special_values is a vector.")
      # transfer vector to list
      special_values_list = list()
      for (i in xs) {
        special_values_list[[i]] = special_values
      }
      special_values=special_values_list

    } else if (is.list(special_values)) {
      # x variables of special_values
      xs_sv = names(special_values)
      # if xs_sv != xs
      if (!identical(xs_sv, xs)) {

        names_bl_x = setdiff(xs_sv, xs)
        if (length(names_bl_x) > 0) {
          warning(paste0("Incorrect inputs; the variables \n", paste0(names_bl_x, collapse = ","), "\n specified in special_values do not exist in x."))
        }

        # names_x_bl = setdiff(xs, xs_sv)
        # if (length(names_x_bl) >0) {
        #   warning("There are ",length(names_x_bl)," x variables that are not specified in special_values.")
        # }
      }

    } else {
      stop("Incorrect inputs; special_values should be a vector or list.")
    }

  }
  return(special_values)
}


# second to hh:mm:ss
sec_to_hms = function(sec) {
  h = sec %/% 3600
  m = sec %% 3600 %/% 60
  s = floor(sec %% 3600 %% 60)

  return(sprintf("%02.f:%02.f:%02.f",h,m,s))
}


# check check_stop_limit
check_stop_limit = function(stop_limit, xs) {
  sl = list()

  if (!is.list(stop_limit)) {
    sl = as.list(rep(stop_limit, length(xs)))
    sl = setNames(sl, xs)
  } else {
    xs2 = setdiff(xs, names(stop_limit))

    sl = c(stop_limit,
           as.list(rep(0.1, length(xs2))) )
    sl = setNames(sl, c(names(stop_limit), xs2))
  }

  sl = lapply(sl, function(s) {
    if (!((s >0 & s <=0.5) || s == 'N')) s = 0.1
    return(s)
  })
  return(sl)
}


# check number of cpu cores
check_no_cores = function(no_cores, xs_len=10) {
  all_cores = detectCores(logical=F)-1

  if (is.null(no_cores)) {
    if (xs_len < 10) {
      no_cores = 1
    } else {
      no_cores = ceiling(all_cores*0.9)
    }
  } else if (!is.numeric(no_cores) || no_cores <1) {
    no_cores = ceiling(all_cores*0.9)
  } else if (no_cores >= all_cores+1) {
    no_cores = all_cores
  }

  return(no_cores)
}


# check woebin or scorecard object
check_bincard = function(x) {
  if (inherits(x, 'list') && all(sapply(x, is.data.frame))) x = rbindlist(x, fill = TRUE)
  setDT(x)
}


menu2 = function(choices, title, chk_rng = TRUE) {
  cat(title, '\n')
  for (l in seq_along(choices)) cat(sprintf('%s: %s', l, choices[l]), '\n')

  if (chk_rng) {
    sel = 'init'
    while (!(sel %in% c(seq_len(length(choices)), 'save') || grepl('^go[1-9][0-9]*$', sel))) {
      sel = readline("Selection (1-3, goX, save): ")
      if (grepl('^[1-9][0-9]*$', sel)) sel = as.integer(sel)
    }
  } else {
    sel = readline("Selection: ")
    if (grepl('^[1-9][0-9]*$', sel)) sel = as.integer(sel)
  }

  return(sel)
}



