# condition & helper functions # https://adv-r.hadley.nz/debugging

# ceiling on decimal
ceiling2 = function(x) {
  x_sci = format(x, scientific = TRUE, digits=2)
  z = ceiling(as.numeric(substr(x_sci, 1, 3)))
  e = substr(x_sci, 4, nchar(x_sci))
  as.numeric(paste0(z, e))
}

# is date/time class
isdatetime = function(x) {
  any(class(x) %in% c("Date","POSIXlt","POSIXct","POSIXt"))
}


# remove constant columns
check_const_cols = function(dt) {
  setDT(dt)
  # constant columns
  const_cols = names(which(dt[,sapply(.SD, function(x) length(unique(x))==1)]))
  if (length(const_cols) > 0) {
    warning( sprintf('There are %s constant columns are removed from input dataset,\n%s', length(const_cols), paste0(const_cols, collapse=', ')) )
    dt = dt[, (const_cols) := NULL]
  }
  return(dt)
}
# remove date time columns
check_datetime_cols = function(dt) {
  setDT(dt)
  # datatime columns
  datetime_cols = names(which(dt[,sapply(.SD, isdatetime)]))
  if (length(datetime_cols) > 0) {
    warning( sprintf('There are %s date/time columns are removed from input dataset,\n%s', length(datetime_cols), paste0(datetime_cols, collapse=', ')) )
    dt = dt[,(datetime_cols) := NULL]
  }
  return(dt)
}
# check categorical columns' unique values
check_cateCols_uniqueValues = function(dt, var_skip = NULL) {
  setDT(dt)
  # categorical columns
  cate_cols = names(which(dt[, sapply(.SD, function(x) is.character(x) | is.factor(x))]))
  cate_cols = setdiff(cate_cols, var_skip)

  if (length(cate_cols) > 0) {
    # have more than 50 unique values
    cateCols_uniVal50 = names(which(dt[, sapply(.SD, function(x) length(unique(x)) > 50), .SDcols = cate_cols]))
    # double check
    if (length(cateCols_uniVal50) > 0) {
      if (menu(c('yes','no'), title = sprintf('There are %s categorical columns have more than 50 unique values, which might cause the binning process slow. Please double check the following columns:\n%s \n\nContinue the binning process?', length(cateCols_uniVal50), paste0(cateCols_uniVal50, collapse = ", "))) == 2 ) stop()
    }
  }
}

# replace blank by NA
rep_blank_na = function(dt) {
  dt = setDT(dt)

  if (any(dt == "", na.rm = TRUE)) {
    warning(sprintf('The blank values are replaced with NAs in the following columns:\n%s', paste(names(which(dt[,sapply(.SD, function(x) any(grepl('^\\s*$', x), na.rm = T))])), collapse = ", ")))

    dt[dt == ""] = NA
  }

  is_inf_nan = sapply(dt, function(x) any(is.infinite(x)) | any(is.nan(x)))
  if (any(is_inf_nan)) {
    warning(sprintf('The Infinite or NaN values are replaced with -999 in the following columns:\n%s', paste(names(which(
      is_inf_nan
    )), collapse = ", ")))

    dt[dt == Inf | dt == -Inf] = -999
    dt = dt[, lapply(.SD, function(x) {
      x[is.nan(x)] = -999
      return(x)
    })]
  }

  return(dt)
}

# check y
check_y = function(dt, y, positive="bad|1") {
  dt = setDT(dt)
  positive = as.character(positive)
  # dt[[y]]  = as.character(dt[[y]])

  # number of columns >= 2
  if (ncol(dt) <=1 & !is.null(ncol(dt))) stop("Incorrect inputs; dt should have at least two columns.")
  # length of y == 1
  if (length(y) != 1) stop("Incorrect inputs; the length of \"y\" != 1.")
  # exist of y column
  if (!(y %in% names(dt))) stop(paste0("Incorrect inputs; there is no \"", y, "\" column in dt."))

  # remove rows have missing values in y
  if (anyNA(dt[[y]])) {
    warning(sprintf("There are NAs in %s. The rows with NAs in \"%s\" are removed from input data.", y, y))
    dt = dt[!is.na(dt[[y]])]
  }

  # numeric to integer
  if (class(dt[[y]]) == "numeric") dt[, (y) := lapply(.SD, as.integer), .SDcols = y]
  # factor to character
  if (class(dt[[y]]) == "factor") dt[, (y) := lapply(.SD, as.character), .SDcols = y]

  # length of unique values in y
  if (length(unique(dt[[y]])) == 2) {
    if (any(grepl(positive, dt[[y]]))) {
      y1 = dt[[y]]
      y2 = ifelse(grepl(positive, y1), 1L, 0L)
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
x_variable = function(dt, y, x, var_skip=NULL) {
  x_all = setdiff(names(dt), c(y, var_skip))

  if (is.null(x)) x = x_all

  if ( length(setdiff(x,x_all)) > 0 ) {
    warning(sprintf('Incorrect inputs; there are %s variables are not exist in the input data frame, which are removed from x. \n%s', length(setdiff(x, x_all)), paste(setdiff(x, x_all), collapse = ', ')) )
    x = intersect(x, x_all)
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
          warning(paste0("Incorrect inputs; the variables \n", paste0(names_bl_x, collapse = ","), "\n specified in breaks_list donot exist in x."))
        }

        names_x_bl = setdiff(xs, xs_breakslist)
        if (length(names_x_bl) >0) {
          warning("There are ",length(names_x_bl)," x variables that donot specified in breaks_list are using optimal binning.")
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
          warning(paste0("Incorrect inputs; the variables \n", paste0(names_bl_x, collapse = ","), "\n specified in special_values donot exist in x."))
        }

        names_x_bl = setdiff(xs, xs_sv)
        if (length(names_x_bl) >0) {
          warning("There are ",length(names_x_bl)," x variables that donot specified in special_values donot have special values.")
        }
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
           as.list(rep(0.1), length(xs2)) )
    sl = setNames(sl, c(names(stop_limit), xs2))
  }

  sl = lapply(sl, function(s) {
    if (!((s >0 & s <=0.5) || s == 'N')) s = 0.1
    return(s)
  })
  return(sl)
}

# y to good bad
# groupby or dcast
# groupby is faster via data.table package
# y_to_goodbad = function(dt, y) {
#   # dt = data.table(x = rnorm(1e+8), y = sample(c(rep(0,9),1), 1e+8, replace = TRUE))
#   #
#   # system.time(
#   #   dcast(dt, x~y, fun=length, value.var = 'y')
#   # )
#   #
#   # system.time(
#   #   dt[, .(good=sum(y==0), bad=sum(y==1)), by=x]
#   # )
#
#
#
# }


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



brk_txt2vector = function(brk) {
  if (is.null(brk) || is.na(brk)) return(brk)

  v = strsplit(brk, "(?<!\\%),(?!\\%)", perl = T)[[1]]
  v = trimws(v)
  v = sub('[\'\"](.+)[\'\"]', '\\1', v)

  # dtbrk = data.table(
  #   brk = brk
  # )[, strsplit(brk, '%,%')
  # ][, id := .I
  # ][, strsplit(V1, ','), by =id
  # ][, V1 := trimws(V1)
  # ][, id2 := .I
  # ][, id3 := id - shift(id,type='lag')]
  # # dtbrk[1, id3 := dtbrk[2, id3]]
  #
  # rowid = which(dtbrk[, (id - shift(id,type = 'lag')) == 1])
  #
  # v = rbind(
  #   data.table(i = rowid, v = paste(dtbrk[rowid-1,V1], dtbrk[rowid,V1], sep = '%,%')),
  #   dtbrk[-c(rowid, rowid-1), .(i = id2, v=V1)]
  # )[order(i)][,sub('[\'\"](.+)[\'\"]', '\\1', v)]

  return(v)
}
