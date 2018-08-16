# conditions # https://adv-r.hadley.nz/debugging

# remove date time # rm_datetime_col
# remove columns if len(x.unique()) == 1
rmcol_datetime_unique1 = function(dt) {
  dt = setDT(dt)

  unique1_cols = names(which(dt[,sapply(.SD, function(x) length(unique(x))==1)]))
  if (length(unique1_cols > 0)) {
    warning(paste0("There are ", length(unique1_cols), " columns have only one unique values, which are removed from input dataset. \n (ColumnNames: ", paste0(unique1_cols, collapse=', '), ")" ))

    dt = copy(dt)[, (unique1_cols) := NULL]
  }


  isdatetime = function(x) (class(x)[1] %in% c("Date","POSIXlt","POSIXct","POSIXt")) == TRUE
  datetime_col = names(which(dt[,sapply(.SD, isdatetime)]))

  if (length(datetime_col) > 0) {
    warning(paste0("The date/times columns (",paste0(datetime_col,collapse = ","),") are removed from input dataset."))

    dt = copy(dt)[,(datetime_col) := NULL]
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

  if (is.null(x)) x = x_all

  if ( length(setdiff(x,x_all)) > 0 ) {
    warning(paste0("Incorrect inputs; the variables \n\"", paste0(setdiff(x,x_all), collapse = ","), "\"\n are not exist in input data, which are removed."))
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
    if (!is.list(breaks_list)) {
      stop("Incorrect inputs; breaks_list should be a list.")

    } else {
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

  return(sprintf("%02s:%02s:%02s",h,m,s))
}
















#1 生命周期利润模型(LTV, life time value) ------
# 客户生命周期利润 = 生命周期收入 - 资金成本 - 坏账成本 - 获客成本 - 人力成本 - 其他成本

## 生命周期收入
# 收费方式: 等额本金、等额本息、等本等息、先息后本
# 生命周期收入 = 笔均交易金额*单笔交易手续费率*生命周期复借笔数
# 单笔交易手续费率 = 每期交易金额*期数*每期手续费/总交易金额

## 资金成本
# 非年化资金成本率 = 年化资金成本率/12*(1 + (期数-1)/期数 + ... + 1/期数)
#                  = 年化资金成本率/12*(期数+1)/2
# 年化资金成本率 = 非年化资金成本率*24/(期数+1)

## 坏账成本
# 通过完整的vintage曲线的最后几个点作为坏账预估数

## 获客成本
# 成本/新增加客户数

## 人力成本
# 年人力总成本/当年新增交易客户



#2 年化利率模型 ------
# 利润 = 收入 - 资金成本 - 坏账成本 - 获客成本 - 人力成本 - 其他成本

## 年化利率 = 非年化收益率*24/(期数+1)
## 年化资金成本
## 年化坏账率 = vintage*24/(期数+1)

## 年化利润率 * (年初余额 + 年末余额)/2
