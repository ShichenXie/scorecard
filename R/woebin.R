# required in woebin2 # return binning if provide breaks
#' @import data.table
woebin2_breaks = function(dtm, breaks) {
  # global variables or functions
  value = bin = . = y = variable = bad = good = V1 = badprob = NULL

  dtm = setDT(dtm)
  if (is.numeric(dtm[,value])) {
    # numeric variable ------
    bstbrks = c(-Inf, setdiff(unique(breaks), c(NA, Inf, -Inf)), Inf)

    binning = dtm[
      , bin := cut(value, bstbrks, right = FALSE, dig.lab = 10, ordered_result = FALSE)
      ][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , keyby = bin
      ][order(bin)]

  } else if (is.factor(dtm[,value]) || is.character(dtm[,value])) {
    # other variable ------
    dtm = dtm[,value := as.character(value)]

    binning = merge(
      data.table(rowid=1:length(breaks), bin = breaks, value = breaks )[, strsplit(as.character(value), "%,%", fixed=TRUE), by = c("rowid","bin") ][, .(rowid, bin, value = V1)],
      dtm, all.y = TRUE
    )[, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , keyby = c("rowid","bin")]

  }

  return(binning)
}

# required in woebin2 # return initial binning
woebin2_init_bin = function(dtm, min_perc_total) {
  # global variables or functions
  value = bin = . = y = variable = bad = good = brkp = badprob = count = merge_tolead = brkp2 = NULL

  dtm = setDT(dtm)
  if (is.numeric(dtm[,value])) {
    # numeric variable ------
    xvalue = dtm[, value]

    # breaks vector & outlier
    iq = quantile(xvalue, na.rm = TRUE)
    iqr = IQR(xvalue, na.rm = TRUE)
    if (iqr == 0) {
      xvalue_rm_outlier = xvalue
    } else {
      xvalue_rm_outlier = xvalue[which(xvalue > iq[2]-3*iqr & xvalue <= iq[4]+3*iqr)]
    }

    # number of initial binning
    n = trunc(1/min_perc_total)
    len_uniq_x = length(setdiff(unique(xvalue_rm_outlier), c(NA,Inf,-Inf)))
    if (len_uniq_x < n) n = len_uniq_x

    # initial breaks
    if (len_uniq_x < 10) {
      brk = setdiff(unique(xvalue_rm_outlier), c(NA, Inf, -Inf))
    } else {
      brk = pretty(xvalue_rm_outlier, n)
    }
    brk = sort(brk)
    brk = unique(c(-Inf, brk[2:(length(brk)-1)], Inf))
    if (anyNA(xvalue)) brk = c(brk, NA)

    # initial binning datatable
    init_bin = dtm[
      , bin := cut(value, brk, right = FALSE, dig.lab = 10, ordered_result = FALSE)
    ][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , keyby = bin
    ][order(bin)
    ][, `:=`(brkp = as.numeric( sub("^\\[(.*),.+", "\\1", bin)), badprob = bad/(good+bad))
    ][, .(variable, bin, brkp, good, bad, badprob)]

  } else if ( is.logical(dtm[,value]) || is.factor(dtm[,value]) || is.character(dtm[,value]) ) {
    # other variable ------

    # initial binning datatable
    init_bin = dtm[
      , .(variable = unique(variable), good = sum(y==0), bad = sum(y==1)), by=value
      ][, badprob := bad/(good+bad)]

    # order by bin if is.factor, or by badprob if is.character
    if (is.logical(dtm[,value]) || is.factor(dtm[,value])) {
      init_bin = init_bin[
        order(value)
      ][, brkp := ifelse(is.na(value), NA, .I)
      ][, .(variable, bin=value, brkp, good, bad, badprob)]

    } else {

      init_bin = init_bin[
        order(badprob)
        # next 3 lines make NA located at the last rows
      ][, brkp := ifelse(is.na(value), NA, .I)
      ][order(brkp)
      ][, brkp := ifelse(is.na(value), NA, .I)
      ][, .(variable, bin=value, brkp, good, bad, badprob)]
    }

  }

  # remove brkp that good == 0 | bad == 0 ------
  while (init_bin[!is.na(brkp)][good==0 | bad==0,.N] > 0) {
    # brkp needs to be removed if good==0 or bad==0
    rm_brkp = init_bin[!is.na(brkp)][
      ,count := good+bad
    ][, merge_tolead := shift(count,type="lag", fill=nrow(dtm)+1) > shift(count,type="lead", fill=nrow(dtm)+1)
    ][good == 0 | bad == 0][count == min(count)]

    # set brkp to lead's or lag's
    if (rm_brkp[1,merge_tolead]) {
      init_bin = init_bin[
        ,brkp2 := shift(brkp,type="lead")
      ][brkp == rm_brkp[1,brkp], brkp := brkp2]

    } else {

      init_bin = init_bin[
        ,brkp2 := shift(brkp,type="lag")
      ][brkp == rm_brkp[1,brkp], brkp := brkp2]
    }

    # groupby brkp
    init_bin = init_bin[
      ,.(variable=unique(variable), bin=paste0(bin, collapse = "%,%"), brkp=unique(brkp), good=sum(good), bad=sum(bad)), by=brkp
    ][, badprob:=bad/(good+bad)]
  }

  if (is.numeric(dtm[,value])) {
    init_bin = init_bin[grepl("%,%",bin), bin := sub("^(\\[.+?,).+,(.+?\\))$", "\\1\\2", bin)]
  }

  return(init_bin)
}

# required in woebin2_tree # add 1 best break for tree-like binning
woebin2_tree_1bst = function(dtm, initial_binning, min_perc_total, bestbreaks=NULL) {
  # global variables or functions
  brkp = patterns = . = good = bad = variable = count_distr = value = min_count_distr = bstbin = min_count_distr = total_iv = bstbin = brkp = bin = NULL

  # total_iv for all best breaks
  total_iv_all_breaks = function(initial_binning, bestbreaks) {
    # best breaks set
    breaks_set = setdiff( initial_binning[,brkp], c(bestbreaks, -Inf, Inf, NA) )

    init_bin_all_breaks = copy(initial_binning)
    # loop on breaks_set
    for (i in breaks_set) {
      # best break + i
      bestbreaks_i = sort(c(bestbreaks, i))

      # best break datatable
      init_bin_all_breaks = init_bin_all_breaks[
        , paste0("bstbin",i) := cut(brkp, c(-Inf, bestbreaks_i, Inf), right = FALSE, dig.lab = 10, ordered_result = FALSE) ]
    }

    # best break dt
    total_iv_all_brks = melt(
      init_bin_all_breaks, id = c("variable", "good", "bad"), variable.name = "bstbin", measure = patterns("bstbin.+")
    )[, .(good = sum(good), bad = sum(bad), variable = unique(variable))
      , keyby=c("bstbin", "value")
    ][, count_distr := (good+bad)/sum(good+bad), by="bstbin"
    ][!is.na(value), min_count_distr := min(count_distr), by="bstbin"
    ][, .(total_iv = iv_01(good, bad), variable = unique(variable), min_count_distr = min(min_count_distr,na.rm=TRUE)), keyby="bstbin"
    ][, bstbin := as.numeric(sub("bstbin(.+)", "\\1", bstbin))][]

    return(total_iv_all_brks)
  }
  total_iv_all_brks = total_iv_all_breaks(initial_binning, bestbreaks)

  # bestbreaks: total_iv==max(total_iv) & min(count_distr)>=0.2
  bstbrk_max_iv = total_iv_all_brks[min_count_distr >= min_perc_total][total_iv==max(total_iv), bstbin]
  # add 1best break to bestbreaks
  bestbreaks = unique(c(bestbreaks, bstbrk_max_iv[1]))

  # binning add 1best break
  binning_add_1bst = function(initial_binning, bestbreaks) {
    value = bstbin = . = good = bad = variable = woe = bin_iv = total_iv = bstbrkp = badprob = NULL # no visible binding for global variable

    if ( is.numeric(dtm[,value]) ) {
      binning_1bst_brk = initial_binning[
        , bstbin := cut(brkp, c(-Inf, bestbreaks, Inf), right = FALSE, dig.lab = 10, ordered_result = FALSE)
        ][, .(variable=unique(variable), bin=unique(bstbin), good = sum(good), bad = sum(bad)) , keyby = bstbin
          ]

    } else if (is.logical(dtm[,value]) || is.factor(dtm[,value]) || is.character(dtm[,value]) ) {

      bestbreaks = setdiff(bestbreaks, min(initial_binning[,brkp]))

      binning_1bst_brk = initial_binning[
        , bstbin := cut(brkp, c(-Inf, bestbreaks, Inf), right = FALSE,dig.lab = 10, ordered_result = FALSE)
        ][, .(variable=unique(variable), bin = paste0(bin, collapse = "%,%"), good = sum(good), bad = sum(bad)), keyby = bstbin ]
    }

    binning_1bst_brk = binning_1bst_brk[
      order(bstbin)
    ][, total_iv := iv_01(good, bad)
    ][, bstbrkp := as.numeric( sub("^\\[(.*),.+", "\\1", bstbin) )
    ][, .(variable, bin, bstbin, bstbrkp, good, bad, total_iv)]

    return(binning_1bst_brk)
  }
  bin_add_1bst = binning_add_1bst(initial_binning, bestbreaks)

  return(bin_add_1bst)
}
# required in woebin2 # return tree-like binning
woebin2_tree = function(dtm, initial_binning, min_perc_total=0.02, stop_limit=0.1, max_bin_num=5) {
  # global variables or functions
  brkp = bstbrkp = total_iv = binning_tree = NULL

  # initialize parameters
  len_brks = initial_binning[!is.na(brkp), .N] ## length all breaks
  bestbreaks=NULL ## best breaks
  IVt1 = IVt2 = 1e-10
  IVchg = 1 ## IV gain ratio
  step_num = 1

  # best breaks from three to n+1 bins
  while ( IVchg >= stop_limit & step_num+1 <= min(max_bin_num, len_brks) ) {
    binning_tree = woebin2_tree_1bst(dtm, initial_binning, min_perc_total, bestbreaks)
    # print(binning_tree)

    # best breaks
    bestbreaks = binning_tree[bstbrkp != -Inf & !is.na(bstbrkp), bstbrkp]
    # print(bestbreaks)

    # information value
    IVt2 = binning_tree[1, total_iv]
    IVchg = IVt2/IVt1-1 ## ratio gain
    IVt1 = IVt2
    # print(IVchg)

    step_num = step_num + 1
  }

  if (is.null(binning_tree)) binning_tree = initial_binning

  return(binning_tree)
}
# examples
# system.time( initial_binning <- woebin2_init_bin(dtm, min_perc_total) )
# system.time( woebin2_tree_1bst(dtm, initial_binning, min_perc_total) )
# system.time( woebin2_tree(dtm, initial_binning, min_perc_total) )

# required in woebin2 # # format binning output
binning_format = function(binning) {
  # global variables or functions
  . = bad = badprob = bin = bin_iv = good = total_iv = variable = woe = NULL
  # required columns in input binning: variable, bin, good, bad
  binning = binning[
    , badprob:=bad/(good+bad)
  ][, woe := lapply(.SD, woe_01, bad), .SDcols = "good"
  ][, bin_iv := lapply(.SD, miv_01, bad), .SDcols = "good"
  ][, total_iv := sum(bin_iv)
  ][, bin := ifelse(is.na(bin) | bin=="NA", "missing", as.character(bin)) # replace NA by missing
  ][, .(variable, bin, count=good+bad, count_distr=(good+bad)/sum(good+bad), good, bad, badprob, woe, bin_iv, total_iv)]

  # move missing from last row to first
  if ( "missing" %in% binning$bin ) {
    binning = rbind(binning[bin=="missing"], binning[bin != "missing"])
  }

  return(binning)
}

# woebin2
# This function provides woe binning for only two columns (one x and one y) dataframe.
woebin2 = function(dt, y, x, breaks=NULL, min_perc_total=0.02, stop_limit=0.1, max_bin_num=5) {
  # global variables or functions
  . = bad = badprob = bin = bin_iv = good = total_iv = variable = woe = NULL

  # data(germancredit)
  # numerical data
  # dt = setDT(germancredit)[, .(y=creditability, age.in.years)][,y:=ifelse(y=="good",1,0)];
  # y="y"; x="age.in.years"

  # categorical data
  # dt = setDT(germancredit)[, .(y=creditability, status.of.existing.checking.account)][,y:=ifelse(y=="good",1,0)];
  # y="y"; x="status.of.existing.checking.account"

  # melt data.table
  dtm = data.table(y=dt[[y]], variable=x, value=dt[[x]])


  # binning
  if (!anyNA(breaks) & !is.null(breaks)) {

    # 1.return binning if breaks provided
    binning = woebin2_breaks(dtm, breaks)

  } else {

    # 2.tree-like best breaks
    initial_binning = woebin2_init_bin(dtm, min_perc_total)
    if (stop_limit == "N") {
      binning = initial_binning
    } else {
      binning = woebin2_tree(dtm, initial_binning, min_perc_total, stop_limit, max_bin_num)
    }

  }

  return(binning_format(binning))
}

#' WOE Binning
#'
#' \code{woebin} generates optimal binning using tree-like segmentation for numerical, factor and categorical variables. \code{woebin} can also customizing breakpoints if the breaks_list was provided.
#'
#' @name woebin
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. Default NULL If x is NULL, all variables exclude y will counted as x variables.
#' @param breaks_list List of break points, defaults NULL If it is not NULL, variable binning will based on the provided breaks.
#' @param min_perc_total The share of initial binning class number over total. Accepted range: 0.01-0.2; default 0.02.
#' @param stop_limit Stop binning segmentation when information value gain ratio less than the stop_limit. Accepted range: 0-0.5; default 0.1.
#' @param max_bin_num Integer. The maximum binning number.
#' @param positive Value of positive class, default "bad|1".
#' @param print_step A non-negative integer. Default is 1. Print variable names by print_step when print_step>0. If print_step=0, no message is printed.
#' @return Optimal or customized binning information
#'
#' @seealso \code{\link{woebin_ply}}, \code{\link{woebin_plot}}, \code{\link{woebin_adj}}
#'
#' @examples
#' # load germancredit data
#' data(germancredit)
#'
#' # Example I
#' # binning for two variables in germancredit dataset
#' bins_2var = woebin(germancredit, y = "creditability", x = c("credit.amount", "purpose"))
#'
#' \dontrun{
#' # Example II
#' # binning for germancredit dataset
#' bins_germ = woebin(germancredit, y = "creditability")
#' # converting bins_germ into a dataframe
#' # bins_germ_df = data.table::rbindlist(bins_germ)
#'
#' # Example III
#' # customizing stop_limit (info-value grain ratio) for each variable
#' bins_cus_sl = woebin(germancredit, y="creditability",
#'   x=c("age.in.years", "credit.amount", "housing", "purpose"),
#'   stop_limit=c(0.05,0.1,0.01,0.1))
#'
#' # Example IV
#' # customizing the breakpoints of binning
#' breaks_list = list(
#'   age.in.years = c(25, 35, 40, 60),
#'   credit.amount = NULL,
#'   housing = c("own", "for free%,%rent"),
#'   purpose = NULL
#' )
#'
#' bins_cus_brk = woebin(germancredit, y="creditability",
#'   x=c("age.in.years", "credit.amount", "housing", "purpose"),
#'   breaks_list=breaks_list)
#' }
#'
#' @import data.table
#' @importFrom stats IQR quantile
#' @export
#'
woebin = function(dt, y, x=NULL, breaks_list=NULL, min_perc_total=0.02, stop_limit=0.1, max_bin_num=5, positive="bad|1", print_step=1L) {
  # global variables
  # detectCores = makeCluster = parLapply = NULL

  # set dt as data.table
  dt = setDT(dt)
  # remove date/time col
  dt = rm_datetime_col(dt)
  # replace "" by NA
  dt = rep_blank_na(dt)
  # check y
  dt = check_y(dt, y, positive)
  # x variable names
  xs = x_variable(dt,y,x)
  # print_step
  print_step = check_print_step(print_step)

  # breaks_list
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
          warning("There are ",length(names_x_bl)," x variables that donot specified in breaks_list were set as NULL, which means optimal binning.")
        }
      }
    }
  }

  # stop_limit vector
  if (length(stop_limit) == 1) {
    stop_limit = rep(stop_limit, length(xs))
  } else if (length(stop_limit) != length(xs)) {
    stop("Incorrect inputs; the length of stop_limit should be 1 or the same as x variables.")
  }
  # stop_limit range
  if ( stop_limit<0 || stop_limit>0.5 || !is.numeric(stop_limit) ) {
    warning("Incorrect parameter specification; accepted stop_limit parameter range is 0-0.5. Parameter was set to default (0.1).")
    sapply(stop_limit, function(x) if ( x < 0 || x > 0.5 || !is.numeric(x) ) x = 0.1 else x, simplify = TRUE)
  }

  # min_perc_total range
  if ( min_perc_total<0.01 || min_perc_total>0.2 || !is.numeric(min_perc_total) ) {
    warning("Incorrect parameter specification; accepted min_perc_total parameter range is 0.01-0.2. Parameter was set to default (0.02).")
    min_perc_total = 0.02
  }

  # max_bin_num
  if (!is.numeric(max_bin_num)) {
    stop("Incorrect inputs; max_bin_num should be numeric variable.")
  }



  # binning for each x variable ------
  x_num = 1
  xs_len = length(xs)
  # export the bins of all columns
  bins = list()
  for (i in 1:length(xs)) {
    x_i = xs[i]
    stop_limit_i = stop_limit[i]

    # print xs
    if ( is.character(dt[[x_i]]) || is.factor(dt[[x_i]]) || is.numeric(dt[[x_i]]) || is.logical(dt[[x_i]]) ) {
      if (length(unique(dt[[x_i]])) > 1) {
        if (print_step>0 & x_num %% print_step == 0) cat(paste0(format(c(x_num,xs_len)),collapse = "/"), x_i,"\n")
      } else {
        if (print_step>0 & x_num %% print_step == 0) cat(paste0(format(c(x_num,xs_len)),collapse = "/"), x_i, "------skiped","\n")
        next
      }
    } else {
      if (print_step>0 & x_num %% print_step == 0) cat("#",paste0(format(c(x_num,xs_len)),collapse = "/"), x_i, "------skiped","\n")
      next
    }
    x_num = x_num+1


    # woebining on one variable
    tryCatch(
      bins[[x_i]] <- woebin2(
        dt[, c(x_i, y), with=FALSE], y, x_i,
        breaks = breaks_list[[x_i]], min_perc_total=min_perc_total,
        stop_limit=stop_limit_i, max_bin_num = max_bin_num
      ),
      finally = next
    )
  }


  # bins = germancredit[, sapply(.SD, woebin2, y, breaks_list, min_perc_total, stop_limit, max_bin_num), .SDcols = xs]

  return(bins)
}

#' Application of Binning
#'
#' \code{woebin_ply} converts original input data into woe values based on the binning information generated by \code{woebin}.
#'
#' @param dt A data frame.
#' @param bins Binning information generated by \code{woebin}.
#' @param print_step A non-negative integer. Default is 1. Print variable names by print_step when print_step>0. If print_step=0, no message is printed.
#' @return Binning information
#'
#' @seealso  \code{\link{woebin}}, \code{\link{woebin_plot}}, \code{\link{woebin_adj}}
#'
#' @examples
#' # load germancredit data
#' data(germancredit)
#'
#' # Example I
#' dt = germancredit[, c("creditability", "credit.amount", "purpose")]
#'
#' # binning for dt
#' bins = woebin(dt, y = "creditability")
#'
#' # converting original value to woe
#' dt_woe = woebin_ply(dt, bins=bins)
#'
#' \dontrun{
#' # Example II
#' # binning for germancredit dataset
#' bins_germancredit = woebin(germancredit, y="creditability")
#'
#' # converting the values of germancredit into woe
#' # bins is a list which generated from woebin()
#' germancredit_woe = woebin_ply(germancredit, bins_germancredit)
#'
#' # bins is a dataframe
#' bins_df = data.table::rbindlist(bins_germancredit)
#' germancredit_woe = woebin_ply(germancredit, bins_df)
#' }
#'
#' @import data.table
#' @export
#'
woebin_ply = function(dt, bins, print_step=1L) {
  # global variables or functions
  . = V1 = bin = variable = woe = NULL

  # set dt as data.table
  kdt = copy(setDT(dt))
  # remove date/time col
  kdt = rm_datetime_col(kdt)
  # replace "" by NA
  kdt = rep_blank_na(kdt)
  # ncol of dt
  if (ncol(kdt) <=1 & !is.null(ncol(kdt))) stop("Incorrect inputs; dt should have at least two columns.")
  # print_step
  print_step = check_print_step(print_step)


  # bins # if (is.list(bins)) rbindlist(bins)
  if (!is.data.table(bins)) {
    if (is.data.frame(bins)) {
      bins = setDT(bins)
    } else {
      bins = rbindlist(bins)
    }
  }

  # x variables
  xs_bin = bins[,unique(variable)]
  xs_dt = names(dt)
  xs = intersect(xs_bin, xs_dt)

  # loop on x variables
  x_num = 1
  xs_len = length(xs)
  for (a in xs) {
    if (print_step > 0 & x_num %% print_step == 0) cat(paste0(format(c(x_num,xs_len)),collapse = "/"), a,"\n")
    x_num = x_num+1

    binsx = bins[variable==a] #bins[[a]]
    na_woe = binsx[bin == "missing", woe]
    binsx_narm = binsx[bin != "missing"]

    # factor or character variable
    if (is.factor(kdt[[a]]) | is.character(kdt[[a]])) {
      # # separate_rows
      # # https://stackoverflow.com/questions/13773770/split-comma-separated-column-into-separate-rows
      # binsx[, lapply(.SD, function(x) unlist(tstrsplit(x, "%,%", fixed=TRUE))), by = bstbin, .SDcols = "bin" ][copy(binsx)[,bin:=NULL], on="bstbin"]#[!is.na(bin)]

      # return
      kdt = setnames(
        binsx[, strsplit(as.character(bin), "%,%", fixed=TRUE), by = .(bin) ][binsx[, .(bin, woe)], on="bin"][,.(V1, woe)],
        c(a, paste0(a, "_woe"))
      )[kdt, on=a
      ][, (a) := NULL] #[!is.na(bin)]

    # logical or numeric variables
    } else if (is.logical(kdt[[a]]) | is.numeric(kdt[[a]])) {
      if (is.logical(kdt[[a]])) kdt[[a]] = as.numeric(kdt[[a]]) # convert logical variable to numeric

      kdt[[a]] = cut(kdt[[a]], unique(c(-Inf, binsx_narm[, as.numeric(sub("^\\[(.*),.+", "\\1", bin))], Inf)), right = FALSE, dig.lab = 10, ordered_result = FALSE)

      # return
      kdt = setnames(
        binsx[,.(bin, woe)], c(a, paste0(a, "_woe"))
      )[kdt, on = a
      ][, (a) := NULL]

    }

    # if is.na(kdt) == na_woe
    kdt[[paste0(a, "_woe")]] = ifelse(is.na(kdt[[paste0(a, "_woe")]]), na_woe,  kdt[[paste0(a, "_woe")]])

  }

  return(kdt)
}


# required in woebin_plot
#' @import data.table ggplot2
plot_bin = function(bin, title) {
  # global variables or functions
  . = bad = badprob = badprob2 = count = count_distr = count_distr2 = count_num = good = goodbad = total_iv = value = variable = woe = NULL

  # data
  ## y_right_max
  y_right_max = ceiling(max(bin$badprob, na.rm=T)*10)
  if (y_right_max %% 2 ==1) y_right_max=y_right_max+1
  if (y_right_max - max(bin$badprob, na.rm=T)*10 <= 0.3) y_right_max = y_right_max+2
  y_right_max = y_right_max/10
  if (y_right_max>1 || y_right_max<=0 || is.na(y_right_max) || is.null(y_right_max)) y_right_max=1

  ## y_left_max
  y_left_max = ceiling(max(bin$count_distr, na.rm=T)*10)/10
  if (y_left_max>1 || y_left_max<=0 || is.na(y_left_max) || is.null(y_left_max)) y_left_max=1


  ## data set
  bin = setDT(bin)
  dat = bin[,.(
    variable, bin, count_num=count, count_distr2=count_distr, count_distr, good=good/sum(count), bad=bad/sum(count), badprob, woe
  )][, `:=`(
    bin = ifelse(is.na(bin), "NA", bin),
    badprob2 = badprob*(y_left_max/y_right_max),
    badprob = round(badprob,4),
    rowid = .I
  )][, bin := factor(bin, levels = bin)]

  dat_melt = melt(dat, id.vars = c("variable", "bin","rowid"), measure.vars =c("good", "bad"), variable.name = "goodbad")[
    ,goodbad:=factor(goodbad, levels=c( "bad", "good"))
    ]

  # title
  if (!is.null(title)) title = paste0(title, "-")

  # plot
  ggplot() +
    geom_bar(data=dat_melt, aes(x=bin, y=value, fill=goodbad), stat="identity") +
    geom_text(data=dat, aes(x = bin, y = count_distr2, label = paste0(round(count_distr2*100, 1), "%, ", count_num) ), vjust = 0.5) +
    geom_line(data=dat, aes(x = rowid, y = badprob2), colour = "blue") +
    geom_point(data=dat, aes(x = rowid, y=badprob2), colour = "blue", shape=21, fill="white") +
    geom_text(data=dat, aes(x = rowid, y = badprob2, label = paste0(round(badprob*100, 1), "%")), colour="blue", vjust = -0.5) +
    scale_y_continuous(limits = c(0,y_left_max), sec.axis = sec_axis(~./(y_left_max/y_right_max), name = "Bad probability")) +
    labs(title = paste0(title, dat[1, variable],"  (iv:",bin[1,round(total_iv,4)],")"), x=NULL, y="Bin count distribution", fill=NULL) +
    theme_bw() +
    theme(
      legend.position="bottom", legend.direction="horizontal",
      axis.title.y.right = element_text(colour = "blue"),
      axis.text.y.right  = element_text(colour = "blue",angle=90, hjust = 0.5),
      axis.text.y = element_text(angle=90, hjust = 0.5) )

}
#' WOE Binning Visualization
#'
#' \code{woebin_plot} create plots of count distribution and bad probability for each bin. The binning informations are generates by  \code{woebin}.
#'
#' @name woebin_plot
#' @param bins A list or data frame. Binning information generated by \code{woebin}.
#' @param x Name of x variables. Default NULL
#' @param title String added to the front of plot title, default NULL.
#' @return List of binning plot
#'
#' @seealso  \code{\link{woebin}}, \code{\link{woebin_ply}}, \code{\link{woebin_adj}}
#'
#' @examples
#' # Load German credit data
#' data(germancredit)
#'
#' # Example I
#' dt1 = germancredit[, c("creditability", "credit.amount")]
#'
#' bins1 = woebin(dt1, y="creditability")
#' p1 = woebin_plot(bins1)
#'
#' \dontrun{
#' # Example II
#' bins = woebin(germancredit, y="creditability")
#' plotlist = woebin_plot(bins)
#'
#' # # save binning plot
#' # for (i in 1:length(plotlist)) {
#' #   ggplot2::ggsave(
#' #      paste0(names(plotlist[i]), ".png"), plotlist[[i]],
#' #      width = 15, height = 9, units="cm" )
#' #   }
#' }
#'
#' @import data.table ggplot2
#' @export
#'
woebin_plot = function(bins, x=NULL, title=NULL) {
  # global variables or functions
  variable = NULL

  # converting data.frame into list
  bins_list = list()
  if (is.data.frame(bins)) {
    bins_dt = setDT(bins)
    x = bins_dt[1, unique(variable)]

    bins = list()
    for (i in x) {
      bins[[i]] = bins_dt[variable == i]
    }

    # bins = eval(parse(text = paste0("list(", setDT(bins)[1, variable], " = bins)")))
  }
  # x variable names
  if (is.null(x) || anyNA(x)) x = names(bins)

  # plot export
  plotlist = list()
  for (i in x) plotlist[[i]] = plot_bin(bins[[i]], title)


  return(plotlist)
}


#' WOE Binning Adjustment
#'
#' \code{woebin_adj} interactively adjust the binning breaks.
#'
#' @param bins A list or data frame. Binning information generated by \code{woebin}.
#' @param dt A data frame.
#' @param y Name of y variable.
#' @param all_var Logical, default FALSE. It means whether to adjust all variables' binning breaks.
#' @param count_distr_limit The count_distr limit to adjust binning breaks, default 0.05.
#'
#' @seealso  \code{\link{woebin}}, \code{\link{woebin_ply}}, \code{\link{woebin_plot}}
#'
#' @examples
#' \dontrun{
#' # Load German credit data
#' data(germancredit)
#'
#' # Example I
#' dt = germancredit[, c("creditability", "age.in.years", "credit.amount")]
#' bins = woebin(dt, y="creditability")
#' breaks_adj = woebin_adj(bins, dt, y="creditability")
#' bins_final = woebin(dt, y="creditability",
#'                     breaks_list=breaks_adj)
#'
#' # Example II
#' binsII = woebin(germancredit, y="creditability")
#' breaks_adjII = woebin_adj(binsII, germancredit, "creditability")
#' bins_finalII = woebin(germancredit, y="creditability",
#'                     breaks_list=breaks_adjII)
#' }
#'
#' @import data.table
#' @importFrom utils menu
#' @importFrom graphics hist plot
#' @export
#'
woebin_adj = function(bins, dt, y, all_var = FALSE, count_distr_limit = 0.05) {
  # global variables or functions
  . = V1 = badprob = badprob2 = bin2 = bin_adj = count_distr = variable = x_breaks = x_class = NULL

  dt = setDT(dt)
  # bins # if (is.list(bins)) rbindlist(bins)
  if (!is.data.table(bins)) {
    if (is.data.frame(bins)) {
      bins = setDT(bins)
    } else {
      bins = rbindlist(bins)
    }
  }

  # function woebin adj plotting
  show_binadj_plot = function(dt, y, x, breaks, stop_limit) {
    text_woebin = paste0("bin_adj=woebin(dt[,c(\"",x,"\",\"",y,"\"),with=F], \"",y,"\", breaks_list=list(",x,"=c(",breaks,")), ", ifelse(stop_limit=="N","stop_limit = \"N\", ",NULL), "print_step=0L)")

    eval(parse(text = text_woebin))


    ## print adjust breaks
    breaks_bin = setdiff(sub("^\\[(.*),.+", "\\1", bin_adj[[1]]$bin), c("-Inf","Inf","missing"))
    breaks_bin = ifelse(
      is.numeric(dt[[x]]),
      paste0(breaks_bin, collapse=", "),
      paste0(paste0("\"",breaks_bin,"\""), collapse=", "))
    cat("> Current breaks: ","\n",breaks_bin,"\n","\n")

    # print bin_adj
    print(woebin_plot(bin_adj)[[1]])

    # # breaks
    if (breaks == "" || is.null(breaks)) breaks = breaks_bin

    return(breaks)
  }

  # class of variables
  vars_class = dt[,sapply(.SD, class), .SDcols = bins[,unique(variable)]]
  vars_class = data.table(variable = names(vars_class), x_class = vars_class)
  # breakslist of bins
  bins_breakslist = bins[
    , bin2 := sub("^\\[(.*),.+", "\\1", bin)
    ][!(bin2 %in% c("-Inf","Inf","missing"))
    ][vars_class, on="variable"
    ][, .(x_breaks = paste(ifelse(x_class == "numeric", bin2, paste0("\"", bin2, "\"")), collapse = ", ")), by=variable]

  # x variables
  if (all_var == FALSE) {
    xs = union(
      bins[count_distr < count_distr_limit, unique(variable)],
      bins[bin != "missing"][, badprob2 := badprob >= shift(badprob, type = "lag"), by="variable"][!is.na(badprob2), length(unique(badprob2)), by="variable"][V1 > 1, variable]
    )
  } else {
    xs = bins[,unique(variable)]
  }

  xs_len = length(xs)
  i = 1
  breaks_list = NULL
  while (i <= xs_len) {
    # gn = readline(paste0("> Go to nth Var (e.g.: g4,Go 4th): "))

    # x variable
    x = xs[i]
    cat("--------", paste0(i, "/", xs_len), x, "--------\n")
    bin = bins[variable==x]
    breaks = stop_limit = NULL

    # basic information of x variable ------
    ## class
    cat(paste0("> class(",x,"): "),"\n",class(dt[[x]]),"\n","\n")
    ## summary
    cat(paste0("> summary(",x,"): "),"\n")
    print(summary(dt[[x]]))
    cat("\n")
    ## table
    if (length(table(dt[[x]])) < 10) {
      cat(paste0("> table(",x,"): "))
      print(table(dt[[x]]))
      cat("\n")
    } else {
      if ( is.numeric(dt[[x]])) {
        ht = hist(dt[[x]], plot = FALSE)
        plot(ht, main = x, xlab = NULL)
      }
    }
    ## current breaks
    breaks_bin = bins_breakslist[variable == x, x_breaks]

    cat("> Current breaks: ","\n", breaks_bin,"\n","\n")
    ## woebin plotting
    plist = woebin_plot(bin)
    print(plist[[1]])

    # adjusting breaks ------
    while (menu(c("No", "Yes"), title=paste0("> Adjust breaks for (", i, "/", xs_len, ") ", x, "?")) == 2) {
      breaks = readline("> Enter modified breaks: ")
      breaks = gsub("^[,\\.]+|[,\\.]+$", "", breaks)
      if (breaks == "N") {
        stop_limit = "N"
        breaks = NULL
      } else {
        stop_limit = NULL
      }

      tryCatch(breaks <- show_binadj_plot(dt, y, x, breaks, stop_limit), finally=next)
    }


    if (!(is.null(breaks) || breaks == "")) bins_breakslist[variable == x][["x_breaks"]]  <- breaks
    i = i + 1
  }


  breaks_list = paste0(bins_breakslist[, paste0(variable, "=c(", x_breaks, ")")], collapse = ", \n ")
  breaks_list = paste0(c("list(", breaks_list, ")"), collapse = "\n ")


  cat(breaks_list,"\n")
  return(breaks_list)
}
