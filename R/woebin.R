# woebin2
# This function provides woe binning for only two columns (one x and one y) dataframe.
woebin2 = function(dt, y, x, breaks=NULL, min_perc_total=0.02, stop_limit=0.1, max_bin_num=5, print_step=FALSE) {

  value = . = variable = bad = good = woe = bin_iv = total_iv = badprob = V1 = NULL # no visible binding for global variable

  # method="tree",

  # data(germancredit)
  # numerical data
  # dt = setDT(germancredit)[, .(y=creditability, age.in.years)][,y:=ifelse(y=="good",1,0)];
  # y="y"; x="age.in.years"

  # categorical data
  # dt = setDT(germancredit)[, .(y=creditability, status.of.existing.checking.account)]
  # y="y"; x="status.of.existing.checking.account"

  # dt = setDT(germancredit)[, .(creditability, present.employment.since, age.in.years)]


  # input data.table
  dtm = data.table(y = dt[[y]], variable=x, value = dt[[x]])
  # convert logical value into numeric
  if (is.logical(dtm[,value])) dtm[, value := as.numeric(value)]

  # 1.return binning if provide breakpoints ------
  if ( !anyNA(breaks) & !is.null(breaks) ) {
    if ( is.numeric(dtm[,value]) | is.logical(dtm[,value]) ) {
      brkp = unique(c(-Inf, breaks, Inf))

      bin = copy(dtm)[
        , bin := cut(value, brkp, right = FALSE, dig.lab = 10, ordered_result = FALSE)
        ][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , keyby = bin
        ][order(bin)
        ][, `:=`(bstbin=bin, bstbrkp = as.numeric( sub("^\\[(.*),.+", "\\1", bin)), badprob = bad/(good+bad) )
        ][, woe := lapply(.SD, woe_01, bad), .SDcols = "good"
        ][, bin_iv := lapply(.SD, miv_01, bad), .SDcols = "good"
        ][, total_iv := sum(bin_iv)
        ][, .(variable, bin, count=good+bad, count_distr=(good+bad)/(sum(good)+sum(bad)), good, bad, badprob, woe, bin_iv, total_iv)]

    } else if (is.factor(dtm[,value]) | is.character(dtm[,value])) {

      bin = merge(
        data.table(rowid=1:length(breaks), bin = breaks, value = breaks )[, strsplit(as.character(value), "%,%", fixed=TRUE), by = c("rowid","bin") ][, .(rowid, bin, value = V1)],
        dtm, all.y = TRUE
      )[, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , keyby = c("rowid","bin")
      ][, `:=`(badprob = bad/(good+bad), bin = ifelse(is.na(bin), "missing", bin) )
      ][, woe := lapply(.SD, woe_01, bad), .SDcols = "good"
      ][, bin_iv := lapply(.SD, miv_01, bad), .SDcols = "good"
      ][, total_iv := sum(bin_iv)
      ][, .(variable, bin, count=good+bad, count_distr=(good+bad)/(sum(good)+sum(bad)), good, bad, badprob, woe, bin_iv, total_iv)]

    }
    return(bin)
  }

  # 2.functions ------
  ###### get breakpoints of initial binning
  breakpoints = function(dtm, min_perc_total) {
    . = value = variable = bad = good = badprob = NULL # no visible binding for global variable

    if ( is.numeric(dtm[,value]) | is.logical(dtm[,value]) ) {
      xvec = dtm[, value]

      # breakpoints vector & outlier
      iq = quantile(xvec, na.rm = TRUE)
      iqr = IQR(xvec, na.rm = TRUE)
      if (iqr == 0) {
        xvec_rm_outlier = xvec
      } else {
        xvec_rm_outlier = xvec[which(xvec > iq[2]-1.5*iqr & xvec <= iq[4]+1.5*iqr)]
      }

      # number of initial binning
      n = trunc(1/min_perc_total)
      len_uniq_x = length(setdiff(unique(xvec_rm_outlier), c(NA,Inf,-Inf)))
      if (len_uniq_x < n) n = length(unique(xvec_rm_outlier))

      # initial breakpoints
      if (len_uniq_x < 10) {
        brkp = setdiff(unique(xvec_rm_outlier), c(NA, Inf, -Inf))
      } else {
        brkp = pretty(xvec_rm_outlier, n)
      }
      brkp = sort(brkp)
      # brkp = quantile(xvec, (0:n)/n, na.rm=TRUE)
      brkp = unique(c(-Inf, brkp[2:(length(brkp)-1)], Inf))
      if (anyNA(xvec)) brkp = c(brkp, NA)


      # binned datatable
      brkdt = copy(dtm)[
        , bin := cut(value, brkp, right = FALSE, dig.lab = 10, ordered_result = FALSE)
        ][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , keyby = bin
        ][order(bin)
        ][, `:=`(brkp = as.numeric( sub("^\\[(.*),.+", "\\1", bin)), badprob = bad/(good+bad) )
        ][, .(variable, bin, brkp, good, bad, badprob)]

    } else if ( is.factor(dtm[,value]) | is.character(dtm[,value]) ) {
      brkdt = copy(dtm)[
        , .(variable = unique(variable), good = sum(y==0), bad = sum(y==1)), by=value
        ][, badprob := bad/(good+bad)]

      # order by bin if is.factor, or by badprob if is.character
      if (is.factor(dtm[,value])) {
        brkdt = brkdt[
          order(value)
        ][, brkp := ifelse(is.na(value), NA, .I)
        ][, .(variable, bin=value, brkp, good, bad, badprob)]

      } else {
        brkdt = brkdt[
          order(badprob)
        # next 3 lines make NA located at the last rows
        ][, brkp := ifelse(is.na(value), NA, .I)
        ][order(brkp)
        ][, brkp := ifelse(is.na(value), NA, .I)
        ][, .(variable, bin=value, brkp, good, bad, badprob)]

      }

    }

    return(brkdt)
  }

  # add one tree-like best breakpoints
  # requried by all_bst_brkp
  add_bst_brkp = function(initial_brkpdt, bst_brkp = NULL) {
    patterns = . = good = bad = variable = total_iv = bstbin = NULL # no visible binding for global variable

    # best breakpoints
    bestbreakpoints = function(initial_brkpdt, bst_brkp, only_total_iv=TRUE) {
      value = bstbin = . = good = bad = variable = woe = bin_iv = total_iv = bstbrkp = badprob = NULL # no visible binding for global variable

      if ( is.numeric(dtm[,value]) | is.logical(dtm[,value]) ) {
        initial_brkpdt[
          , bstbin := cut(brkp, c(-Inf, bst_brkp, Inf), right = FALSE, dig.lab = 10, ordered_result = FALSE)
          ][, .(good = sum(good), bad = sum(bad), variable=unique(variable)) , keyby = bstbin
          ][, `:=`(badprob = bad/(good+bad), bin = bstbin )
          ][order(bstbin)
          ][, woe := lapply(.SD, woe_01, bad), .SDcols = "good"
          ][, bin_iv := lapply(.SD, miv_01, bad), .SDcols = "good"
          ][, total_iv := sum(bin_iv)
          ][, bstbrkp := as.numeric( sub("^\\[(.*),.+", "\\1", bstbin) )
          ][, .(variable, bin, bstbin, bstbrkp, good, bad, badprob, woe, bin_iv, total_iv)]


      } else if ( is.factor(dtm[,value]) | is.character(dtm[,value]) ) {
        bst_brkp = setdiff(bst_brkp, min(initial_brkpdt[,brkp]))

        initial_brkpdt[
          , bstbin := cut(brkp, c(-Inf, bst_brkp, Inf), right = FALSE,dig.lab = 10, ordered_result = FALSE)
          ][, .(variable=unique(variable), bin = paste0(bin, collapse = "%,%"), good = sum(good), bad = sum(bad)), keyby = bstbin
          ][, badprob:=bad/(good+bad)
          ][order(bstbin)
          ][, woe := lapply(.SD, woe_01, bad), .SDcols = "good"
          ][, bin_iv := lapply(.SD, miv_01, bad), .SDcols = "good"
          ][, total_iv := sum(bin_iv)
          ][, bstbrkp := as.numeric( sub("^\\[(.*),.+", "\\1", bstbin) )
          ][, .(variable, bin, bstbin, bstbrkp, good, bad, badprob, woe, bin_iv, total_iv) ]
        # variable bstbin bstbrkp good bad badprob woe miv total_iv

      }


    }

    # best breakpoints set
    bst_brkp_set = setdiff( initial_brkpdt[,brkp], c(bst_brkp, -Inf, Inf, NA) )

    # loop on bst_brkp_set
    for (i in bst_brkp_set) {
      # best breakpoint + i
      bst_brkp_i = sort(c(bst_brkp, i))

      # best breakpoint datatable
      bst_brkpdt = initial_brkpdt[
        , paste0("bstbin",i) := cut(brkp, c(-Inf, bst_brkp_i, Inf), right = FALSE, dig.lab = 10, ordered_result = FALSE) ]
    }
    # best breakpoint dt
    bstbin_total_iv = melt(
      bst_brkpdt, id = c("variable", "good", "bad"), variable.name = "bstbin", measure = patterns("bstbin.+")
    )[, .(good = sum(good), bad = sum(bad), variable = unique(variable))
      , keyby=c("bstbin", "value")
    ][, .(total_iv = iv_01(good, bad), variable = unique(variable))
      , keyby="bstbin"
    ][, bstbin := as.numeric(sub("bstbin(.+)", "\\1", bstbin))][]


    # bst_brkp that total_iv==max(total_iv) & min(count_distr)>=0.2
    rm_bst_brkp = NULL
    min_count_distr = 0
    while (min_count_distr < min_perc_total) {

      if (!is.null(rm_bst_brkp)) bstbin_total_iv = bstbin_total_iv[!(bstbin %in% rm_bst_brkp)]
      # if (nrow(bstbin_total_iv) != 0) {
        sel_bstbin_max_total_iv = bstbin_total_iv[total_iv==max(total_iv, na.rm = TRUE), bstbin]
      # } else {
      #   sel_bstbin_max_total_iv = NULL
      # }
      # print(sel_bstbin_max_total_iv)

      # return
      bst_brkp = sort(unique(c(bst_brkp,sel_bstbin_max_total_iv)))
      bst_bins_dt = bestbreakpoints(initial_brkpdt, bst_brkp, only_total_iv=FALSE)


      # minimum count_distr
      min_count_distr = bst_bins_dt[!is.na(bstbin)][,min(count_distr=(good+bad)/sum(good+bad))]
      # excluded bst_brkp that min_count_distr < 0.2
      if (min_count_distr < min_perc_total) {
        bst_brkp = setdiff(bst_brkp, sel_bstbin_max_total_iv)

        rm_bst_brkp = c(rm_bst_brkp, sel_bstbin_max_total_iv)
      }
    }

    return(bst_bins_dt)
  }
  ###### all tree-like best breakpoints
  all_bst_brkp = function(initial_brkpdt, stop_limit=0.1, max_bin_num=5, print_step=TRUE) {
    total_iv = bstbrkp = NULL# no visible binding for global variable

    len_brkp = length( setdiff(initial_brkpdt[, brkp], c(-Inf, Inf, NA)) )

    # best breakpoints for two bins
    ALL_bst_brkp = add_bst_brkp(initial_brkpdt)
    if (print_step) print(ALL_bst_brkp)
    IVt1 = ALL_bst_brkp[1, total_iv]

    len_step = 2
    if (len_brkp >= 2) {
      # initial information value gain ratio
      IVchg = 1
    # best breakpoints from three to n+1 bins
    while ( IVchg >= stop_limit & len_step+1 <= min(max_bin_num, len_brkp) ) {
      ALL_bst_brkp = add_bst_brkp(initial_brkpdt, ALL_bst_brkp[bstbrkp != -Inf, bstbrkp])
      if (print_step) print(ALL_bst_brkp)

      IVt2 = ALL_bst_brkp[1, total_iv]
      # information value gain ratio
      IVchg = IVt2/IVt1-1
      # print(IVchg,"\n")
      IVt1 = IVt2

      len_step = len_step + 1
    }
    }

    return(ALL_bst_brkp)
  }
  # examples
  # system.time( initial_brkpdt = breakpoints(dtm, min_perc_total) )
  # system.time(add_bst_brkp(initial_brkpdt))
  # system.time(all_bst_brkp(initial_brkpdt, print_step = TRUE))

  # 3.run functions ------
  initial_brkpdt = breakpoints(dtm, min_perc_total)
  if (stop_limit == "N") return(initial_brkpdt)
  bst_brkpdt = all_bst_brkp(initial_brkpdt, stop_limit, max_bin_num, print_step)

  return(bst_brkpdt)
}

#' WOE Binning
#'
#' \code{woebin} generates optimal binning for both numerical and categorical variables using tree-like segmentation. For the categorical variables, the binning segmentation will ordered by the levels for factor and by the bad probability for character. \code{woebin} can also customizing breakpoints for both numerical and categorical variables.
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
#' @param order Logical. If it is TRUE, return binning information by descending sorted iv values.
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
woebin = function(dt, y, x=NULL, breaks_list=NULL, min_perc_total=0.02, stop_limit=0.1, max_bin_num=5, positive="bad|1", order=FALSE, print_step=1L) {
  # method="tree",

  x_num = bin = variable = total_iv = good = bad = badprob = woe = bin_iv = . = NULL # no visible binding for global variable

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




  # parameter for print ------
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
      )[, bin := ifelse(is.na(bin) | bin=="NA", "missing", as.character(bin)) # renmae NA as missing
      ],
      finally = next
    )
  }

  # reorder bins by iv ------
  total_iv_list = unique(rbindlist(bins)[, .(variable, total_iv)])
  if (order == TRUE) total_iv_list = total_iv_list[order(-total_iv)]

  bins_list = list()
  for (v in total_iv_list$variable) {
    bins_adj = bins[[v]][,.(variable, bin, count=good+bad,  count_distr=(good+bad)/(sum(good)+sum(bad)), good, bad, badprob, woe, bin_iv, total_iv)]

    # move missing from last row to first
    if ( "missing" %in% bins_adj$bin ) {
      bins_adj = rbind(bins_adj[bin=="missing"], bins_adj[bin != "missing"])
    }

    bins_list[[v]] = bins_adj
  }

  # return(list(bins = bins_list, iv=total_iv_list))
  return(bins_list)
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
woebin_ply = function(dt, bins, print_step=1L) { # dt, y, x=NA, bins
  . = variable = bin = woe = V1 = NULL  # no visible binding for global variable

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
  . = variable = count = count_distr = good = bad = badprob = woe = goodbad = value = count_num = badprob2 = count_distr2 = total_iv = NULL # no visible binding for global variable

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
  variable = NULL # no visible binding for global variable

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
woebin_adj = function(bins, dt, y) {
  variable = p = bin_adj = NULL

  dt = setDT(dt)
  # bins # if (is.list(bins)) rbindlist(bins)
  if (!is.data.table(bins)) {
    if (is.data.frame(bins)) {
      bins = setDT(bins)
    } else {
      bins = rbindlist(bins)
    }
  }

  # x variables
  xs = bins[,unique(variable)]
  xs_len = length(xs)
  break_list = NULL

  for (i in 1:xs_len) {
    # x variable
    x = xs[i]
    cat("--------", paste0(i,"/",xs_len), x, "--------\n")
    bin = bins[variable==x]
    breaks = NULL

    # print basic information of data
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
    breaks_bin = setdiff(sub("^\\[(.*),.+", "\\1", bin$bin), c("-Inf","Inf","missing"))
    breaks_bin = ifelse(
      is.numeric(dt[[x]]),
      paste0(breaks_bin, collapse=", "),
      paste0(paste0("\"",breaks_bin,"\""), collapse=", "))
    cat("> Current breaks: ","\n", breaks_bin,"\n","\n")
    ## woebin plotting
    plist = woebin_plot(bin)
    print(plist[[1]])

    # adjusting breaks
    while (menu(c("No", "Yes"), title=paste0("> Adjust breaks for (", i, "/", xs_len, ") ", x, "?")) == 2) {
      breaks = readline("> Enter modified breaks: ")
      breaks = gsub("^[,\\.]+|[,\\.]+$", "", breaks)

      # woebin adj plotting
      show_binadj_plot = function(dt, y, x, breaks) {
        eval(parse(text = paste0(
          "bin_adj=woebin(dt[,c(\"",x,"\",\"",y,"\"),with=F],\"",y,"\",breaks_list = list(",x,"=c(",breaks,")),print_step=0L)")))

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
        if (breaks == "") breaks = breaks_bin

        return(breaks)
      }

      tryCatch(show_binadj_plot(dt, y, x, breaks), finally=next)
    }


    if (is.null(breaks) | breaks == "") breaks = breaks_bin
    # break_list
    if (length(breaks)>0) {
      break_list = c(break_list, paste0(x,"=c(",breaks,")"))
      break_list = paste0(break_list, collapse=", \n")
    }
  }
  break_list = paste0(c("list(",break_list,")"),collapse = "\n")
  cat(break_list,"\n")

  return(break_list)
}
