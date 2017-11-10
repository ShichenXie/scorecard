# woebin2
# This function provides woe binning for only two columns (one x and one y) dataframe.
woebin2 <- function(dt, y, x, breaks=NULL, min_perc_total=0.02, stop_limit=0.1, max_bin_num=5, print_step=FALSE) {

  value = . = variable = bad = good = woe = bin_iv = total_iv = badprob = V1 = NULL # no visible binding for global variable

  # method="tree",

  # data(germancredit)
  # numerical data
  # dt <- setDT(germancredit)[, .(y=creditability, age.in.years)];
  # y="y"; x="age.in.years"

  # categorical data
  # dt <- setDT(germancredit)[, .(y=creditability, status.of.existing.checking.account)]
  # y="y"; x="status.of.existing.checking.account"

  # dt <- setDT(germancredit)[, .(creditability, present.employment.since, age.in.years)]


  # input data.table
  dtm <- data.table(y = dt[[y]], variable=x, value = dt[[x]])
  # convert logical value into numeric
  if (is.logical(dtm[,value])) dtm[, value := as.numeric(value)]

  # return binning if provide breakpoints ------
  if ( !anyNA(breaks) & !is.null(breaks) ) {
    if ( is.numeric(dtm[,value]) | is.logical(dtm[,value]) ) {
      brkp <- unique(c(-Inf, breaks, Inf))

      bin <- copy(dtm)[
        , bin := cut(value, brkp, right = FALSE, dig.lab = 10, ordered_result = FALSE)
        ][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , keyby = bin
        ][order(bin)
        ][, `:=`(bstbin=bin, bstbrkp = as.numeric( sub("^\\[(.*),.+", "\\1", bin)), badprob = bad/(good+bad) )
        ][, woe := lapply(.SD, woe_01, bad), .SDcols = "good"
        ][, bin_iv := lapply(.SD, miv_01, bad), .SDcols = "good"
        ][, total_iv := sum(bin_iv)
        ][, .(variable, bin, count=good+bad, count_distr=(good+bad)/(sum(good)+sum(bad)), good, bad, badprob, woe, bin_iv, total_iv)]

    } else if (is.factor(dtm[,value]) | is.character(dtm[,value])) {

      bin <- merge(
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

  # functions ------
  ###### get breakpoints of initial binning
  breakpoints <- function(dtm, min_perc_total) {
    . = value = variable = bad = good = badprob = NULL # no visible binding for global variable

    if ( is.numeric(dtm[,value]) | is.logical(dtm[,value]) ) {
      xvec <- dtm[, value]

      # breakpoints vector & outlier
      iq <- quantile(xvec, na.rm = TRUE)
      iqr <- IQR(xvec, na.rm = TRUE)
      if (iqr == 0) {
        xvec_rm_outlier <- xvec
      } else {
        xvec_rm_outlier <- xvec[which(xvec > iq[2]-3*iqr & xvec <= iq[4]+3*iqr)]
      }

      # number of initial binning
      n <- trunc(1/min_perc_total)
      if (length(unique(xvec_rm_outlier)) < n) n <- length(unique(xvec_rm_outlier))

      # initial breakpoints
      brkp <- pretty(xvec_rm_outlier, n)
      # brkp <- quantile(xvec, (0:n)/n, na.rm=TRUE)
      brkp <- unique(c(-Inf, brkp[2:(length(brkp)-1)], Inf))
      if (anyNA(xvec)) brkp <- c(brkp, NA)


      # binned datatable
      brkdt <- copy(dtm)[
        , bin := cut(value, brkp, right = FALSE, dig.lab = 10, ordered_result = FALSE)
        ][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , keyby = bin
        ][order(bin)
        ][, `:=`(brkp = as.numeric( sub("^\\[(.*),.+", "\\1", bin)), badprob = bad/(good+bad) )
        ][, .(variable, bin, brkp, good, bad, badprob)]

    } else if ( is.factor(dtm[,value]) | is.character(dtm[,value]) ) {
      brkdt <- copy(dtm)[
        , .(variable = unique(variable), good = sum(y==0), bad = sum(y==1)), by=value
        ][, badprob := bad/(good+bad)]

      # order by bin if is.factor, or by badprob if is.character
      if (is.factor(dtm[,value])) {
        brkdt <- brkdt[
          order(value)
        ][, brkp := ifelse(is.na(value), NA, .I)
        ][, .(variable, bin=value, brkp, good, bad, badprob)]

      } else {
        brkdt <- brkdt[
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
  add_bst_brkp <- function(initial_brkpdt, bst_brkp = c()) {
    patterns = . = good = bad = variable = total_iv = bstbin = NULL # no visible binding for global variable

    # best breakpoints
    bestbreakpoints <- function(initial_brkpdt, bst_brkp, only_total_iv=TRUE) {
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
        bst_brkp <- setdiff(bst_brkp, min(initial_brkpdt[,brkp]))

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
    bst_brkp_set <- setdiff( initial_brkpdt[,brkp], c(bst_brkp, -Inf, Inf, NA) )

    # loop on bst_brkp_set
    for (i in bst_brkp_set) {
      # best breakpoint + i
      bst_brkp_i <- sort(c(bst_brkp, i))

      # best breakpoint datatable
      bst_brkpdt <- initial_brkpdt[
        , paste0("bstbin",i) := cut(brkp, c(-Inf, bst_brkp_i, Inf), right = FALSE, dig.lab = 10, ordered_result = FALSE)
        ]
    }

    bst_brkp_max_total_iv <- melt(
      bst_brkpdt, id = c("variable", "good", "bad"), variable.name = "bstbin", measure = patterns("bstbin.+")
    )[, .(good = sum(good), bad = sum(bad), variable = unique(variable))
      , keyby=c("bstbin", "value")
    ][, .(total_iv = iv_01(good, bad), variable = unique(variable))
      , keyby="bstbin"
    ][total_iv==max(total_iv), as.numeric(sub("bstbin(.+)", "\\1", bstbin))]


    # return
    bst_brkp <- sort( unique(c(bst_brkp,  bst_brkp_max_total_iv)) )
    #
    # bst_bins_dt <-
      bestbreakpoints(initial_brkpdt, bst_brkp, only_total_iv=FALSE)

    # return(list(best_breakpoints = bst_brkp, best_bins_datatable = bst_bins_dt, total_iv = bst_bins_dt[1, total_iv]))
  }
  ###### all tree-like best breakpoints
  all_bst_brkp <- function(initial_brkpdt, stop_limit=0.1, max_bin_num=5, print_step=TRUE) {
    total_iv = bstbrkp = NULL# no visible binding for global variable

    len_brkp <- length( setdiff(initial_brkpdt[, brkp], c(-Inf, Inf, NA)) )

    # best breakpoints for two bins
    ALL_bst_brkp <- add_bst_brkp(initial_brkpdt)
    if (print_step) print(ALL_bst_brkp)
    IVt1 <- ALL_bst_brkp[1, total_iv]

    len_step = 1
    if (len_brkp >= 2) {
      # initial information value gain ratio
      IVchg <- 1
    # best breakpoints from three to n+1 bins
    while (IVchg >= stop_limit) {
      ALL_bst_brkp <- add_bst_brkp(initial_brkpdt, ALL_bst_brkp[bstbrkp != -Inf, bstbrkp])
      if (print_step) print(ALL_bst_brkp)

      IVt2 <- ALL_bst_brkp[1, total_iv]
      # information value gain ratio
      IVchg <- IVt2/IVt1-1
      # print(IVchg,"\n")
      IVt1 <- IVt2

      len_step = len_step + 1
      if (len_step >= len_brkp | len_step >= max_bin_num-1) break
    }
    }

    return(ALL_bst_brkp)
  }
  # examples
  # system.time( initial_brkpdt <- breakpoints(dtm, min_perc_total) )
  # system.time(add_bst_brkp(initial_brkpdt))
  # system.time(all_bst_brkp(initial_brkpdt, print_step = TRUE))

  # run functions ------
  initial_brkpdt <- breakpoints(dtm, min_perc_total)
  if (stop_limit == "N") return(initial_brkpdt)
  all_bst_brkp(initial_brkpdt, stop_limit, max_bin_num, print_step)
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
#' @seealso \code{\link{woebin_ply}}, \code{\link{woebin_plot}}
#'
#' @examples
#' # load germancredit data
#' data(germancredit)
#'
#' # Example I
#' # binning for two variables in germancredit dataset
#' bins_2var <- woebin(germancredit, y = "creditability", x = c("credit.amount", "purpose"))
#'
#' \dontrun{
#' # Example II
#' # binning for germancredit dataset
#' bins_germ <- woebin(germancredit, y = "creditability")
#'
#' # Example III
#' # customizing stop_limit (info-value grain ratio) for each variable
#' bins_cus_sl <- woebin(germancredit, y="creditability",
#'   x=c("age.in.years", "credit.amount", "housing", "purpose"),
#'   stop_limit=c(0.05,0.1,0.01,0.1))
#'
#' # Example IV
#' # customizing the breakpoints of binning
#' breaks_list <- list(
#'   age.in.years = c(25, 35, 40, 60),
#'   credit.amount = NULL,
#'   housing = c("own", "for free%,%rent"),
#'   purpose = NULL
#' )
#'
#' bins_cus_brk <- woebin(germancredit, y="creditability",
#'   x=c("age.in.years", "credit.amount", "housing", "purpose"),
#'   breaks_list=breaks_list)
#' }
#'
#' @import data.table
#' @importFrom stats IQR quantile
#' @export
#'
woebin <- function(dt, y, x=NULL, breaks_list=NULL, min_perc_total=0.02, stop_limit=0.1, max_bin_num=5, positive="bad|1", order=FALSE, print_step=1L) {
  # method="tree",

  x_num = bin = variable = total_iv = good = bad = badprob = woe = bin_iv = . = NULL # no visible binding for global variable

  # set dt as data.table
  dt <- setDT(dt)
  # replace "" by NA
  dt <- rep_blank_na(dt)
  # check y
  dt <- check_y(dt, y, positive)
  # x variable names
  x <- x_variable(dt,y,x)
  # print_step
  print_step <- check_print_step(print_step)

  # breaks_list
  if (!is.null(breaks_list)) {
    if (!is.list(breaks_list)) {
      stop("Incorrect inputs; breaks_list should be a list.")
    } else {
      # length of breaks_list != x
      if (length(breaks_list) < length(x)) {
        x_bl <- setdiff(x, names(breaks_list))
        warning(paste0("Incorrect inputs; the length of breaks_list < x's. The variables (", paste0(x_bl, collapse = ","), "=NULL) were added into breaks_list."))
        for (i in x_bl) { breaks_list[[i]] <- NULL }

      } else if (length(breaks_list) > length(x)) {
        warning(paste0("Incorrect inputs; the length of breaks_list > x's. The variables (", paste0(setdiff(names(breaks_list),x), collapse = ","), ") were removed from breaks_list."))

      }
    }
  }

  # stop_limit vector
  if (length(stop_limit) == 1) {
    stop_limit = rep(stop_limit, length(x))
  } else if (length(stop_limit) != length(x)) {
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




  # parameter for print
  x_num <- 1
  x_length <- length(x)
  # export the bins of all columns
  bins <- list()
  for (i in 1:length(x)) {
    x_i <- x[i]
    stop_limit_i <- stop_limit[i]

    # print x
    if ( is.character(dt[[x_i]]) || is.factor(dt[[x_i]]) || is.numeric(dt[[x_i]]) || is.logical(dt[[x_i]]) ) {
      if (length(unique(dt[[x_i]])) > 1) {
        if (print_step>0 & x_num %% print_step == 0) cat(paste0(format(c(x_num,x_length)),collapse = "/"), x_i,"\n")
      } else {
        if (print_step>0 & x_num %% print_step == 0) cat(paste0(format(c(x_num,x_length)),collapse = "/"), x_i, "------skiped","\n")
        next
      }
    } else {
      if (print_step>0 & x_num %% print_step == 0) cat(paste0(format(c(x_num,x_length)),collapse = "/"), x_i, "------skiped","\n")
      next
    }
    x_num <- x_num+1


    # woebining on one variable
    bin2 <- woebin2(
      dt[, c(x_i, y), with=FALSE], y, x_i,
      breaks = breaks_list[[x_i]], min_perc_total=min_perc_total,
      stop_limit=stop_limit_i, max_bin_num = max_bin_num)


    # renmae NA as missing
    bins[[x_i]] <- bin2[, bin := ifelse(is.na(bin) | bin=="NA", "missing", as.character(bin))]
  }

  # total_iv list
  total_iv_list <- unique(rbindlist(bins)[, .(variable, total_iv)])
  if (order == TRUE) total_iv_list <- total_iv_list[order(-total_iv)]

  # reorder bins by iv
  bins_list <- list()
  for (v in total_iv_list$variable) {
    bins_adj <- bins[[v]][,.(variable, bin, count=good+bad,  count_distr=(good+bad)/(sum(good)+sum(bad)), good, bad, badprob, woe, bin_iv, total_iv)]

    if ( "missing" %in% bins_adj$bin ) {
      bins_adj <- rbind(bins_adj[bin == "missing"], bins_adj[bin != "missing"])
    }

    bins_list[[v]] <- setDF( bins_adj )
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
#' @seealso  \code{\link{woebin}}, \code{\link{woebin_plot}}
#'
#' @examples
#' # load germancredit data
#' data(germancredit)
#'
#' # Example I
#' dt <- germancredit[, c("creditability", "credit.amount", "purpose")]
#'
#' # binning for dt
#' bins <- woebin(dt, y = "creditability")
#'
#' # converting original value to woe
#' dt_woe <- woebin_ply(dt, bins=bins)
#'
#' \dontrun{
#' # Example II
#' # binning for germancredit dataset
#' bins_germancredit <- woebin(germancredit, y="creditability")
#'
#' # converting the values of germancredit into woe
#' # bins is a list which generated from woebin()
#' germancredit_woe <- woebin_ply(germancredit, bins_germancredit)
#'
#' # bins is a dataframe
#' bins_df <- data.table::rbindlist(bins_germancredit)
#' germancredit_woe <- woebin_ply(germancredit, bins_df)
#' }
#'
#' @import data.table
#' @export
#'
woebin_ply <- function(dt, bins, print_step=1L) { # dt, y, x=NA, bins
  . = variable = bin = woe = V1 = NULL  # no visible binding for global variable

  # set dt as data.table
  kdt <- copy(setDT(dt))
  # replace "" by NA
  kdt <- rep_blank_na(kdt)
  # ncol of dt
  if (ncol(dt) <=1 & !is.null(ncol(dt))) stop("Incorrect inputs; dt should have at least two columns.")
  # print_step
  print_step <- check_print_step(print_step)


  # bins # if (is.list(bins)) rbindlist(bins)
  if (!is.data.table(bins)) {
    if (is.data.frame(bins)) {
      bins <- setDT(bins)
    } else {
      bins <- rbindlist(bins)
    }
  }

  # x variables
  x <- bins[,unique(variable)]
  # if (anyNA(x)) {
  # if (length(setdiff(names(kdt), y)) >= length(bins[,unique(variable)])) {
  #   x <- bins[,unique(variable)]
  # } else {
  #   x <- setdiff(names(kdt), y)
  # }
  # }

  # loop on x variables
  x_num <- 1
  x_length <- length(x)
  for (a in x) {
    if (print_step > 0 & x_num %% print_step == 0) cat(paste0(format(c(x_num,x_length)),collapse = "/"), a,"\n")
    x_num <- x_num+1

    binsx <- bins[variable==a] #bins[[a]]
    na_woe <- binsx[bin == "missing", woe]

    # factor or character variable
    if (is.factor(kdt[[a]]) | is.character(kdt[[a]])) {
      # # separate_rows
      # # https://stackoverflow.com/questions/13773770/split-comma-separated-column-into-separate-rows
      # binsx[, lapply(.SD, function(x) unlist(tstrsplit(x, "%,%", fixed=TRUE))), by = bstbin, .SDcols = "bin" ][copy(binsx)[,bin:=NULL], on="bstbin"]#[!is.na(bin)]

      # return
      kdt <- setnames(
        binsx[, strsplit(as.character(bin), "%,%", fixed=TRUE), by = .(bin) ][binsx[, .(bin, woe)], on="bin"][,.(V1, woe)],
        c(a, paste0(a, "_woe"))
      )[kdt, on=a
      ][, (a) := NULL] #[!is.na(bin)]

    # logical or numeric variables
    } else if (is.logical(kdt[[a]]) | is.numeric(kdt[[a]])) {
      if (is.logical(kdt[[a]])) kdt[[a]] <- as.numeric(kdt[[a]]) # convert logical variable to numeric

      kdt[[a]] = cut(kdt[[a]], unique(c(-Inf, binsx[, as.numeric(sub("^\\[(.*),.+", "\\1", bin))], Inf)), right = FALSE, dig.lab = 10, ordered_result = FALSE)

      # return
      kdt <- setnames(
        binsx[,.(bin, woe)], c(a, paste0(a, "_woe"))
      )[kdt, on = a
      ][, (a) := NULL]

    }

    kdt[[paste0(a, "_woe")]] <- ifelse(is.na(kdt[[paste0(a, "_woe")]]), na_woe,  kdt[[paste0(a, "_woe")]])

  }

  return(kdt)
}

#' WOE Binning Visualization
#'
#' \code{woebin_plot} create plots of count distribution and bad probability for each bin. The binning informations are generates by  \code{woebin}.
#'
#' @name woebin_plot
#' @param bins A list or data frame. Binning information generated by \code{woebin}.
#' @param x Name of x variables. Default NULL
#' @param title String added to the front of plot title, default "".
#' @return List of binning plot
#'
#' @seealso  \code{\link{woebin}}, \code{\link{woebin_ply}}
#'
#' @examples
#' # Load German credit data
#' data(germancredit)
#'
#' # Example I
#' dt1 <- germancredit[, c("creditability", "credit.amount")]
#'
#' bins1 <- woebin(dt1, y="creditability")
#' p1 <- woebin_plot(bins1)
#'
#' \dontrun{
#' # Example II
#' bins <- woebin(germancredit, y="creditability")
#' plotlist <- woebin_plot(bins)
#'
#' # # save binning plot
#' # for (i in 1:length(plotlist)) {
#' #   ggplot2::ggsave(
#' #      paste0("./woebinplot/", names(plotlist[i]), ".png"), plotlist[[i]],
#' #      width = 15, height = 9, units="cm" )
#' #   }
#' }
#'
#' @import data.table ggplot2
#' @export
#'
woebin_plot <- function(bins, x=NULL, title="") {
  variable = NULL # no visible binding for global variable

  # plot function
  pf <- function(bin, title) {
    . = variable = count = count_distr = good = bad = badprob = woe = goodbad = value = count_num = badprob2 = count_distr2 = NULL # no visible binding for global variable

    # data
    ## y_right_max
    y_right_max <- ceiling(max(bin$badprob, na.rm=T)*10)
    if (y_right_max %% 2 ==1) y_right_max=y_right_max+1
    if (y_right_max - max(bin$badprob, na.rm=T)*10 <= 0.3) y_right_max = y_right_max+2
    y_right_max <- y_right_max/10
    if (y_right_max>1 || y_right_max<=0 || is.na(y_right_max) || is.null(y_right_max)) y_right_max=1

    ## y_left_max
    y_left_max <- ceiling(max(bin$count_distr, na.rm=T)*10)/10
    if (y_left_max>1 || y_left_max<=0 || is.na(y_left_max) || is.null(y_left_max)) y_left_max=1


    ## data set
    dat <- setDT(bin)[,.(
      variable, bin, count_num=count, count_distr2=count_distr, count_distr, good=good/sum(count), bad=bad/sum(count), badprob, woe
    )][, `:=`(
      bin = ifelse(is.na(bin), "NA", bin),
      badprob2 = badprob*(y_left_max/y_right_max),
      badprob = round(badprob,4),
      rowid = .I
    )][, bin := factor(bin, levels = bin)]

    dat_melt <- melt(dat, id.vars = c("variable", "bin","rowid"), measure.vars =c("good", "bad"), variable.name = "goodbad")[
      ,goodbad:=factor(goodbad, levels=c( "bad", "good"))
      ]

    # title
    if (title != "" & !is.na(title)) title <- paste0(title, "-")

    # plot
    ggplot() +
      geom_bar(data=dat_melt, aes(x=bin, y=value, fill=goodbad), stat="identity") +
      geom_text(data=dat, aes(x = bin, y = count_distr2, label = paste0(round(count_distr2*100, 1), "%, ", count_num) ), vjust = 0.5) +
      geom_line(data=dat, aes(x = rowid, y = badprob2), colour = "blue") +
      geom_point(data=dat, aes(x = rowid, y=badprob2), colour = "blue", shape=21, fill="white") +
      geom_text(data=dat, aes(x = rowid, y = badprob2, label = paste0(round(badprob*100, 1), "%")), colour="blue", vjust = -0.5) +
      scale_y_continuous(limits = c(0,y_left_max), sec.axis = sec_axis(~./(y_left_max/y_right_max), name = "Bad probability")) +
      labs(title = paste0(title, dat[1, variable]), x=NULL, y="Bin count distribution", fill=NULL) +
      theme_bw() +
      theme(
        legend.position="bottom", legend.direction="horizontal",
        axis.title.y.right = element_text(colour = "blue"),
        axis.text.y.right  = element_text(colour = "blue",angle=90, hjust = 0.5),
        axis.text.y = element_text(angle=90, hjust = 0.5) )

  }

  # converting data.frame into list
  bins_list <- list()
  if (is.data.frame(bins)) {
    bins_dt <- setDT(bins)
    x <- bins_dt[1, unique(variable)]

    bins <- list()
    for (i in x) {
      bins[[i]] <- bins_dt[variable == i]
    }

    # bins <- eval(parse(text = paste0("list(", setDT(bins)[1, variable], " = bins)")))
  }
  # x variable names
  if (is.null(x) || anyNA(x)) x = names(bins)

  # plot export
  plotlist <- list()
  for (i in x) plotlist[[i]] <- pf(bins[[i]], title)


  return(plotlist)
}

