# woebin2
# This function provides woe binning for only two columns (one x and one y) dataframe.
woebin2 <- function(dt, y, x, breaks=NA, min_perc_total=0.02, stop_limit=0.1, positive="bad|1", print_step=FALSE) {
  # method="tree",

  # data(germancredit)
  # numerical data
  # dt <- setDT(germancredit)[, .(y=creditability, age.in.years)];
  # y="y"; x="age.in.years"

  # character data
  # dt <- setDT(germancredit)[, .(y=creditability, status.of.existing.checking.account)]
  # y="y"; x="status.of.existing.checking.account"

  # dt <- setDT(germancredit)[, .(creditability, present.employment.since, age.in.years)]


  # input data.table
  dtm <- data.table(y = dt[[y]], variable=x, value = dt[[x]])[, y := ifelse(grepl(positive, y), 1, 0)]
  # convert logical value into numeric
  if (is.logical(dtm[,value])) dtm[, value := as.numeric(value)]

  # return binning if provide breakpoints
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

    } else if ( is.factor(dtm[,value]) | is.character(dtm[,value]) ) {

      bin <- merge(
        data.table(bin = breaks, value = breaks )[
          , strsplit(as.character(value), "%,%", fixed=TRUE), by = .(bin) ][, `:=`(value = V1, V1=NULL)],
        dtm
      )[, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , keyby = bin
      ][, `:=`(badprob = bad/(good+bad) )
      ][order(badprob)
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
        ][, badprob := bad/(good+bad)
        ][order(badprob)
        ][, brkp := ifelse(is.na(value), NA, as.integer(row.names(.SD)))
        ][order(brkp)
        ][, brkp := ifelse(is.na(value), NA, as.integer(row.names(.SD)))
        ][, .(variable, bin=value, brkp, good, bad, badprob)]

    }

    return(brkdt)
  }

  # add one tree-like best breakpoints
  # requried by all_bst_brkp
  add_bst_brkp <- function(initial_brkpdt, bst_brkp = c()) {

    # best breakpoints
    bestbreakpoints <- function(initial_brkpdt, bst_brkp, only_total_iv=TRUE) {
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
  all_bst_brkp <- function(initial_brkpdt, stop_limit=0.1, print_step=FALSE) {
    len_brkp <- length( setdiff(initial_brkpdt[, brkp], c(-Inf, Inf, NA)) )

    # best breakpoints for two bins
    ALL_bst_brkp <- add_bst_brkp(initial_brkpdt)
    if (print_step == TRUE) print(ALL_bst_brkp)
    IVt1 <- ALL_bst_brkp[1, total_iv]

    len_step = 1
    if (len_brkp >= 2) {
      # initial information value gain ratio
      IVchg <- 1
    # best breakpoints from three to n+1 bins
    while (IVchg >= stop_limit) {
      ALL_bst_brkp <- add_bst_brkp(initial_brkpdt, ALL_bst_brkp[bstbrkp != -Inf, bstbrkp])
      if (print_step == TRUE) print(ALL_bst_brkp)

      IVt2 <- ALL_bst_brkp[1, total_iv]
      # information value gain ratio
      IVchg <- IVt2/IVt1-1
      # print(IVchg)
      IVt1 <- IVt2

      len_step = len_step + 1
      if (len_step >= len_brkp) break
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
  all_bst_brkp(initial_brkpdt, stop_limit, print_step)
}

#' woe binning
#'
#' \code{woebin} generates optimal binning for both numerical and character variables using tree-like segmentation.
#'
#' @name woebin
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name vector of x variables. Default NA. If x is NA, all variables exclude y will counted as x variables.
#' @param breaks_list List of break points, defaults NA. If it is not NA,  variable binning will based on the provided breaks.
#' @param min_perc_total The share of initial binning class number over total. Accepted range: 0.01-0.2; default 0.02.
#' @param stop_limit Stop binning segmentation when information value gain ratio less than the stop_limit. Accepted range: 0-0.5; default 0.1.
#' @param positive Value of positive class, default "bad|1".
#' @param print_step Logical. If it is TRUE, print the variable name  when generate binning.
#' @return List of binning information for each variable.
#'
#' @examples
#' # load germancredit data
#' data(germancredit)
#'
#' dt <- germancredit[, c("creditability", "age.in.years",
#'       "credit.amount", "housing", "purpose")]
#'
#' # set stop_limit (infovalue grain ratio) for each x variable
#' bins <- woebin(dt, y = "creditability", stop_limit = c(0.05, 0.1, 0.01, 0.1))
#' bins
#'
#'
#' # set binning breakpoints manually
#' breaks_adj <- list(
#'   age.in.years = c(25, 35, 40, 60),
#'   credit.amount = NULL,
#'   housing = c("own", "for free%,%rent"),
#'   purpose = NULL
#' )
#'
#' bins_adj <- woebin(dt, y="creditability", breaks_list=breaks_adj)
#' bins_adj
#'
#' @import data.table
#' @importFrom stats IQR quantile
#' @export
#'
woebin <- function(dt, y, x=NA, breaks_list=NA, min_perc_total=0.02, stop_limit=0.1, positive="bad|1", print_step = FALSE) {
  # method="tree",

  # transfer dt to data.table
  dt <- setDT(dt)
  dt[dt==""] <- NA
  # x variable names vector
  if (anyNA(x)) x <- setdiff(names(dt), y)

  # breaks_list
  if (!anyNA(breaks_list)) {
    if (!is.list(breaks_list)) {
      break
    } else if (length(breaks_list) != length(x)) {
      break
    }
  }

  # stop_limit vector
  if (length(stop_limit) == 1) {
    stop_limit = rep(stop_limit, length(x))
  } else if (length(stop_limit) != length(x)) {
    break
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




  # export the bins of all columns
  bins <- list()
  for (i in 1:length(x)) {
    x_i <- x[i]
    stop_limit_i <- stop_limit[i]

    # print x
    if (length(unique(dt[[x_i]])) > 1) {
      if (print_step) print(x_i)
    } else {
      print(paste0(x_i, "------next"))
      next

    }


    # woebining on one variable
    if (anyNA(breaks_list)) { # is.na(breaks_list)
      bin2 <- woebin2(dt[, c(x_i, y), with=FALSE], y, x_i, min_perc_total=min_perc_total, stop_limit=stop_limit_i, positive=positive)
    } else { # breaks_list provided
      bin2 <- woebin2(dt[, c(x_i, y), with=FALSE], y, x_i, breaks = breaks_list[[x_i]], positive=positive)
    }


    # renmae NA as missing
    bins[[x_i]] <- bin2[, bin := ifelse(is.na(bin) | bin=="NA", "missing", as.character(bin))]
  }

  # total_iv list
  total_iv_list <- unique(rbindlist(bins)[, .(variable, total_iv)])[order(-total_iv)]

  # reorder bins by iv
  bins_list <- list()
  for (v in total_iv_list$variable) {
    bins_list[[v]] <- bins[[v]][,.(variable, bin, count=good+bad,  count_distr=(good+bad)/(sum(good)+sum(bad)), good, bad, badprob, woe, bin_iv, total_iv)]
  }

  # return(list(bins = bins_list, iv=total_iv_list))
  return(bins_list)
}

#' woe binning apply
#'
#' \code{woebin_ply} converts original input data to woe values based on the binning information generated by \code{woebin}.
#'
#' @param dt A data frame.
#' @param bins Binning information generated by \code{woebin}.
#' @return WOE value for each variable.
#'
#' @examples
#' # load germancredit data
#' data(germancredit)
#'
#' dt <- germancredit[, c("creditability", "age.in.years",
#'       "credit.amount", "housing", "purpose")]
#'
#' # binning
#' bins <- woebin(dt, y = "creditability")
#'
#' dt_woe <- woebin_ply(dt, bins=bins)
#'
#' @import data.table
#' @export
#'
woebin_ply <- function(dt, bins) { # dt, y, x=NA, bins
  kdt <- copy(setDT(dt))
  kdt[kdt==""] <- NA

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
  for (a in x) {
    print(a)
    binsx <- bins[variable==a] #bins[[a]]
    na_woe <- binsx[bin == "missing", woe]

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

#' binning visualization
#'
#' \code{woebin_plot} create plots of count distribution and bad probability for each bin. The binning informations are generates by  \code{woebin}.
#'
#' @name woebin_plot
#' @param bins Binning information generated by \code{woebin}.
#' @param x Name vector of x variables. Default NA.
#' @param title String added to the front of plot title, default "".
#' @return A list of ggplot objects.
#'
#' @examples
#' data(germancredit)
#' bins <- woebin(germancredit, y="creditability")
#'
#' plotlist <- woebin_plot(bins)
#' plotlist
#'
#' @import data.table ggplot2
#' @export
#'
woebin_plot <- function(bins, x=NULL, title="") {

  pf <- function(bin, title) {
    # data
    dat <- bin[,.(
      variable, bin, count_num=count, count=count/sum(count), count_distr, good=good/sum(count), bad=bad/sum(count), badprob, woe
    )][, `:=`(
      bin = ifelse(is.na(bin), "NA", bin),
      badprob2 = badprob*max(count),
      badprob = round(badprob,4),
      rowid = as.integer(row.names(.SD))
    )][, bin := factor(bin, levels = bin)]

    dat_melt <- melt(dat, id.vars = c("variable", "bin","rowid"), measure.vars =c("good", "bad"), variable.name = "goodbad")[
      ,goodbad:=factor(goodbad, levels=c( "bad", "good"))
      ]

    # title
    if (title != "" & !is.na(title)) title <- paste0(title, "-")

    # plot
    ggplot() +
      geom_bar(data=dat_melt, aes(x=bin, y=value, fill=goodbad), stat="identity") +
      geom_text(data=dat, aes(x = bin, y = count, label = paste0(round(count*100, 1), "%, ", count_num) ), vjust = 0.5) +
      geom_line(data=dat, aes(x = rowid, y = badprob2), colour = "blue") +
      geom_point(data=dat, aes(x = rowid, y=badprob2), colour = "blue", shape=21, fill="white") +
      geom_text(data=dat, aes(x = rowid, y = badprob2, label = paste0(round(badprob*100, 1), "%")), colour="blue", vjust = -0.5) +
      scale_y_continuous(sec.axis = sec_axis(~./max(dat$count), name = "Bad probability")) +
      labs(title = paste0(title, dat[1, variable]), x=NULL, y="Bin count distribution", fill=NULL) +
      theme_bw() +
      theme(legend.position="bottom", legend.direction="horizontal")

  }

  if (is.data.frame(bins)) {
    bins <- eval(parse(text = paste0("list(", bins[1, variable], " = bins)")))
  }
  if (is.null(x) | anyNA(x)) x = names(bins)

  # plot export
  plotlist <- list()
  for (i in x) plotlist[[i]] <- pf(bins[[i]], title)


  return(plotlist)
}

