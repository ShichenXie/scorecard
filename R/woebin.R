# woebin2
# This function provides woe binning for only two columns (one x and one y) dataframe.
woebin2 <- function(dt, y, x, min_perc_total=0.02, stop_limit=0.1, method="tree", positive="bad|1", print_step=FALSE) {
  # # load germancredit data
  # data(germancredit)
  # dt <- data.table(germancredit[, c('creditability', 'age.in.years')])
  # dt <- data.table(germancredit[, c('creditability', 'status.of.existing.checking.account')])
  # dt <- data.table(germancredit[, c('creditability', 'present.employment.since', 'age.in.years')])

  # x column name
  # if (x=="") x <- setdiff(names(dt), y)
  # if (length(x) > 1) break


  # input data.table
  dtm <- data.table(y = dt[[y]], variable=x, value = dt[[x]])[, y := ifelse(grepl(positive, y), 1, 0)]
  if (is.logical(dtm[,value])) dtm[, value := as.numeric(value)]

  # functions ------
  ###### breakpoints for initial bins
  breakpoints <- function(dtm, min_perc_total) {
    if ( is.numeric(dtm[,value]) | is.logical(dtm[,value]) ) {
      xvec <- dtm[, value]

      # breakpoints vector & outlier handle
      iq <- quantile(xvec, na.rm = TRUE)
      iqr <- IQR(xvec, na.rm = TRUE)
      if (iqr == 0) {
        xvec_rm_outlier <- xvec
      } else {
        xvec_rm_outlier <- xvec[which(xvec > iq[2]-3*iqr & xvec <= iq[4]+3*iqr)]
      }

      # number of initial bins
      n <- trunc(1/min_perc_total)
      if (length(unique(xvec_rm_outlier)) < n) n <- length(unique(xvec_rm_outlier))

      # initial breakpoints
      brkp <- pretty(xvec_rm_outlier, n)
      # brkp <- quantile(xvec, (0:n)/n, na.rm=TRUE)
      brkp <- unique(c(-Inf, brkp[2:(length(brkp)-1)], Inf))
      if (anyNA(xvec)) brkp <- c(brkp, NA)


      # binned datatable
      brkdt <- copy(dtm)[
        , bin := cut(value, brkp, right = FALSE, dig.lab = 10, ordered_result = TRUE)
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
  add_bst_brkp <- function(brkpdt, bst_brkp = c()) {

    # best breakpoints
    bestbreakpoints <- function(brkpdt, bst_brkp, only_total_iv=TRUE) {
      if ( is.numeric(dtm[,value]) | is.logical(dtm[,value]) ) {
        brkpdt[
          , bstbin := cut(brkp, c(-Inf, bst_brkp, Inf), right = FALSE, dig.lab = 10, ordered_result = TRUE)
          ][, .(good = sum(good), bad = sum(bad), variable=unique(variable)) , keyby = bstbin
          ][, `:=`(badprob = bad/(good+bad), bin = bstbin )
          ][order(bstbin)
          ][, woe := lapply(.SD, woe_01, bad), .SDcols = "good"
          ][, miv := lapply(.SD, miv_01, bad), .SDcols = "good"
          ][, total_iv := sum(miv)
          ][, bstbrkp := as.numeric( sub("^\\[(.*),.+", "\\1", bstbin) )
          ][, .(variable, bin, bstbin, bstbrkp, good, bad, badprob, woe, miv, total_iv)]


      } else if ( is.factor(dtm[,value]) | is.character(dtm[,value]) ) {
        bst_brkp <- setdiff(bst_brkp, min(brkpdt[,brkp]))

        brkpdt[
          , bstbin := cut(brkp, c(-Inf, bst_brkp, Inf), right = FALSE,dig.lab = 10, ordered_result = TRUE)
          ][, .(variable=unique(variable), bin = paste0(bin, collapse = "##"), good = sum(good), bad = sum(bad)), keyby = bstbin
          ][, badprob:=bad/(good+bad)
          ][order(bstbin)
          ][, woe := lapply(.SD, woe_01, bad), .SDcols = "good"
          ][, miv := lapply(.SD, miv_01, bad), .SDcols = "good"
          ][, total_iv := sum(miv)
          ][, bstbrkp := as.numeric( sub("^\\[(.*),.+", "\\1", bstbin) )
          ][, .(variable, bin, bstbin, bstbrkp, good, bad, badprob, woe, miv, total_iv) ]
        # variable bstbin bstbrkp good bad badprob woe miv total_iv

      }


    }

    # best breakpoints set
    bst_brkp_set <- setdiff( brkpdt[,brkp], c(bst_brkp, -Inf, Inf, NA) )

    # loop on bst_brkp_set
    for (i in bst_brkp_set) {
      # best breakpoint + i
      bst_brkp_i <- sort(c(bst_brkp, i))

      # best breakpoint datatable
      bst_brkpdt <- brkpdt[
        , paste0("bstbin",i) := cut(brkp, c(-Inf, bst_brkp_i, Inf), right = FALSE, dig.lab = 10, ordered_result = TRUE)
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
      bestbreakpoints(brkpdt, bst_brkp, only_total_iv=FALSE)

    # return(list(best_breakpoints = bst_brkp, best_bins_datatable = bst_bins_dt, total_iv = bst_bins_dt[1, total_iv]))
  }
  ###### all tree-like best breakpoints
  all_bst_brkp <- function(brkpdt, stop_limit=0.1, print_step=FALSE) {
    len_brkp <- length( setdiff(brkpdt[, brkp], c(-Inf, Inf, NA)) )

    # best breakpoints for two bins
    ALL_bst_brkp <- add_bst_brkp(brkpdt)
    if (print_step == TRUE) print(ALL_bst_brkp)
    IVt1 <- ALL_bst_brkp[1, total_iv]

    len_step = 1
    if (len_brkp >= 2) {
      # initial information value gain ratio
      IVchg <- 1
    # best breakpoints from three to n+1 bins
    while (IVchg >= stop_limit) {
      ALL_bst_brkp <- add_bst_brkp(brkpdt, ALL_bst_brkp[bstbrkp != -Inf, bstbrkp])
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
  # system.time( brkpdt <- breakpoints(dtm, min_perc_total) )
  # system.time(add_bst_brkp(brkpdt))
  # system.time(all_bst_brkp(brkpdt, print_step = TRUE))

  # run functions ------
  brkpdt <- breakpoints(dtm, min_perc_total)
  if (stop_limit == "N") return(brkpdt)
  all_bst_brkp(brkpdt, stop_limit, print_step)
}

#' woe binning
#'
#' This function generates a list of binnings based on tree-like segmentation for both numeric and character variables.
#' @name woebin
#' @param dt Name of input data
#' @param y Name of y variable.
#' @param x Name vector of x variables, defaults: "".
#' @param min_perc_total The share of initial binning class number over total. Accepted range: 0.01-0.2; default: 0.02.
#' @param stop_limit Stop binning segmentation when information value gain ratio less than the stop_limit. Accepted range: 0-0.5; default: 0.1.
#' @param positive Name of positive class, defaults: bad or 1.
#' @return List of binnig for each variable.
#' @export
#' @examples
#' # Load German credit data and create good and bad series
#' data(germancredit)
#' dt <- germancredit[, c('creditability', 'credit.amount', 'age.in.years', 'present.employment.since')]
#'
#' # woe binning
#' woebin(dt, y = "creditability")
#'
#' # set stop_limit for each x variable
#' woebin(dt, y = "creditability", stop_limit = c(0.05, 0.1, 0.01))

woebin <- function(dt, y, x="", min_perc_total=0.02, stop_limit=0.1, method="tree", positive="bad|1", print_step = FALSE) {

  # transfer dt to data.table
  dt <- data.table(dt)
  # x variable names vector
  if (length(x)==1 & ""%in%x) x <- setdiff(names(dt), y)

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
    if (print_step) print(x_i)

    # woebining on one variable
    bin2 <- woebin2(dt[, c(x_i, y), with=FALSE], y, x_i, min_perc_total, stop_limit_i, method, positive)

    # renmae NA as missing
    bins[[x_i]] <- bin2[, bin := ifelse(is.na(bin), "missing", as.character(bin))][]
  }

  # total_iv list
  total_iv_list <- unique(rbindlist(bins)[, .(variable, total_iv)])[order(-total_iv)]

  # reorder bins by iv
  bins_list <- list()
  for (v in total_iv_list$variable) {
    bins_list[[v]] <- bins[[v]]
  }

  return(list(bins = bins_list, iv=total_iv_list))
}

#' binning apply
#'
#' This function applies binning via woebin to dataframe
#'
#' @param dt Name of input data
#' @param bins Binning information generated from woebin function
#' @param y Name of y variable
#' @return List of binnig for each variable.
#' @export
#' @examples
#'
#' # load germancredit data
#' data(germancredit)
#' dt <- germancredit[, c('creditability', 'age.in.years', 'present.employment.since')]
#'
#' # woe binning
#' wb <- woebin(dt, "creditability")
#' # woe binning apply
#' dt_woe <- woebin_ply(dt, wb$bins, "creditability")
woebin_ply <- function(dt, bins, y) {
  kdt <- copy(data.table(dt))

  for (x in names(bins)) {
    print(x)
    binsx <- bins[[x]]

    if (is.factor(kdt[[x]]) | is.character(kdt[[x]])) {
      kdt <- setnames(
        woebin2(kdt[, c(y,x), with=FALSE], y, x, stop_limit = "N")[,.(bin,brkp)],
        c(x, paste0(x, "_brkp"))
      )[][kdt, on=x]

      kdt[[x]] <- kdt[[paste0(x, "_brkp")]]
      kdt[, paste0(x, "_brkp") := NULL]
    } else if (is.logical(kdt[[x]])) {
      kdt[[x]] <- as.numeric(kdt[[x]])
    }

    kdt[[x]] = cut(kdt[[x]], unique(c(-Inf, binsx[, bstbrkp], Inf)), right = FALSE, dig.lab = 10, ordered_result = TRUE)

    kdt <- setnames(
      binsx[,.(bstbin, woe)], c(x, paste0(x,"_woe"))
    )[kdt, on = x][,(x):=NULL][]
  }

  return(kdt)
}

#' binning visualization
#'
#' This function visualizes the binning results generated via \code{\line{woebin}}
#' @name woebin_plot
#' @param bins binning generated via \code{\line{woebin}}
#' @export
#' @examples
#' data(germancredit)
#' bins <- woebin(germancredit, y="creditability")$bins
#'
#' plotlist <- woebin_plot(bins)
#' plotlist
#'
woebin_plot <- function(bins) {

  pf <- function(bin) {
    # data
    dat <- bin[,.(
      variable, bin, good, bad, counts=good+bad, badprob, woe
    )][, `:=`(
      bin = ifelse(is.na(bin), "NA", bin),
      badprob2 = badprob*max(counts),
      badprob = round(badprob,4),
      rowid = as.integer(row.names(.SD))
    )][, bin := factor(bin, levels = bin)]

    dat_melt <- melt(dat, id.vars = c("variable", "bin","rowid"), measure.vars =c("good", "bad"), variable.name = "goodbad")[
      ,goodbad:=factor(goodbad, levels=c( "bad", "good"))
      ]

    # plot
    ggplot() +
      geom_bar(data=dat_melt, aes(x=bin, y=value, fill=goodbad), stat="identity") +
      geom_text(data=dat, aes(x = bin, y = counts, label = counts), vjust = -0.5) +
      geom_line(data=dat, aes(x = rowid, y = badprob2), colour = "blue") +
      geom_point(data=dat, aes(x = rowid, y=badprob2), colour = "blue", shape=21, fill="white") +
      geom_text(data=dat, aes(x = rowid, y = badprob2, label = badprob), colour="blue", vjust = -0.5) +
      scale_y_continuous(sec.axis = sec_axis(~./max(dat$counts), name = "Bad probability")) +
      labs(title = dat[1, variable], x=NULL, y="Bin count", fill=NULL) +
      theme_bw() +
      theme(legend.position="bottom", legend.direction="horizontal")

  }

  # plot export
  plotlist <- list()
  if (!is.data.frame(bins) & is.list(bins)) {
    bins_length <- length(bins)
    for (i in 1:bins_length) plotlist[[bins[[i]][1,variable]]] <- pf(bins[[i]])
  } else if (is.data.frame(bins)) {
    bins_length <- 1
    plotlist[[bins[1,variable]]] <- pf(bins)
  }

  return(plotlist)
}

