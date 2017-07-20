# woebin2
# This function provides woe binning for only two columns (one x and one y) dataframe.
woebin2 <- function(dt, y, x="", binnum=5, method="tree", min_perc_total=0.02, positive="bad|1", print_step = FALSE) {
  # # load germancredit data
  # data(germancredit)
  # dt <- data.table(germancredit[, c('creditability', 'credit.amount')])
  # dt <- data.table(germancredit[, c('creditability', 'present.employment.since')])

  # x column name
  if (x=="") x <- setdiff(names(dt), y)
  if (length(x) > 1) break


  # input data.table
  dtm <- data.table(y = dt[[y]], variable=x, value = dt[[x]])[, y := ifelse(grepl(positive, y), 1, 0)]
  if (is.logical(dtm[,value])) dtm[, value := as.numeric(value)]

  # functions ------
  ###### breakpoints for initial bins
  breakpoints <- function(dtm, min_perc_total) {
    if ( is.numeric(dtm[,value]) | is.logical(dtm[,value]) ) {
      xvec <- dtm$value

      # breakpoints vector
      iq <- quantile(xvec, na.rm = TRUE)
      iqr <- IQR(xvec, na.rm = TRUE)
      if (iqr == 0) {
        xvec_rm_outlier <- xvec
      } else {
        xvec_rm_outlier <- xvec[which(xvec > iq[2]-3*iqr & xvec <= iq[4]+3*iqr)]
      }

      # number of initial bins
      n <- trunc(1/min_perc_total)
      if (length(unique(xvec_rm_outlier)) < n) {n <- length(unique(xvec_rm_outlier))}

      brkp <- pretty(xvec_rm_outlier, n)
      # brkp <- quantile(xvec, (0:n)/n, na.rm=TRUE)
      brkp <- unique(c(-Inf, brkp[2:(length(brkp)-1)], Inf))

      if (anyNA(xvec)) brkp <- c(brkp, NA)


      # binned datatable
      brkdt <- copy(dtm)[
        , bin := cut(value, brkp, right = FALSE, dig.lab = 10, ordered_result = TRUE)
        ][][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , keyby = bin
            ][order(bin)
              ][, `:=`(brkp = as.numeric( sub("^\\[(.*),.+", "\\1", bin)), badprob = bad/(good+bad) )
                ][, .(variable, bin, brkp, good, bad, badprob)]

    } else if ( is.factor(dtm[,value]) | is.character(dtm[,value]) ) {

      brkdt <- copy(dtm)[
        , .(variable = unique(variable), good = sum(y==0), bad = sum(y==1)), by=value
        ][, badprob := bad/(good+bad)
          ][order(badprob)
            ][,brkp := ifelse(is.na(value), NA, as.integer(row.names(.SD)))
              ][order(brkp)
                ][,brkp := ifelse(is.na(value), NA, as.integer(row.names(.SD)))
                  ][,.(variable, bin=value, brkp, good, bad, badprob)]

    }



    return(brkdt)
  }

  # add one tree-like best breakpoints
  # requried by all_bst_brkp
  add_bst_brkp <- function(brkpdt, bst_brkp = c()) {
    # datatable of iv and bst_brkp
    iv_dt <- data.table()

    # best breakpoints
    # requried by add_bst_brkp
    bestbreakpoints <- function(brkpdt, bst_brkp) {
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
    system.time(
    for (i in bst_brkp_set) {
      # best breakpoint + i
      bst_brkp_i <- sort(c(bst_brkp, i))
      # IV1 <- brkpdt[
      #   , woe := lapply(.SD, woe_01, bad), .SDcols = "good"
      #   ][, miv := lapply(.SD, miv_01, bad), .SDcols = "good"
      #     ][, total_iv := sum(miv)][]


      bst_brkpdt <- bestbreakpoints(brkpdt, bst_brkp_i)

      # rbind datatable of iv and bst_brkp with new row
      iv_dt <- rbindlist(list(
        iv_dt,
        data.table( total_iv = bst_brkpdt[,unique(total_iv)], brkp = i )
      ))
    }
    )
    # return
    # selected best breakpoint
    bst_brkp <- sort( c(bst_brkp, iv_dt[total_iv==max(total_iv), brkp]) )
    #
    bst_bins_dt <- bestbreakpoints(brkpdt, bst_brkp)

    return(list(best_breakpoints = bst_brkp, best_bins_datatable = bst_bins_dt, total_iv = bst_bins_dt[, unique(total_iv)]))
  }
  ###### all tree-like best breakpoints
  all_bst_brkp <- function(brkpdt, binnum=5, print_step=FALSE) {
    len_brkp <- length(setdiff(brkpdt[, brkp], c(-Inf, Inf, NA)))
    if ( binnum > len_brkp ) binnum <- len_brkp

    # best breakpoints for two bins
    ALL_bst_brkp <- add_bst_brkp(brkpdt)
    if (print_step == TRUE) print(ALL_bst_brkp)
    # IVt1 <- ALL_bst_brkp$total_iv

    if (binnum >= 2) {
    # best breakpoints from three to n+1 bins
    for (i in 2:binnum) {
      ALL_bst_brkp <- add_bst_brkp(brkpdt, ALL_bst_brkp$best_breakpoints)
      if (print_step == TRUE) print(ALL_bst_brkp)

      # IVt2 <- ALL_bst_brkp$total_iv
      # IVt1 <- IVt2
    }
    }

    return(ALL_bst_brkp)
  }
  # examples
  # brkpdt <- breakpoints(dtm, min_perc_total)
  # bestbreakpoints(brkpdt, 2)
  # add_bst_brkp(brkpdt)
  # all_bst_brkp(brkpdt, print_step = TRUE)

  # run functions ------
  brkpdt <- breakpoints(dtm, min_perc_total)
  if (binnum == "N") return(brkpdt)
  all_bst_brkp(brkpdt, binnum, print_step)
}

#' woe binning
#'
#' This function generates a list of binnings based on tree-like segmentation for both numeric and character variables.
#' @name woebin
#' @param dt Name of input data
#' @param y Name of y variable.
#' @param x Name vector of x variables, defaults: "".
#' @param binnum Number of binning, defaults: 6.
#' @param min_perc_total The share of initial binning class on total number.
#' @param positive Name of positive class, defaults: bad or 1.
#' @return List of binnig for each variable.
#' @export
#' @examples
#' # Load German credit data and create good and bad series
#' data(germancredit)
#' dt <- germancredit[, c('creditability', 'credit.amount', 'age.in.years', 'present.employment.since')]
#'
#' woebin(dt, y = "creditability")

woebin <- function(dt, y, x="", binnum=5, method="tree", min_perc_total=0.01, positive="bad|1", print_step = FALSE) {
  # transfer dt to data.table
  dt <- data.table(dt)
  # x variable names
  if (x=="") xnames <- setdiff(names(dt), y)

  # x_binnum data.table
  if (length(x) == length(binnum) | length(binnum) == 1) {
    x_binnum <- data.table(x = xnames, binnum = binnum)
  }

  # export the bins of all columns
  bins <- list()
  for (r in 1:nrow(x_binnum)) {
    x <- x_binnum[r, x]
    binnum <- x_binnum[r, binnum]
    print(x)

    dt2 <- dt[, c(x, y), with=FALSE]

    bin2 <- woebin2(dt, y, x, binnum, method, min_perc_total, positive, print_step)

    bins[[x]] <- bin2$best_bins_datatable
  }

  return(bins)
}

#' woe binning apply
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
#' dt <- data.table(germancredit[, c('creditability', 'age.in.years', 'present.employment.since')])
#'
#' bins <- woebin(dt, "creditability")
#' dt_woe <- woebin_ply(dt, bins, "creditability")
woebin_ply <- function(dt, bins, y) {
  kdt <- copy(dt)

  for (x in names(bins)) {
    binsx <- bins[[x]]

    if (is.factor(kdt[[x]]) | is.character(kdt[[x]])) {
      kdt <- setnames(
        woebin2(kdt[, c(y,x), with=FALSE], y, binnum = "N")[,.(bin,brkp)],
        c(x, paste0(x, "_brkp"))
      )[kdt, on=x]

      kdt[[x]] <- kdt[[paste0(x, "_brkp")]]
      kdt[, paste0(x, "_brkp") := NULL]
    }

    kdt[, (x) := cut(kdt[[x]], c(-Inf, binsx[, bstbrkp]), right = FALSE, dig.lab = 10, ordered_result = TRUE) ]

    kdt <- setnames(binsx[,.(bstbin, woe)], c(x, paste0(x,"_woe")))[kdt, on = x][,(x):=NULL]
  }

  return(kdt)
}

