# woebin woebin_plot woebin_ply woebin_adj

# converting vector (breaks & special_values) to data frame
split_vec_todf = function(vec) {
  value = . = bin_chr = V1 = NULL

  if (!is.null(vec)) data.table(
    value=vec, bin_chr=vec
  )[, rowid := .I
  ][, strsplit(as.character(value), "%,%", fixed=TRUE), by = .(rowid, bin_chr)
  ][, .(rowid, bin_chr, value = ifelse(V1=="missing", NA, as.character(V1)) )]
}
# add missing to spl_val if there is na in dtm$value and
# missing is not specified in breaks and spl_val
add_missing_spl_val = function(dtm, breaks, spl_val) {
  value = NULL

  if (dtm[,any(is.na(value))]) {
    no_missing = !any(grepl('missing', c(breaks, spl_val)))
    if (no_missing) {
      spl_val = c('missing',spl_val)
    }
  }
  return(spl_val)
}
# split dtm into bin_sv and dtm (without speical_values)
dtm_binning_sv = function(dtm, breaks, spl_val) {
  binning_sv = value = . = y = variable = good = bad = bin = NULL
  # spl_val
  spl_val = add_missing_spl_val(dtm, breaks, spl_val)
  if (!is.null(spl_val)) {
    # special_values from vector to data frame
    sv_df = split_vec_todf(spl_val)

    # dtm_sv & dtm
    dtm_sv = setDT(dtm)[value %in% sv_df$value]
    dtm = setDT(dtm)[!(value %in% sv_df$value)]

    # if (nrow(dtm_sv) == 0) return(list(binning_sv=NULL, dtm=dtm))
    # binning_sv
    binning_sv = merge(
      dtm_sv[, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , by = value][,value:=as.character(value)],
      sv_df[,value:=as.character(value)],
      all.x = TRUE, by='value'
    )[, value:=ifelse(is.na(value), "missing", as.character(value))
    ][, .(bin=paste0(value,collapse="%,%"), good=sum(good), bad=sum(bad), variable=unique(variable)), by=rowid
    ][, .(variable, bin, good, bad)]
  }

  return(list(binning_sv=binning_sv, dtm=dtm))
}

# check empty bins for unmeric variable
check_empty_bins = function(dtm, binning) {
  . = bin = value = variable = y = NULL
  # check empty bins
  ## break points from bin
  breaks_list = lapply(
    list(left="\\1", right="\\2"),
    function(x) setdiff(sub("^\\[(.*), *(.*)\\)", x, unique(binning$bin)), c("Inf","-Inf")) )
  ## if there are empty bins
  if (!setequal(breaks_list$left, breaks_list$right)) {
    bstbrks = unique(c(-Inf, unique(breaks_list$right), Inf))
    binning = dtm[
      , bin := cut(value, bstbrks, right = FALSE, dig.lab = 10, ordered_result = FALSE)
    ][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , by = .(bin)
    ][order(bin)]

    # warning( paste0("The break points are modified into \'", paste0(breaks_list$right, collapse = ", "), "\'. There are empty bins based on the provided break points." ) )
  }
  return(binning)
}

# check zero in good bad, remove bins that have zeros in good or bad column
check_zero_goodbad = function(dtm, binning, count_distr_limit = NULL) {
  brkp = good = bad = count = merge_tolead = count_lag = count_lead = brkp2 = . = variable = bin = badprob = value = NULL

  while (binning[!is.na(brkp)][good==0 | bad==0,.N] > 0) {
    # brkp needs to be removed if good==0 or bad==0
    rm_brkp = binning[!is.na(brkp)][
      ,count := good+bad
      ][,`:=`(
        count_lag=shift(count,type="lag", fill=nrow(dtm)+1),
        count_lead=shift(count,type="lead", fill=nrow(dtm)+1)
      )][, merge_tolead := count_lag > count_lead
         ][good == 0 | bad == 0][count == min(count)]

    # set brkp to lead's or lag's
    shift_type = ifelse(rm_brkp[1,merge_tolead], 'lead', 'lag')
    binning = binning[
      ,brkp2 := shift(brkp,type=shift_type)
      ][brkp == rm_brkp[1,brkp], brkp := brkp2]

    # groupby brkp
    binning = binning[
      ,.(variable=unique(variable), bin=paste0(bin, collapse = "%,%"), good=sum(good), bad=sum(bad)), by=brkp
      ][, badprob:=bad/(good+bad)]
  }

  # format bin
  if (is.numeric(dtm[,value])) {
    binning = binning[
      grepl("%,%",bin), bin := sub("^(\\[.+?,).+,(.+?\\))$", "\\1\\2", bin)
      ][bin == 'missing', brkp := NA
      ][bin != 'missing', brkp := as.numeric(sub("^\\[(.*),.+", "\\1", bin))]
  }
  return(binning)
}

# check count distri, remove bins that count_distribution rate less than count_distr_limit
check_count_distri = function(dtm, binning, count_distr_limit) {
  count_distr = count = good = bad = brkp = merge_tolead = count_lag = count_lead = brkp2 = . = variable = bin = value = NULL
  if (!('count' %in% names(binning))) binning[, count := good + bad]

  binning[, count_distr := (count)/sum(count)]
  while (binning[!is.na(brkp)][count_distr<count_distr_limit,.N] > 0) {
    # brkp needs to be removed if good==0 or bad==0
    rm_brkp = binning[!is.na(brkp)][
      ,count_distr := (count)/sum(count)
      ][,`:=`(
        count_lag=shift(count_distr,type="lag", fill=nrow(dtm)+1),
        count_lead=shift(count_distr,type="lead", fill=nrow(dtm)+1)
      )][, merge_tolead := count_lag > count_lead
         ][count_distr<count_distr_limit][count_distr == min(count_distr)]

    # set brkp to lead's or lag's
    shift_type = ifelse(rm_brkp[1,merge_tolead], 'lead', 'lag')
    binning = binning[
      ,brkp2 := shift(brkp,type=shift_type)
      ][brkp == rm_brkp[1,brkp], brkp := brkp2]

    # groupby brkp
    binning = binning[
      ,.(variable=unique(variable), bin=paste0(bin, collapse = "%,%"), count=sum(count), good=sum(good), bad=sum(bad)), by=brkp
      ][, count_distr := (count)/sum(count)]
  }

  # format bin
  if (is.numeric(dtm[,value])) {
    binning = binning[
      grepl("%,%",bin), bin := sub("^(\\[.+?,).+,(.+?\\))$", "\\1\\2", bin)
      ][bin == 'missing', brkp := NA
        ][bin != 'missing', brkp := as.numeric(sub("^\\[(.*),.+", "\\1", bin))]
  }
  return(binning)
}


# required in woebin2 # return binning if breaks provided
#' @import data.table
woebin2_breaks = function(dtm, breaks, spl_val) {
  # global variables or functions
  value = bin = . = y = variable = bad = good = V1 = badprob = bksv_list = bin_chr = NULL

  # breaks from vector to data frame
  bk_df = split_vec_todf(breaks)

  # dtm $ binning_sv
  dtm_binsv_list = dtm_binning_sv(dtm, breaks, spl_val)
  dtm = dtm_binsv_list$dtm
  binning_sv = dtm_binsv_list$binning_sv
  if (dtm[,.N] == 0 || is.null(dtm)) return(list(binning_sv=binning_sv, binning=NULL))


  # binning
  if (is.numeric(dtm[,value])) {
    bstbrks = c(-Inf, setdiff(unique(bk_df$value), c(NA, Inf, -Inf)), Inf)

    binning = dtm[
      , bin := cut(value, bstbrks, right = FALSE, dig.lab = 10, ordered_result = FALSE)
    ][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , by = .(bin)
    ][order(bin)]
    # check empty bins
    binning = check_empty_bins(dtm, binning)


    # merge binning with bk_df
    if (bk_df[is.na(value),.N] == 1) {
      binning = merge(
        binning[, value:=sub("^\\[(.*), *(.*)\\)","\\2",bin)],
        bk_df,
        all.x = TRUE, by="value"
      )[order(rowid,value)][, bin:=ifelse(is.na(bin), "missing", as.character(bin))
      ][, .(bin=paste0(bin,collapse="%,%"), good=sum(good), bad=sum(bad), variable=unique(variable)), by=rowid
      ][order(rowid)]
    }

  } else if (is.factor(dtm[,value]) || is.character(dtm[,value])) {
    dtm = dtm[,value := as.character(value)]

    # the values not specified in breaks_list
    diff_dt_brk = setdiff(dtm[,unique(value)], bk_df[,value])
    if (length(diff_dt_brk) > 0) {
      warning(sprintf('The categorical values (`%s`) are not specified in `breaks_list` for the column `%s`.', paste0(diff_dt_brk, collapse = ', '), dtm[1,variable]) )
      stop()
    }

    # merge binning with bk_df
    binning = merge(
      dtm, bk_df[,bin:=bin_chr], all.x = TRUE
    )[order(rowid, bin)][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , by = .(rowid, bin)]

  }


  # # remove rowid column in binning data frame
  binning = binning[,rowid:=1][,rowid:=NULL]
  # # bind binning_sv and binning
  # if (setDT(binning_sv)[,.N] > 0) binning = rbind(binning_sv, binning)

  return(list(binning_sv=binning_sv, binning=binning))
}

# required in woebin2 # return initial binning
woebin2_init_bin = function(dtm, init_count_distr, breaks, spl_val) {
  # global variables or functions
  . = bad = badprob = bin = brkp = good = value = variable = y = NULL

  # dtm $ binning_sv
  dtm_binsv_list = dtm_binning_sv(dtm, breaks, spl_val)
  dtm = dtm_binsv_list$dtm
  binning_sv = dtm_binsv_list$binning_sv
  if (is.null(dtm) || dtm[,.N]==0) return(list(binning_sv=binning_sv, initial_binning=NULL))


  # binning
  if (is.numeric(dtm[,value])) {
    # numeric variable ------
    xvalue = dtm[, value]

    # breaks vector & outlier
    iq = quantile(xvalue, na.rm = TRUE)
    iqr = IQR(xvalue, na.rm = TRUE)
    if (iqr == 0) {
      xvalue_rm_outlier = xvalue
    } else {
      xvalue_rm_outlier = xvalue[which(xvalue >= iq[2]-3*iqr & xvalue <= iq[4]+3*iqr)]
    }

    # number of initial binning
    n = trunc(1/init_count_distr)
    len_uniq_x = length(setdiff(unique(xvalue_rm_outlier), c(NA,Inf,-Inf)))
    if (len_uniq_x < n) n = len_uniq_x

    # initial breaks
    if (len_uniq_x < 10) {
      brk = setdiff(unique(xvalue_rm_outlier), c(NA, Inf, -Inf))
    } else {
      brk = pretty(xvalue_rm_outlier, n)
    }
    brk = sort(brk[(brk <= max(xvalue, na.rm =TRUE)) & (brk > min(xvalue, na.rm =TRUE))])
    brk = unique(c(-Inf, brk, Inf))
    if (anyNA(xvalue)) brk = c(brk, NA)

    # initial binning datatable
    init_bin = dtm[
      , bin := cut(value, brk, right = FALSE, dig.lab = 10, ordered_result = FALSE)
    ][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , by = bin
    ][order(bin)]
    # check empty bins
    init_bin = check_empty_bins(dtm, init_bin)

    init_bin = init_bin[
      , `:=`(brkp = as.numeric( sub("^\\[(.*),.+", "\\1", bin)), badprob = bad/(good+bad))
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

  # remove brkp that good == 0 or bad == 0 ------
  init_bin = check_zero_goodbad(dtm, init_bin)
  return(list(binning_sv=binning_sv, initial_binning=init_bin))
}

# required in woebin2_tree # add 1 best break for tree-like binning
woebin2_tree_add_1brkp = function(dtm, initial_binning, count_distr_limit, bestbreaks=NULL) {
  # global variables or functions
  brkp = patterns = . = good = bad = variable = count_distr = value = min_count_distr = bstbin = min_count_distr = total_iv = bstbin = brkp = bin = NULL


  # total_iv for all best breaks
  total_iv_all_breaks = function(initial_binning, bestbreaks, dtm_rows) {
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
      , by=.(bstbin, value)
    ][, count_distr := (good+bad)/dtm_rows, by=bstbin
    ][!is.na(value), min_count_distr := min(count_distr), by=bstbin
    ][, .(total_iv = iv_01(good, bad), variable = unique(variable), min_count_distr = min(min_count_distr,na.rm=TRUE)), by=bstbin
    ][, bstbin := as.numeric(sub("bstbin(.+)", "\\1", bstbin))][]

    return(total_iv_all_brks)
  }
  # binning add 1best break
  binning_add_1bst = function(initial_binning, bestbreaks) {
    value = bstbin = . = good = bad = variable = woe = bin_iv = total_iv = bstbrkp = badprob = NULL # no visible binding for global variable

    if ( is.numeric(dtm[,value]) ) {
      binning_1bst_brk = initial_binning[
        , bstbin := cut(brkp, c(-Inf, bestbreaks, Inf), right = FALSE, dig.lab = 10, ordered_result = FALSE)
        ][, .(variable=unique(variable), bin=unique(bstbin), good = sum(good), bad = sum(bad)) , by = bstbin
          ]

    } else if (is.logical(dtm[,value]) || is.factor(dtm[,value]) || is.character(dtm[,value]) ) {

      bestbreaks = setdiff(bestbreaks, min(initial_binning[,brkp]))

      binning_1bst_brk = initial_binning[
        , bstbin := cut(brkp, c(-Inf, bestbreaks, Inf), right = FALSE,dig.lab = 10, ordered_result = FALSE)
        ][, .(variable=unique(variable), bin = paste0(bin, collapse = "%,%"), good = sum(good), bad = sum(bad)), by = bstbin ]
    }

    binning_1bst_brk = binning_1bst_brk[
      order(bstbin)
      ][, total_iv := iv_01(good, bad)
        ][, bstbrkp := as.numeric( sub("^\\[(.*),.+", "\\1", bstbin) )
          ][, .(variable, bin, bstbin, bstbrkp, good, bad, total_iv)]

    return(binning_1bst_brk)
  }


  # adding 1 best breakpoint
  dtm_rows = nrow(dtm)
  total_iv_all_brks = total_iv_all_breaks(initial_binning, bestbreaks, dtm_rows)

  # bestbreaks: total_iv == max(total_iv) & min(count_distr) >= count_distr_limit
  bstbrk_max_iv = total_iv_all_brks[min_count_distr >= count_distr_limit][total_iv==max(total_iv)][, bstbin]
  # add 1best break to bestbreaks
  bestbreaks = unique(c(bestbreaks, bstbrk_max_iv[1]))
  bin_add_1bst = binning_add_1bst(initial_binning, bestbreaks)

  return(bin_add_1bst)
}

# required in woebin2 # return tree-like binning
woebin2_tree = function(
  dtm,
  init_count_distr  = 0.02,
  count_distr_limit = 0.05,
  stop_limit        = 0.1,
  bin_num_limit     = 8,
  breaks            = NULL,
  spl_val           = NULL
) {
  # global variables or functions
  brkp = bstbrkp = total_iv = NULL

  # initial binning
  bin_list = woebin2_init_bin(dtm, init_count_distr=init_count_distr, breaks=breaks, spl_val=spl_val)
  initial_binning = bin_list$initial_binning
  binning_sv = bin_list$binning_sv

  if (nrow(initial_binning)<=1 || is.null(initial_binning)) {
    return(list(binning_sv=binning_sv, binning=initial_binning))
  }
  # initialize parameters
  ## length all breaks
  len_brks = initial_binning[!is.na(brkp), .N]
  ## param
  bestbreaks = NULL ## best breaks
  IVt1 = IVt2 = 1e-10
  IVchg = 1 ## IV gain ratio
  step_num = 1

  # best breaks from three to n+1 bins
  binning_tree = NULL
  while ( (IVchg >= stop_limit) & (step_num+1 <= min(bin_num_limit, len_brks)) ) {
    binning_tree = woebin2_tree_add_1brkp(dtm, initial_binning, count_distr_limit, bestbreaks)
    # print(binning_tree)

    # update parameters
    ## best breaks
    bestbreaks = binning_tree[bstbrkp != -Inf & !is.na(bstbrkp), bstbrkp]
    ## information value
    IVt2 = binning_tree[1, total_iv]
    IVchg = IVt2/IVt1-1 ## ratio gain
    IVt1 = IVt2
    # print(IVchg)
    step_num = step_num + 1
  }

  if (is.null(binning_tree)) binning_tree = initial_binning

  return(list(binning_sv=binning_sv, binning=binning_tree))
  # return(binning_tree)
}
# examples
# system.time( binning_list <- woebin2_init_bin(dtm, init_count_distr=0.02, breaks =NULL, spl_val=NULL) )
# initial_binning=binning_list$initial_binning
# binning_sv = binning_list$binning_sv
# system.time( woebin2_tree_add_1brkp(dtm, initial_binning, count_distr_limit=0.05) )
# system.time( woebin2_tree(dtm, initial_binning, count_distr_limit=0.05) )


# required in woebin2 # return chimerge binning
#' @importFrom stats qchisq
woebin2_chimerge = function(
  dtm,
  init_count_distr  = 0.02,
  count_distr_limit = 0.05,
  stop_limit        = 0.1,
  bin_num_limit     = 8,
  breaks            = NULL,
  spl_val           = NULL
) {
  .= a= a_colsum= a_lag= a_lag_rowsum= a_rowsum= a_sum= bad= bin= brkp= brkp2= chisq= count= count_distr= e= e_lag= chisq_lead= good= goodbad= merge_tolead =value= variable= NULL

  # [chimerge](http://blog.csdn.net/qunxingvip/article/details/50449376)
  # [ChiMerge:Discretization of numeric attributs](http://www.aaai.org/Papers/AAAI/1992/AAAI92-019.pdf)

  # chisq = function(a11, a12, a21, a22) {
  #   A = list(a1 = c(a11, a12), a2 = c(a21, a22))
  #   Adf = do.call(rbind, A)
  #
  #   Edf =
  #     matrix(rowSums(Adf), ncol = 1) %*%
  #     matrix(colSums(Adf), nrow = 1) /
  #     sum(Adf)
  #
  #   sum((Adf-Edf)^2/Edf)
  # }
  # initial binning
  bin_list = woebin2_init_bin(dtm, init_count_distr=init_count_distr, breaks=breaks, spl_val=spl_val)
  initial_binning = bin_list$initial_binning
  binning_sv = bin_list$binning_sv

  if (nrow(initial_binning)<=1 || is.null(initial_binning)) {
    return(list(binning_sv=binning_sv, binning=initial_binning))
  }

  # function to create a chisq column in initial_binning
  add_chisq = function(initial_binning) {
  chisq_df = melt(initial_binning[!is.na(brkp)], id.vars = c("brkp", "variable", "bin"), measure.vars = c("good", "bad"), variable.name = "goodbad", value.name = "a"
  )[order(brkp)
  ][, a_lag := shift(a, type="lag"), by=.(goodbad)
  ][, `:=`(
    a_rowsum = sum(a),
    a_lag_rowsum = sum(a_lag),
    a_colsum = a+a_lag,
    a_sum = sum(a+a_lag)), by='bin'
  ][, `:=`(
    e = (a_rowsum*a_colsum)/a_sum,
    e_lag = a_lag_rowsum*a_colsum/a_sum
  )][, .(chisq=sum((a-e)^2/e + (a_lag-e_lag)^2/e_lag)), by='bin']

  return(merge(initial_binning[,count:=good+bad], chisq_df, all.x = TRUE))
  }

  # dtm_rows
  dtm_rows = nrow(dtm)
  # chisq limit
  chisq_limit = qchisq(1-stop_limit,1)
  # binning with chisq column
  binning_chisq = add_chisq(initial_binning)

  # param
  bin_chisq_min = binning_chisq[, min(chisq, na.rm = TRUE)]
  bin_count_distr_min = binning_chisq[!is.na(brkp), min((good+bad)/dtm_rows)]
  bin_nrow = binning_chisq[,.N]
  # remove brkp if chisq < chisq_limit
  while (
    bin_chisq_min < chisq_limit ||
    bin_count_distr_min < count_distr_limit ||
    bin_nrow > bin_num_limit) {
    # brkp needs to be removed
    if (bin_chisq_min < chisq_limit) {
      rm_brkp = binning_chisq[, merge_tolead := FALSE][order(chisq, count)][1,]

    } else if (bin_count_distr_min < count_distr_limit) {
      rm_brkp = binning_chisq[,`:=`(
        count_distr = count/sum(count),
        chisq_lead = shift(chisq, type = "lead", fill = Inf)
      )][,merge_tolead := ifelse(is.na(chisq), TRUE, chisq > chisq_lead)
       ][!is.na(brkp)][order(count_distr)][1,]

    } else if (bin_nrow > bin_num_limit) {
      rm_brkp = binning_chisq[, merge_tolead := FALSE][order(chisq, count)][1,]

    }

    # groupby brkp
    shift_type = ifelse(rm_brkp[1,merge_tolead], 'lead', 'lag')
    binning_chisq = binning_chisq[
      ,brkp2 := shift(brkp,type=shift_type)
    ][brkp == rm_brkp[1,brkp], brkp := brkp2
    ][,.(variable=unique(variable), bin=paste0(bin, collapse = "%,%"), good=sum(good), bad=sum(bad)), by=brkp
    ]#[, badprob:=bad/(good+bad)]

    # update
    ## add chisq to new binning data frame
    binning_chisq = add_chisq(binning_chisq)
    ## param
    bin_nrow = binning_chisq[,.N]
    if (bin_nrow == 1) break
    bin_chisq_min = binning_chisq[, min(chisq, na.rm = TRUE)]
    bin_count_distr_min = binning_chisq[!is.na(brkp), min((good+bad)/dtm_rows)]
  }

  # format bin # remove (.+\\)%,%\\[.+,)
  if (is.numeric(dtm[,value])) {
    binning_chisq = binning_chisq[grepl("%,%",bin), bin := sub("^(\\[.+?,).+,(.+?\\))$", "\\1\\2", bin)]
  }

  return(list(binning_sv=binning_sv, binning=binning_chisq))
  # return(binning_chisq)
}


# required in woebin2 # return equal binning, supports numerical variables only
woebin2_equal = function(dtm, init_count_distr=0.02, count_distr_limit=0.05, stop_limit=0.1, bin_num_limit=8, breaks=NULL, spl_val=NULL, method='freq') {
  count = value = group = . = minv = maxv = bin = y = variable = bad = good = badprob = NULL

  # dtm $ binning_sv
  dtm_binsv_list = dtm_binning_sv(dtm, breaks, spl_val)
  dtm = dtm_binsv_list$dtm
  binning_sv = dtm_binsv_list$binning_sv
  if (is.null(dtm) || dtm[,.N]==0) return(list(binning_sv=binning_sv, binning=NULL))

  # dt_sl = dtm[,.(label=y, datset=variable, score=value)]
  # dtm = dt_sl[,.(y=label, variable=datset, value=score)]


  # breaks
  if (bin_num_limit >= dtm[, length(unique(value))] ) {
    # in each value
    brkp = dtm[order(value)][, unique(value)]
    brkp = c(-Inf, brkp[-1], Inf)
  } else {
    if (method == 'freq') {
      brkp = copy(dtm)[order(value)
                      ][, group := ceiling(.I/(.N/bin_num_limit))
                      ][, .(value=value[1]), by = group
                      ][, c(-Inf, value[-1], Inf)]

    } else if (method == 'width') {
      minmax = dtm[, .(maxv = max(value), minv = min(value))]
      brkp = seq(minmax[,minv], minmax[,maxv], length.out = bin_num_limit+1)
      brkp = c(-Inf, brkp[-c(1, length(brkp))], Inf)

    }
  }
  binning_equal = dtm[, bin := cut(value, unique(brkp), right = FALSE, dig.lab = 10, ordered_result = F)
              ][, .(good = sum(y==0), bad = sum(y==1), count = .N), keyby = .(variable, bin)
              ][, `:=`(brkp = as.numeric( sub("^\\[(.*),.+", "\\1", bin)), badprob = bad/(good+bad))
              ][, .(variable, bin, brkp, count, good, bad, badprob)]


  # create binning
  binning_equal = check_empty_bins(dtm, binning_equal)
  binning_equal = check_zero_goodbad(dtm, binning_equal)
  binning_equal = check_count_distri(dtm, binning_equal, count_distr_limit)

  return(list(binning_sv=binning_sv, binning=binning_equal))
}

# required in woebin2 # # format binning output
binning_format = function(binning) {
  # global variables or functions
  . = bad = badprob = bin = bin_iv = good = total_iv = variable = woe = is_sv = count = NULL

  # required columns in input binning: variable, bin, good, bad
  if (!('count' %in% names(binning))) binning[, count := good+bad]

  binning = binning[
    , badprob:=bad/(good+bad)
  ][, woe := lapply(.SD, woe_01, bad), .SDcols = "good"
  ][, bin_iv := lapply(.SD, miv_01, bad), .SDcols = "good"
  ][, total_iv := sum(bin_iv)
  ][, bin := ifelse(is.na(bin) | bin=="NA", "missing", as.character(bin)) # replace NA by missing
  ][, .(variable, bin, count, count_distr=(good+bad)/sum(good+bad), good, bad, badprob, woe, bin_iv, total_iv,  breaks = sub("^\\[(.*), *(.*)\\)((%,%missing)*)", "\\2\\3", bin), is_special_values=is_sv)]

  # move missing from last row to first
  if ( "missing" %in% binning$bin ) {
    binning = rbind(binning[bin=="missing"], binning[bin != "missing"])
  }

  return(binning)
}

# woebin2
# This function provides woe binning for only two columns (one x and one y) data frame.
woebin2 = function(
  dtm,
  breaks            = NULL,
  spl_val           = NULL,
  init_count_distr  = 0.02,
  count_distr_limit = 0.05,
  stop_limit        = 0.1,
  bin_num_limit     = 8,
  method            = "tree"
) {
  # global variables or functions
  . = bad = badprob = bin = bin_iv = good = total_iv = variable = woe = is_sv = NULL


  # binning
  if (!anyNA(breaks) & !is.null(breaks)) {
    # 1.return binning if breaks provided
    bin_list = woebin2_breaks(dtm=dtm, breaks=breaks, spl_val=spl_val)

  } else {
    if (stop_limit == "N") {
      # binning of initial & specialvalues
      bin_list = woebin2_init_bin(dtm, init_count_distr=init_count_distr, breaks=breaks, spl_val=spl_val)

    } else {
      if (method == "tree") {
        # 2.tree-like optimal binning
        bin_list = woebin2_tree(dtm, init_count_distr, count_distr_limit, stop_limit, bin_num_limit, breaks=breaks, spl_val=spl_val)

      } else if (method == "chimerge") {
        # 2.chimerge optimal binning
        bin_list = woebin2_chimerge(dtm, init_count_distr, count_distr_limit, stop_limit, bin_num_limit, breaks=breaks, spl_val=spl_val)
      } else if (method %in% c('freq','width')) {
        # 3. in equal freq or width
        bin_list = woebin2_equal(dtm, init_count_distr, count_distr_limit, stop_limit, bin_num_limit, breaks=breaks, spl_val=spl_val, method = method)
      }
    }
  }

  # # binding binning_sv and binning
  if (any(sapply(bin_list, is.null))) {
    binning = rbindlist(bin_list)[, is_sv := names(bin_list)[!sapply(bin_list, is.null)]]
  } else {
    binning = rbindlist(bin_list, use.names = TRUE, fill = TRUE, idcol = 'is_sv')
  }
  binning = binning[, is_sv := is_sv == 'binning_sv']

  return(binning_format(binning))
}

# convert bins to breaks_list
bins_to_breaks = function(bins, dt, to_string=FALSE, save_name=NULL) {
  .= bin= bin2= is_special_values= variable= x_breaks= x_class = NULL

  # bins # if (is.list(bins)) rbindlist(bins)
  if (!is.data.table(bins)) {
    if (is.data.frame(bins)) {
      bins = setDT(bins)
    } else {
      bins = rbindlist(bins)
    }
  }

  # x variables
  xs_all = bins[,unique(variable)]

  # class of variables
  vars_class = data.table(
    variable = xs_all,
    x_class = dt[,sapply(.SD, class), .SDcols = xs_all]
  )

  # breaks
  bins_breakslist = bins[
    , bin2 := sub("^\\[(.*), *(.*)\\)((%,%missing)*)", "\\2\\3", bin)
    ][!(bin2 %in% c("-Inf","Inf","missing") & !is_special_values)
    ][vars_class, on="variable"
    ][, .(
      x_breaks = paste(ifelse(x_class=="numeric", bin2, paste0("\"",bin2,"\"")), collapse=", "),
      x_class=unique(x_class)
    ), by=variable]

  if (to_string) {
    bins_breakslist = paste0(bins_breakslist[, paste0(variable, "=c(", x_breaks, ")")], collapse = ", \n ")
    bins_breakslist = paste0(c("breaks_list=list(", bins_breakslist, ")"), collapse = "\n ")
    if (!is.null(save_name)) {
      save_name = sprintf('%s_%s.R', save_name, format(Sys.time(),"%Y%m%d_%H%M%S"))
      writeLines(bins_breakslist, save_name)
      cat(sprintf('[INFO] The breaks_list is saved as %s\n', save_name))
      return()
    }
  }

  return(bins_breakslist)
}

# @param init_count_distr The minimum percentage of initial binning class number over total. Accepted range: 0.01-0.2; default is 0.02, which means initial cut into 50 fine bins for continuous variables.

#' WOE Binning
#'
#' \code{woebin} generates optimal binning for numerical, factor and categorical variables using methods including tree-like segmentation or chi-square merge. \code{woebin} can also customizing breakpoints if the `breaks_list` was provided. The default `woe` is defined as ln(Bad_i/Good_i). If you prefer ln(Good_i/Bad_i), please set the argument `positive` as negative value, such as '0' or 'good'. If there is a zero frequency class when calculating woe, the zero will replaced by 0.99 to make the woe calculable.
#'
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. Default is NULL. If x is NULL, then all columns except y and var_skip are counted as x variables.
#' @param var_skip Name of variables that will skip for binning. Default is NULL.
#' @param breaks_list List of break points, default is NULL. If it is not NULL, variable binning will based on the provided breaks.
#' @param special_values the values specified in special_values will be in separate bins. Default is NULL.
#' @param stop_limit Stop binning segmentation when information value gain ratio less than the stop_limit if using tree method; or stop binning merge when the chi-square of each neighbor bins are larger than 'qchisq(1-stoplimit, 1)' if using chimerge method. Accepted range: 0-0.5; default is 0.1.
#' @param count_distr_limit The minimum count distribution percentage. Accepted range: 0.01-0.2; default is 0.05.
#' @param bin_num_limit Integer. The maximum number of binning. Default is 8.
#' @param positive Value of positive class, default "bad|1".
#' @param no_cores Number of CPU cores for parallel computation. Defaults NULL. If no_cores is NULL, the no_cores will set as 1 if length of x variables less than 10, and will set as the number of all CPU cores if the length of x variables greater than or equal to 10.
#' @param print_step A non-negative integer. Default is 1. If print_step>0, print variable names by each print_step-th iteration. If print_step=0 or no_cores>1, no message is print.
#' @param method Four methods are provided, "tree" and "chimerge" for optimal binning that support both numerical and categorical variables, and 'width' and 'freq' for equal binning that support numerical variables only. Default is "tree".
#' @param save_breaks_list A string. The file name to save breaks_list. Default is None.
#' @param ignore_const_cols Logical. Ignore constant columns. Default is TRUE.
#' @param ignore_datetime_cols Logical. Ignore datetime columns. Default is TRUE.
#' @param check_cate_num Logical. Check whether the number of unique values in categorical columns larger than 50. It might make the binning process slow if there are too many unique categories. Default is TRUE.
#' @param replace_blank_na Logical. Replace blank values with NA. Default is TRUE.
#' @param ... Additional parameters.
#'
#' @return A list of data frames include binning information for each x variables.
#'
#' @seealso \code{\link{woebin_ply}}, \code{\link{woebin_plot}}, \code{\link{woebin_adj}}
#'
#' @examples
#' # load germancredit data
#' data(germancredit)
#'
#' # Example I
#' # binning of two variables in germancredit dataset
#' # using tree method
#' bins2_tree = woebin(germancredit, y="creditability",
#'    x=c("credit.amount","housing"), method="tree")
#' bins2_tree
#'
#' \donttest{
#' # using chimerge method
#' bins2_chi = woebin(germancredit, y="creditability",
#'    x=c("credit.amount","housing"), method="chimerge")
#'
#' # binning in equal freq/width # only supports numerical variables
#' numeric_cols = c("duration.in.month", "credit.amount",
#'   "installment.rate.in.percentage.of.disposable.income", "present.residence.since",
#'   "age.in.years", "number.of.existing.credits.at.this.bank",
#'   "number.of.people.being.liable.to.provide.maintenance.for")
#' bins_freq  = woebin(germancredit, y="creditability", x=numeric_cols, method="freq")
#' bins_width = woebin(germancredit, y="creditability", x=numeric_cols, method="width")
#'
#' # y can be NULL if no label column in dataset
#' bins_freq_noy  = woebin(germancredit, y=NULL, x=numeric_cols)
#'
#' # Example II
#' # binning of the germancredit dataset
#' bins_germ = woebin(germancredit, y = "creditability")
#' # converting bins_germ into a data frame
#' # bins_germ_df = data.table::rbindlist(bins_germ)
#'
#' # Example III
#' # customizing the breakpoints of binning
#' library(data.table)
#' dat = rbind(
#'   germancredit,
#'   data.table(creditability=sample(c("good","bad"),10,replace=TRUE)),
#'   fill=TRUE)
#'
#' breaks_list = list(
#'   age.in.years = c(26, 35, 37, "Inf%,%missing"),
#'   housing = c("own", "for free%,%rent")
#' )
#'
#' special_values = list(
#'   credit.amount = c(2600, 9960, "6850%,%missing"),
#'   purpose = c("education", "others%,%missing")
#' )
#'
#' bins_cus_brk = woebin(dat, y="creditability",
#'   x=c("age.in.years","credit.amount","housing","purpose"),
#'   breaks_list=breaks_list, special_values=special_values)
#'
#' # Example IV
#' # save breaks_list as a R file
#' bins2 = woebin(germancredit, y="creditability",
#'    x=c("credit.amount","housing"), save_breaks_list='breaks_list')
#'
#' }
#'
#' @import data.table foreach
#' @importFrom stats IQR quantile setNames
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom parallel detectCores
#' @export
woebin = function(dt, y, x=NULL, var_skip=NULL, breaks_list=NULL, special_values=NULL, stop_limit=0.1, count_distr_limit=0.05, bin_num_limit=8, positive="bad|1", no_cores=NULL, print_step=0L, method="tree", save_breaks_list=NULL, ignore_const_cols=TRUE, ignore_datetime_cols=TRUE, check_cate_num=TRUE, replace_blank_na=TRUE, ...) {
  # start time
  start_time = proc.time()

  # global variable
  i = NULL
  # arguments ------
  # print_info
  print_info = list(...)[['print_info']]
  if (is.null(print_info)) print_info = TRUE
  # init_count_distr
  min_perc_fine_bin = list(...)[['init_count_distr']]
  init_count_distr = list(...)[['init_count_distr']]
  if (is.null(init_count_distr)) {
    init_count_distr <- ifelse(!is.null(min_perc_fine_bin), min_perc_fine_bin, 0.02)
  }
  # count_distr_limit
  min_perc_coarse_bin = list(...)[['min_perc_coarse_bin']]
  if (!is.null(min_perc_coarse_bin)) count_distr_limit = min_perc_coarse_bin
  # bin_num_limit
  max_num_bin = list(...)[['max_num_bin']]
  if (!is.null(max_num_bin)) bin_num_limit = max_num_bin



  # print info
  if (print_info) cat('[INFO] creating woe binning ... \n')
  # set dt as data.table
  dt = setDT(copy(dt))  #copy(setDT(dt))
  if (!is.null(x)) dt = dt[, c(y,x), with=FALSE]
  # check y
  if (!is.null(y)) dt = check_y(dt, y, positive)
  # remove constant columns
  if (ignore_const_cols) dt = check_const_cols(dt)
  # remove date/time columns
  if (ignore_datetime_cols) dt = check_datetime_cols(dt)
  # check categorical columns' unique values
  if (check_cate_num) check_cateCols_uniqueValues(dt, var_skip)
  # replace black with na
  if (replace_blank_na) dt = rep_blank_na(dt)
  # x variable names
  xs = x_variable(dt, y, x, var_skip)
  xs_len = length(xs)
  # print_step
  print_step = check_print_step(print_step)
  # breaks_list
  breaks_list = check_breaks_list(breaks_list, xs)
  # special_values
  special_values = check_special_values(special_values, xs)



  # # stop_limit vector
  # if (length(stop_limit) == 1) {
  #   stop_limit = rep(stop_limit, length(xs))
  # } else if (length(stop_limit) != length(xs)) {
  #   stop("Incorrect inputs; the length of stop_limit should be 1 or the same as x variables.")
  # }
  # stop_limit range
  if ( stop_limit<0 || stop_limit>0.5 || !is.numeric(stop_limit) ) {
    warning("Incorrect parameter specification; accepted stop_limit parameter range is 0-0.5. Parameter was set to default (0.1).")
    stop_limit = 0.1
  }

  # init_count_distr range
  if ( init_count_distr<0.01 || init_count_distr>0.2 || !is.numeric(init_count_distr) ) {
    warning("Incorrect parameter specification; accepted init_count_distr parameter range is 0.01-0.2. Parameter was set to default (0.02).")
    init_count_distr = 0.02
  }
  # count_distr_limit
  if ( count_distr_limit<0.01 || count_distr_limit>0.2 || !is.numeric(count_distr_limit) ) {
    warning("Incorrect parameter specification; accepted count_distr_limit parameter range is 0.01-0.2. Parameter was set to default (0.05).")
    count_distr_limit = 0.05
  }

  # bin_num_limit
  if (!is.numeric(bin_num_limit)) {
    warning("Incorrect inputs; bin_num_limit should be numeric variable. Parameter was set to default (8).")
    bin_num_limit = 8
  }

  # method
  if (!(method %in% c("tree", "chimerge", 'freq', 'width'))) {
    warning("Incorrect inputs; method should be tree or chimerge. Parameter was set to default (tree).")
    method = "tree"
  }
  if (is.null(y) & !(method %in% c('freq', 'width'))) method = 'freq'

  # binning ------
  # loop on xs # https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/
  if (is.null(no_cores) || no_cores<1) {
    no_cores = ifelse(xs_len < 10, 1, detectCores(logical=F))
  }

  bins = list()
  if (!is.null(y)) {
    y = dt[[y]]
  } else y = NA
  if (no_cores == 1) {
    for (i in 1:xs_len) {
      x_i = xs[i]
      # print xs
      if (print_step>0 & i %% print_step == 0) cat(paste0(format(c(i,xs_len)),collapse = "/"), x_i,"\n")

      # woebining on one variable
      bins[[x_i]] <-
        try(do.call(woebin2, args = list(
          dtm              = data.table(y=y, variable=x_i, value=dt[[x_i]]),
          breaks           = breaks_list[[x_i]],
          spl_val          = special_values[[x_i]],
          init_count_distr = init_count_distr,
          count_distr_limit= count_distr_limit,
          stop_limit       = stop_limit,
          bin_num_limit    = bin_num_limit,
          method           = method
        )), silent = TRUE)
    }

  } else {
    registerDoParallel(no_cores)
    # run
    bins <-
      foreach(
        i = seq_len(xs_len),
        .combine = list,
        .multicombine = TRUE,
        .maxcombine = xs_len+1,
        .inorder = FALSE,
        .errorhandling = "pass",
        .final = function(bs) {
          if (xs_len==1) bs = list(bs)
          setNames(bs, xs)
        },
        .export = c('dt', 'xs', 'y', 'breaks_list', 'special_values', 'init_count_distr', 'count_distr_limit', 'stop_limit', 'bin_num_limit', 'method')
      ) %dopar% {
        x_i = xs[i]

        # woebining on one variable
        try(do.call(woebin2, args = list(
          dtm              = data.table(y=y, variable=x_i, value=dt[[x_i]]),
          breaks           = breaks_list[[x_i]],
          spl_val          = special_values[[x_i]],
          init_count_distr = init_count_distr,
          count_distr_limit= count_distr_limit,
          stop_limit       = stop_limit,
          bin_num_limit    = bin_num_limit,
          method           = method
        )), silent = TRUE)
      }
    # finish
    stopImplicitCluster()
  }

  # check errors in binning
  error_variables = names(bins)[which(sapply(bins, function(x) inherits(x, 'try-error')))]
  if (length(error_variables) > 0) {
    warning(sprintf('The following columns are removed from binning results due to errors:\n%s', paste0(error_variables, collapse=', ')))
    bins = bins[setdiff(names(bins), error_variables)]
  }

  # running time
  rs = proc.time() - start_time
  # hms
  if (rs[3] > 10 & print_info) cat(sprintf("[INFO] Binning on %s rows and %s columns in %s",nrow(dt),ncol(dt),sec_to_hms(rs[3])),"\n")
  # save breaks_list
  if (!is.null(save_breaks_list)) bins_to_breaks(bins, dt, to_string=TRUE, save_name=save_breaks_list)
  return(bins)
}




#' @import data.table
woepoints_ply1 = function(dtx, binx, x_i, woe_points) {
  # woe_points: "woe" "points"
  . = V1 = bin = woe = NULL

  # binx
  binx = binx[
    , bin:=as.character(bin)
  ][,.(unlist(strsplit(bin, "%,%", fixed=TRUE)),
       eval(parse(text = woe_points)) ), by=bin]

  # dtx
  ## cut numeric variable
  if ( is.numeric(dtx[[x_i]]) ) {
    binx_sv = binx[!grepl("\\[",V1)]
    binx_other = binx[grepl("\\[",V1)]

    dtx[[x_i]] = ifelse(
      dtx[[x_i]] %in% binx_sv$V1,
      dtx[[x_i]],
      as.character(cut(dtx[[x_i]], unique(c(-Inf, binx_other[bin != "missing", as.numeric(sub("^\\[(.*),.+", "\\1", V1))], Inf)), right = FALSE, dig.lab = 10, ordered_result = FALSE))
    )

  }
  ## to charcarter, na to missing
  dtx[[x_i]] = as.character(dtx[[x_i]])
  dtx[[x_i]] = ifelse(is.na(dtx[[x_i]]), "missing", dtx[[x_i]])
  ## add rowid column
  dtx = setDT(dtx)[, rowid := .I]

  # rename binx
  setnames(binx, c("bin", x_i, paste(x_i, woe_points, sep="_")))
  # merge
  dtx_suffix = merge(setDF(dtx), setDF(binx), by=x_i, all.x = TRUE)
  dtx_suffix = setDT(dtx_suffix)[order(rowid)][, (c("rowid", "bin", x_i)) := NULL]

  return(dtx_suffix)
}
#' WOE Transformation
#'
#' \code{woebin_ply} converts original input data into woe values based on the binning information generated from \code{woebin}.
#'
#' @param dt A data frame.
#' @param bins Binning information generated from \code{woebin}.
#' @param no_cores Number of CPU cores for parallel computation. Defaults NULL. If no_cores is NULL, the no_cores will set as 1 if length of x variables less than 10, and will set as the number of all CPU cores if the length of x variables greater than or equal to 10.
#' @param print_step A non-negative integer. Default is 1. If print_step>0, print variable names by each print_step-th iteration. If print_step=0 or no_cores>1, no message is print.
#' @param replace_blank_na Logical. Replace blank values with NA. Default is TRUE. This argument should be the same with \code{woebin}'s.
#' @param ... Additional parameters.
#'
#' @return A data frame with columns for variables converted into woe values.
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
#' str(dt_woe)
#'
#' \donttest{
#' # Example II
#' # binning for germancredit dataset
#' bins_germancredit = woebin(germancredit, y="creditability")
#'
#' # converting the values in germancredit to woe
#' # bins is a list which generated from woebin()
#' germancredit_woe = woebin_ply(germancredit, bins_germancredit)
#'
#' # bins is a data frame
#' bins_df = data.table::rbindlist(bins_germancredit)
#' germancredit_woe = woebin_ply(germancredit, bins_df)
#'
#' # return value is bin but not woe
#' germancredit_bin = woebin_ply(germancredit, bins_germancredit, value = 'bin')
#' }
#'
#' @import data.table
#' @export
#'
woebin_ply = function(dt, bins, no_cores=NULL, print_step=0L, replace_blank_na=TRUE, ...) {
  # start time
  start_time = proc.time()

  # print info
  print_info = list(...)[['print_info']]
  if (is.null(print_info)) print_info = TRUE
  if (print_info) cat('[INFO] converting into woe values ... \n')
  # value
  value = list(...)[['value']]
  if (is.null(value) || !(value %in% c('woe', 'bin'))) value = 'woe'

  # global variables or functions
  . = V1 = bin = variable = woe = i = databc_colomun_placeholder = NULL

  # set dt as data.table
  dt = setDT(copy(dt))
  # # remove date/time col
  # dt = rmcol_datetime_unique1(dt)
  # replace "" by NA
  if (replace_blank_na) dt = rep_blank_na(dt)
  # print_step
  print_step = check_print_step(print_step)


  # bins # if (is.list(bins)) rbindlist(bins)
  if (inherits(bins, 'list') && all(sapply(bins, is.data.frame))) {bins = rbindlist(bins)}
  bins = setDT(bins)

  # x variables
  xs_bin = bins[,unique(variable)]
  xs_dt = names(dt)
  xs = intersect(xs_bin, xs_dt)
  # loop on x variables
  xs_len = length(xs)

  # initial data set
  n = 0
  while (paste0('dat_col_placeholder',n) %in% xs) n = n+1
  dt_init = copy(dt)[, (paste0('dat_col_placeholder',n)) := 1][,(xs) := NULL]
  # the databc_colomun_placeholder will be remove in the result, in case dt_init is an empty dataframe

  # loop on xs # https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/
  if (is.null(no_cores)) {
    no_cores = ifelse(xs_len < 10, 1, detectCores(logical=F))
  }

  if (no_cores == 1) {
    dat = dt_init
    for (i in 1:xs_len) {
      x_i = xs[i]
      # print x
      if (print_step > 0 & i %% print_step == 0) cat(paste0(format(c(i,xs_len)),collapse = "/"), x_i,"\n")

      binx = bins[variable==x_i]
      dtx = dt[, x_i, with=FALSE]

      dat = cbind(dat, woepoints_ply1(dtx, binx, x_i, woe_points=value))
    }
  } else {
    registerDoParallel(no_cores)
    # run
    dat <-
      foreach(
        i = 1:xs_len,
        .combine=cbind,
        .init = dt_init,
        .inorder = FALSE,
        .errorhandling = "pass",
        .export = c('dt', 'bins', 'xs')
      ) %dopar% {
        x_i = xs[i]

        binx = bins[variable==x_i]
        dtx = dt[, x_i, with=FALSE]

        woepoints_ply1(dtx, binx, x_i, woe_points=value)
      }
    # finish
    stopImplicitCluster()
  }

  # running time
  rs = proc.time() - start_time
  # hms
  if (rs[3] > 10 & print_info) cat(sprintf("[INFO] Woe transformating on %s rows and %s columns in %s",nrow(dt),xs_len,sec_to_hms(rs[3])),"\n")

  return(dat[, (paste0('dat_col_placeholder',n)) := NULL])
}


# required in woebin_plot
#' @import data.table ggplot2
plot_bin = function(bin, title, show_iv, line_color = 'blue', bar_color = NULL) {
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
  if (show_iv) {
    title_string = paste0(title, dat[1, variable],"  (iv:",bin[1,round(total_iv,4)],")")
  } else {
    title_string = paste0(title, dat[1, variable])
  }

  # plot
  p_bin = ggplot() +
    # geom_text(aes(label="@shichen.name/getpedr", x=dat[, x[.N], by=symbol][,V1[1]], y=Inf), vjust = -0.5, hjust = 1, color = "#F0F0F0") +
    # coord_cartesian(clip = 'off') +
    geom_bar(data=dat_melt, aes(x=bin, y=value, fill=goodbad), stat="identity") +
    geom_text(data=dat, aes(x = bin, y = count_distr2, label = paste0(round(count_distr2*100, 1), "%, ", count_num) ), vjust = 0.5) +
    geom_line(data=dat, aes(x = rowid, y = badprob2), colour = line_color) +
    geom_point(data=dat, aes(x = rowid, y=badprob2), colour = line_color, shape=21, fill="white") +
    geom_text(data=dat, aes(x = rowid, y = badprob2, label = paste0(round(badprob*100, 1), "%")), colour=line_color, vjust = -0.5) +
    scale_y_continuous(limits = c(0,y_left_max), sec.axis = sec_axis(~./(y_left_max/y_right_max), name = "Bad probability")) +
    labs(title = title_string, x=NULL, y="Bin count distribution", fill=NULL) +
    theme_bw() +
    theme(
      legend.position="bottom", legend.direction="horizontal",
      axis.title.y.right = element_text(colour = line_color),
      axis.text.y.right  = element_text(colour = line_color,angle=90, hjust = 0.5),
      axis.text.y = element_text(angle=90, hjust = 0.5) )

  if (!is.null(bar_color)) p_bin = p_bin + scale_fill_manual(values= bar_color)

  return(p_bin)
}
#' WOE Binning Visualization
#'
#' \code{woebin_plot} create plots of count distribution and bad probability for each bin. The binning informations are generates by  \code{woebin}.
#'
#' @name woebin_plot
#' @param bins A list of data frames. Binning information generated by \code{woebin}.
#' @param x Name of x variables. Default is NULL. If x is NULL, then all columns except y are counted as x variables.
#' @param title String added to the plot title. Default is NULL.
#' @param show_iv Logical. Default is TRUE, which means show information value in the plot title.
#' @param ... Additional parameters
#'
#' @return A list of binning graphics.
#'
#' @seealso  \code{\link{woebin}}, \code{\link{woebin_ply}}, \code{\link{woebin_adj}}
#'
#' @examples
#' # Load German credit data
#' data(germancredit)
#'
#' # Example I
#' bins1 = woebin(germancredit, y="creditability", x="credit.amount")
#'
#' p1 = woebin_plot(bins1)
#' # modify colors
#' # woebin_plot(bins1, line_color='#FC8D59', bar_color=c('#FFFFBF', '#99D594'))
#'
#' \donttest{
#' # Example II
#' bins = woebin(germancredit, y="creditability")
#' plotlist = woebin_plot(bins)
#' print(plotlist$credit.amount)
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
woebin_plot = function(bins, x=NULL, title=NULL, show_iv = TRUE, ...) {
  # global variables or functions
  variable = NULL
  xs = x

  # line bar colors
  line_color = list(...)[['line_color']]
  if (is.null(line_color)) line_color = 'blue'
  bar_color = list(...)[['bar_color']]

  # converting data.frame into list
  # bins # if (is.list(bins)) rbindlist(bins)
  if (!is.data.table(bins)) {
    if (is.data.frame(bins)) {
      bins = setDT(bins)
    } else {
      bins = rbindlist(bins)
    }
  }

  # x variable names
  if (is.null(xs)) xs = bins[,unique(variable)]


  # plot export
  plotlist = list()
  for (i in xs) plotlist[[i]] = plot_bin(bins[variable==i], title, show_iv, line_color = line_color, bar_color = bar_color)


  return(plotlist)
}



# print basic information in woebin_adj
woebin_adj_print_basic_info = function(i, xs_adj, bins, dt, bins_breakslist) {
  x_i = xs_adj[i]
  xs_len = length(xs_adj)
  variable = x_breaks = NULL

  bin = bins[variable==x_i]

  cat("--------", paste0(i, "/", xs_len), x_i, "--------\n")
  ## class
  cat(paste0("> class(",x_i,"): "),"\n",class(dt[[x_i]]),"\n","\n")
  ## summary
  cat(paste0("> summary(",x_i,"): "),"\n")
  print(summary(dt[[x_i]]))
  cat("\n")
  ## table
  if (length(table(dt[[x_i]])) < 10 || !is.numeric(dt[[x_i]])) {
    cat(paste0("> table(",x_i,"): "))
    print(table(dt[[x_i]]))
    cat("\n")
  } else {
    if ( is.numeric(dt[[x_i]])) {
      ht = hist(dt[[x_i]], plot = FALSE)
      plot(ht, main = x_i, xlab = NULL)
    }
  }
  ## current breaks
  breaks_bin = bins_breakslist[variable == x_i, x_breaks]

  cat("> Current breaks: \n", breaks_bin,"\n \n")
  ## woebin plotting
  plist = woebin_plot(bin)
  print(plist[[1]])

}
# plot adjusted binning in woebin_adj
woebin_adj_break_plot = function(dt, y, x_i, breaks, stop_limit, sv_i, method) {
  bin_adj = NULL

  text_woebin = paste0("bin_adj=woebin(dt[,c(\"",x_i,"\",\"",y,"\"),with=F], \"",y,"\", breaks_list=list(",x_i,"=c(",breaks,")), special_values =list(",x_i,"=c(", sv_i, ")), ", ifelse(stop_limit=="N","stop_limit = \"N\", ",NULL), "print_step=0L, print_info=FALSE, method=\"",method,"\")")

  eval(parse(text = text_woebin))


  ## print adjust breaks
  breaks_bin = setdiff(sub("^\\[(.*), *(.*)\\)((%,%missing)*)", "\\2\\3", bin_adj[[1]]$bin), c("-Inf","Inf","missing"))
  breaks_bin = ifelse(
    is.numeric(dt[[x_i]]),
    paste0(breaks_bin, collapse=", "),
    paste0(paste0("\"",breaks_bin,"\""), collapse=", "))
  cat("> Current breaks: ","\n",breaks_bin,"\n","\n")

  # print bin_adj
  print(woebin_plot(bin_adj)[[1]])

  # # breaks
  if (breaks == "" || is.null(breaks)) breaks = breaks_bin

  return(breaks)
}
#' WOE Binning Adjustment
#'
#' \code{woebin_adj} interactively adjust the binning breaks.
#'
#' @param dt A data frame.
#' @param y Name of y variable.
#' @param bins A list of data frames. Binning information generated from \code{woebin}.
#' @param adj_all_var Logical, whether to show variables have monotonic woe trends. Default is TRUE
#' @param special_values The values specified in special_values will in separate bins. Default is NULL.
#' @param method Optimal binning method, it should be "tree" or "chimerge". Default is "tree".
#' @param save_breaks_list A string. The file name to save breaks_list. Default is None.
#' @param count_distr_limit The minimum count distribution percentage. Accepted range: 0.01-0.2; default is 0.05. This argument should be the same with woebin's.
#'
#' @return A list of modified break points of each x variables.
#'
#' @seealso  \code{\link{woebin}}, \code{\link{woebin_ply}}, \code{\link{woebin_plot}}
#'
#' @examples
#' \donttest{
#' # Load German credit data
#' data(germancredit)
#'
#' # Example I
#' dt = germancredit[, c("creditability", "age.in.years", "credit.amount")]
#' bins = woebin(dt, y="creditability")
#' breaks_adj = woebin_adj(dt, y="creditability", bins)
#' bins_final = woebin(dt, y="creditability",
#'                     breaks_list=breaks_adj)
#'
#' # Example II
#' binsII = woebin(germancredit, y="creditability")
#' breaks_adjII = woebin_adj(germancredit, "creditability", binsII)
#' bins_finalII = woebin(germancredit, y="creditability",
#'                     breaks_list=breaks_adjII)
#' }
#'
#' @import data.table
#' @importFrom utils menu
#' @importFrom graphics hist plot
#' @export
woebin_adj = function(dt, y, bins, adj_all_var=TRUE, special_values=NULL, method="tree", save_breaks_list=NULL, count_distr_limit = 0.05) {
  # global variables or functions
  . = V1 = badprob = badprob2 = bin2 = bin = bin_adj = count_distr = variable = x_breaks = x_class = NULL

  dt = setDT(copy(dt))
  # bins # if (is.list(bins)) rbindlist(bins)
  if (!is.data.table(bins)) {
    if (is.data.frame(bins)) {
      bins = setDT(bins)
    } else {
      bins = rbindlist(bins)
    }
  }

  # x variables
  xs_all = bins[,unique(variable)]
  if (adj_all_var == FALSE) {
    xs_adj = bins[
      !(bin == "missing" & count_distr >= count_distr_limit)
    ][, badprob2 := badprob >= shift(badprob, type = "lag"), by=variable
    ][!is.na(badprob2), length(unique(badprob2)), by=variable
    ][V1 > 1, variable]
  } else {
    xs_adj = xs_all
  }
  # length of adjusting variables
  xs_len = length(xs_adj)
  # special_values
  special_values = check_special_values(special_values, xs_adj)

  # breakslist of bins
  bins_breakslist = bins_to_breaks(bins, dt)
  # loop on adjusting variables
  if (xs_len == 0) {
    warning("The binning breaks of all variables are perfect according to default settings.")

    breaks_list = paste0(bins_breakslist[, paste0(variable, "=c(", x_breaks, ")")], collapse = ", \n ")
    breaks_list = paste0(c("list(", breaks_list, ")"), collapse = "\n ")

    return(breaks_list)
  }


  i = 1
  breaks_list = NULL
  while (i <= xs_len) {
    # x variable
    breaks = stop_limit = NULL
    x_i = xs_adj[i]
    sv_i = paste(paste0("\'",special_values[[x_i]],"\'"), collapse = ",")

    # basic information of x_i variable ------
    woebin_adj_print_basic_info(i, xs_adj, bins, dt, bins_breakslist)

    # adjusting breaks ------
    adj_brk = menu(c("next", "yes", "back"), title=paste0("> Adjust breaks for (", i, "/", xs_len, ") ", x_i, "?"))

    while (adj_brk == 2) {
      # modify breaks adj_brk == 2
      breaks = readline("> Enter modified breaks: ")
      breaks = gsub("^[,\\.]+|[,\\.]+$", "", breaks)
      if (breaks == "N") {
        stop_limit = "N"
        breaks = NULL
      } else {
        stop_limit = NULL
      }

      tryCatch(breaks <- woebin_adj_break_plot(dt, y, x_i, breaks, stop_limit, sv_i, method=method), error = function(e) e)

      adj_brk = menu(c("next", "yes", "back"), title=paste0("> Adjust breaks for (", i, "/", xs_len, ") ", x_i, "?"))
    }

    if (adj_brk == 3) {
      # go back adj_brk == 3
      i = ifelse(i > 1, i-1, i)
    } else {
      # go next adj_brk == 1
      if (!(is.null(breaks) || breaks == "")) bins_breakslist[variable == x_i][["x_breaks"]]  <- breaks
    i = i + 1
    }
  }

  breaks_list = paste0(bins_breakslist[, paste0(variable, "=c(", x_breaks, ")")], collapse = ", \n ")
  breaks_list = paste0(c("list(", breaks_list, ")"), collapse = "\n ")

  cat(breaks_list,"\n")
  if (!is.null(save_breaks_list)) {
    bins_adj = woebin(dt, y, x=bins_breakslist[,variable], breaks_list=breaks_list, print_info=FALSE)
    bins_to_breaks(bins_adj, dt, to_string=TRUE, save_name=save_breaks_list)
  }
  return(breaks_list)
}
