# woebin woebin_plot woebin_ply woebin_adj

# converting vector (breaks & special_values) to dataframe
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
    # special_values from vector to dataframe
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

# required in woebin2 # return binning if breaks provided
#' @import data.table
woebin2_breaks = function(dtm, breaks, spl_val) {
  # global variables or functions
  value = bin = . = y = variable = bad = good = V1 = badprob = bksv_list = bin_chr = NULL

  # breaks from vector to dataframe
  bk_df = split_vec_todf(breaks)

  # dtm $ binning_sv
  dtm_binsv_list = dtm_binning_sv(dtm, breaks, spl_val)
  dtm = dtm_binsv_list$dtm
  binning_sv = dtm_binsv_list$binning_sv
  if (dtm[,.N] == 0) return(list(binning_sv=binning_sv, binning=NULL))


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

    # merge binning with bk_df
    binning = merge(
      dtm, bk_df[,bin:=bin_chr], all.x = TRUE
    )[order(rowid, bin)][, .(good = sum(y==0), bad = sum(y==1), variable=unique(variable)) , by = .(rowid, bin)]

  }


  # # remove rowid column in binning dataframe
  binning = binning[,rowid:=1][,rowid:=NULL]
  # # bind binning_sv and binning
  # if (setDT(binning_sv)[,.N] > 0) binning = rbind(binning_sv, binning)

  return(list(binning_sv=binning_sv, binning=binning))
}

# required in woebin2 # return initial binning
woebin2_init_bin = function(dtm, min_perc_fine_bin, breaks, spl_val) {
  # global variables or functions
  value = bin = . = y = variable = bad = good = brkp = badprob = count = merge_tolead = brkp2 = count_lag = count_lead = NULL


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
    n = trunc(1/min_perc_fine_bin)
    len_uniq_x = length(setdiff(unique(xvalue_rm_outlier), c(NA,Inf,-Inf)))
    if (len_uniq_x < n) n = len_uniq_x

    # initial breaks
    if (len_uniq_x < 10) {
      brk = setdiff(unique(xvalue_rm_outlier), c(NA, Inf, -Inf))
    } else {
      brk = pretty(xvalue_rm_outlier, n)
    }
    brk = sort(brk[(brk < max(xvalue_rm_outlier, na.rm =TRUE)) & (brk > min(xvalue_rm_outlier, na.rm =TRUE))])
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
  while (init_bin[!is.na(brkp)][good==0 | bad==0,.N] > 0) {
    # brkp needs to be removed if good==0 or bad==0
    rm_brkp = init_bin[!is.na(brkp)][
      ,count := good+bad
    ][,`:=`(
      count_lag=shift(count,type="lag", fill=nrow(dtm)+1),
      count_lead=shift(count,type="lead", fill=nrow(dtm)+1)
   )][, merge_tolead := count_lag > count_lead
    ][good == 0 | bad == 0][count == min(count)]

    # set brkp to lead's or lag's
    shift_type = ifelse(rm_brkp[1,merge_tolead], 'lead', 'lag')
    init_bin = init_bin[
      ,brkp2 := shift(brkp,type=shift_type)
    ][brkp == rm_brkp[1,brkp], brkp := brkp2]

    # groupby brkp
    init_bin = init_bin[
      ,.(variable=unique(variable), bin=paste0(bin, collapse = "%,%"), good=sum(good), bad=sum(bad)), by=brkp
    ][, badprob:=bad/(good+bad)]
  }

  # format bin
  if (is.numeric(dtm[,value])) {
    init_bin = init_bin[
      grepl("%,%",bin), bin := sub("^(\\[.+?,).+,(.+?\\))$", "\\1\\2", bin)
    ][, brkp := as.numeric(sub("^\\[(.*),.+", "\\1", bin))]
  }

  return(list(binning_sv=binning_sv, initial_binning=init_bin))
}

# required in woebin2_tree # add 1 best break for tree-like binning
woebin2_tree_add_1brkp = function(dtm, initial_binning, min_perc_coarse_bin, bestbreaks=NULL) {
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

  # bestbreaks: total_iv == max(total_iv) & min(count_distr) >= min_perc_coarse_bin
  bstbrk_max_iv = total_iv_all_brks[min_count_distr >= min_perc_coarse_bin][total_iv==max(total_iv)][, bstbin]
  # add 1best break to bestbreaks
  bestbreaks = unique(c(bestbreaks, bstbrk_max_iv[1]))
  bin_add_1bst = binning_add_1bst(initial_binning, bestbreaks)

  return(bin_add_1bst)
}

# required in woebin2 # return tree-like binning
woebin2_tree = function(dtm, min_perc_fine_bin=0.02, min_perc_coarse_bin=0.05, stop_limit=0.1, max_num_bin=8, breaks=NULL, spl_val=NULL) {
  # global variables or functions
  brkp = bstbrkp = total_iv = binning_tree = NULL

  # initial binning
  bin_list = woebin2_init_bin(dtm, min_perc_fine_bin=min_perc_fine_bin, breaks=breaks, spl_val=spl_val)
  initial_binning = bin_list$initial_binning
  binning_sv = bin_list$binning_sv

  if (nrow(initial_binning)==1) {
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
  while ( (IVchg >= stop_limit) & (step_num+1 <= min(max_num_bin, len_brks)) ) {
    binning_tree = woebin2_tree_add_1brkp(dtm, initial_binning, min_perc_coarse_bin, bestbreaks)
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
# system.time( binning_list <- woebin2_init_bin(dtm, min_perc_fine_bin=0.02, breaks =NULL, spl_val=NULL) )
# initial_binning=binning_list$initial_binning
# binning_sv = binning_list$binning_sv
# system.time( woebin2_tree_add_1brkp(dtm, initial_binning, min_perc_coarse_bin=0.05) )
# system.time( woebin2_tree(dtm, initial_binning, min_perc_coarse_bin=0.05) )

# required in woebin2 # return chimerge binning
#' @importFrom stats qchisq
woebin2_chimerge = function(dtm, min_perc_fine_bin=0.02, min_perc_coarse_bin=0.05, stop_limit=0.1, max_num_bin=8, breaks=NULL, spl_val=NULL) {
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
  bin_list = woebin2_init_bin(dtm, min_perc_fine_bin=min_perc_fine_bin, breaks=breaks, spl_val=spl_val)

  initial_binning = bin_list$initial_binning
  binning_sv = bin_list$binning_sv

  # function to create a chisq column in initial_binning
  add_chisq = function(initial_binning) {
  chisq_df = melt(initial_binning[!is.na(brkp)], id.vars = c("brkp", "variable", "bin"), measure.vars = c("good", "bad"), variable.name = "goodbad", value.name = "a"
  )[order(brkp)
  ][, a_lag := shift(a, type="lag"), by=.(goodbad)
  ][, `:=`(
    a_rowsum = sum(a),
    a_lag_rowsum = sum(a_lag),
    a_colsum = a+a_lag,
    a_sum = sum(a+a_lag)), by=brkp
  ][, `:=`(
    e = a_rowsum/a_sum*a_colsum,
    e_lag = a_lag_rowsum/a_sum*a_colsum
  )][, .(chisq=sum((a-e)^2/e + (a_lag-e_lag)^2/e_lag)), by=brkp]

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
    bin_count_distr_min < min_perc_coarse_bin ||
    bin_nrow > max_num_bin) {
    # brkp needs to be removed
    if (bin_chisq_min < chisq_limit) {
      rm_brkp = binning_chisq[, merge_tolead := FALSE][order(chisq, count)][1,]

    } else if (bin_count_distr_min < min_perc_coarse_bin) {
      rm_brkp = binning_chisq[,`:=`(
        count_distr = count/sum(count),
        chisq_lead = shift(chisq, type = "lead", fill = Inf)
      )][,merge_tolead := ifelse(is.na(chisq), TRUE, chisq > chisq_lead)
       ][!is.na(brkp)][order(count_distr)][1,]

    } else if (bin_nrow > max_num_bin) {
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
    ## add chisq to new binning dataframe
    binning_chisq = add_chisq(binning_chisq)
    ## param
    bin_chisq_min = binning_chisq[, min(chisq, na.rm = TRUE)]
    bin_count_distr_min = binning_chisq[!is.na(brkp), min((good+bad)/dtm_rows)]
    bin_nrow = binning_chisq[,.N]
  }

  # format bin # remove (.+\\)%,%\\[.+,)
  if (is.numeric(dtm[,value])) {
    binning_chisq = binning_chisq[grepl("%,%",bin), bin := sub("^(\\[.+?,).+,(.+?\\))$", "\\1\\2", bin)]
  }

  return(list(binning_sv=binning_sv, binning=binning_chisq))
  # return(binning_chisq)
}

# required in woebin2 # # format binning output
binning_format = function(binning) {
  # global variables or functions
  . = bad = badprob = bin = bin_iv = good = total_iv = variable = woe = is_sv = NULL
  # required columns in input binning: variable, bin, good, bad
  binning = binning[
    , badprob:=bad/(good+bad)
  ][, woe := lapply(.SD, woe_01, bad), .SDcols = "good"
  ][, bin_iv := lapply(.SD, miv_01, bad), .SDcols = "good"
  ][, total_iv := sum(bin_iv)
  ][, bin := ifelse(is.na(bin) | bin=="NA", "missing", as.character(bin)) # replace NA by missing
  ][, .(variable, bin, count=good+bad, count_distr=(good+bad)/sum(good+bad), good, bad, badprob, woe, bin_iv, total_iv,  breaks = sub("^\\[(.*), *(.*)\\)((%,%missing)*)", "\\2\\3", bin), is_special_values=is_sv)]

  # move missing from last row to first
  if ( "missing" %in% binning$bin ) {
    binning = rbind(binning[bin=="missing"], binning[bin != "missing"])
  }

  return(binning)
}

# woebin2
# This function provides woe binning for only two columns (one x and one y) dataframe.
woebin2 = function(dtm, breaks=NULL, spl_val=NULL, min_perc_fine_bin=0.02, min_perc_coarse_bin=0.05, stop_limit=0.1, max_num_bin=8, method="tree") {
  # global variables or functions
  . = bad = badprob = bin = bin_iv = good = total_iv = variable = woe = is_sv = NULL


  # binning
  if (!anyNA(breaks) & !is.null(breaks)) {
    # 1.return binning if breaks provided
    bin_list = woebin2_breaks(dtm=dtm, breaks=breaks, spl_val=spl_val)

  } else {
    if (stop_limit == "N") {
      # binning of initial & specialvalues
      bin_list = woebin2_init_bin(dtm, min_perc_fine_bin=min_perc_fine_bin, breaks=breaks, spl_val=spl_val)

    } else {
      if (method == "tree") {
        # 2.tree-like optimal binning
        bin_list = woebin2_tree(dtm, min_perc_fine_bin, min_perc_coarse_bin, stop_limit, max_num_bin, breaks=breaks, spl_val=spl_val)

      } else if (method == "chimerge") {
        # 2.chimerge optimal binning
        bin_list = woebin2_chimerge(dtm, min_perc_fine_bin, min_perc_coarse_bin, stop_limit, max_num_bin, breaks=breaks, spl_val=spl_val)
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

#' WOE Binning
#'
#' \code{woebin} generates optimal binning for numerical, factor and categorical variables using methods including tree-like segmentation or chi-square merge. \code{woebin} can also customizing breakpoints if the breaks_list was provided.
#'
#' @name woebin
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. Default is NULL. If x is NULL, then all variables except y are counted as x variables.
#' @param breaks_list List of break points, default is NULL. If it is not NULL, variable binning will based on the provided breaks.
#' @param special_values the values specified in special_values will be in separate bins. Default is NULL.
#' @param min_perc_fine_bin The minimum percentage of initial binning class number over total. Accepted range: 0.01-0.2; default is 0.02, which means initial binning into 50 fine bins for continuous variables.
#' @param min_perc_coarse_bin The minimum percentage of final binning class number over total. Accepted range: 0.01-0.2; default is 0.05.
#' @param stop_limit Stop binning segmentation when information value gain ratio less than the stop_limit, or stop binning merge when the minimum of chi-square less than 'qchisq(1-stoplimit, 1)'. Accepted range: 0-0.5; default is 0.1.
#' @param max_num_bin Integer. The maximum number of binning.
#' @param positive Value of positive class, default "bad|1".
#' @param no_cores Number of CPU cores for parallel computation. Defaults NULL. If no_cores is NULL, the no_cores will set as 1 if length of x variables less than 10, and will set as the number of all CPU cores if the length of x variables greater than or equal to 10.
#' @param print_step A non-negative integer. Default is 1. If print_step>0, print variable names by each print_step-th iteration. If print_step=0 or no_cores>1, no message is print.
#' @param method Optimal binning method, it should be "tree" or "chimerge". Default is "tree".
#' @return Optimal or customized binning information.
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
#'
#' \dontrun{
#' # using chimerge method
#' bins2_chi = woebin(germancredit, y="creditability",
#'    x=c("credit.amount","housing"), method="chimerge")
#'
#' # Example II
#' # binning of the germancredit dataset
#' bins_germ = woebin(germancredit, y = "creditability")
#' # converting bins_germ into a dataframe
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
#' }
#'
#' @import data.table foreach
#' @importFrom stats IQR quantile setNames
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom parallel detectCores
#' @export
#'
woebin = function(dt, y, x=NULL, breaks_list=NULL, special_values=NULL, min_perc_fine_bin=0.02, min_perc_coarse_bin=0.05, stop_limit=0.1, max_num_bin=8, positive="bad|1", no_cores=NULL, print_step=0L, method="tree") {
  # start time
  start_time = proc.time()
  # global variable
  i = NULL

  # set dt as data.table
  dt = setDT(dt)
  # remove date/time col
  dt = rmcol_datetime_unique1(dt)
  # replace "" by NA
  dt = rep_blank_na(dt)
  # check y
  dt = check_y(dt, y, positive)
  # x variable names
  xs = x_variable(dt,y,x)
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

  # min_perc_fine_bin range
  if ( min_perc_fine_bin<0.01 || min_perc_fine_bin>0.2 || !is.numeric(min_perc_fine_bin) ) {
    warning("Incorrect parameter specification; accepted min_perc_fine_bin parameter range is 0.01-0.2. Parameter was set to default (0.02).")
    min_perc_fine_bin = 0.02
  }
  # min_perc_coarse_bin
  if ( min_perc_coarse_bin<0.01 || min_perc_coarse_bin>0.2 || !is.numeric(min_perc_coarse_bin) ) {
    warning("Incorrect parameter specification; accepted min_perc_coarse_bin parameter range is 0.01-0.2. Parameter was set to default (0.05).")
    min_perc_coarse_bin = 0.05
  }

  # max_num_bin
  if (!is.numeric(max_num_bin)) {
    warning("Incorrect inputs; max_num_bin should be numeric variable. Parameter was set to default (8).")
    max_num_bin = 8
  }

  # method
  if (!(method %in% c("tree", "chimerge"))) {
    warning("Incorrect inputs; method should be tree or chimerge. Parameter was set to default (tree).")
    method = "tree"
  }

  # binning for each x variable
  # loop on xs # https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/
  if (is.null(no_cores) || no_cores<1) {
    no_cores = ifelse(xs_len < 10, 1, detectCores())
  }


  bins = list()
  if (no_cores == 1) {
    for (i in 1:xs_len) {
      x_i = xs[i]
      # print xs
      if (print_step>0 & i %% print_step == 0) cat(paste0(format(c(i,xs_len)),collapse = "/"), x_i,"\n")

      # woebining on one variable
      bins[[x_i]] <-
      tryCatch(
        woebin2(
          dtm = data.table(y=dt[[y]], variable=x_i, value=dt[[x_i]]),
          breaks=breaks_list[[x_i]],
          spl_val=special_values[[x_i]],
          min_perc_fine_bin=min_perc_fine_bin,
          min_perc_coarse_bin=min_perc_coarse_bin,
          stop_limit=stop_limit, max_num_bin=max_num_bin,
          method=method
        ),
        error = function(e) return(paste0("The variable '", x_i, "'", " caused the error: '", e, "'"))
      )
    }

  } else {
    registerDoParallel(no_cores)
    # run
    bins <-
      foreach(
        i = 1:xs_len,
        .combine = list,
        .multicombine = TRUE,
        .maxcombine = xs_len+1,
        .inorder = FALSE,
        .errorhandling = "pass",
        .final = function(bs) {
          if (xs_len==1) bs = list(bs)
          setNames(bs, xs)
        },
        .export = c("dt", "xs", "y", "breaks_list", "min_perc_fine_bin", "stop_limit", "max_num_bin")
      ) %dopar% {
        x_i = xs[i]

        # woebining on one variable
        tryCatch(
          woebin2(
            dtm = data.table(y=dt[[y]], variable=x_i, value=dt[[x_i]]),
            breaks=breaks_list[[x_i]],
            spl_val=special_values[[x_i]],
            min_perc_fine_bin=min_perc_fine_bin,
            min_perc_coarse_bin=min_perc_coarse_bin,
            stop_limit=stop_limit, max_num_bin=max_num_bin,
            method=method
          ),
          error = function(e) return(paste0("The variable '", x_i, "'", " caused the error: '", e, "'"))
        )
      }
    # finish
    stopImplicitCluster()
  }
  # running time
  rs = proc.time() - start_time
  # hms
  if (rs[3] > 10) cat(sprintf("Binning on %s rows and %s columns in %s",nrow(dt),ncol(dt),sec_to_hms(rs[3])),"\n")

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
#' str(dt_woe)
#'
#' \dontrun{
#' # Example II
#' # binning for germancredit dataset
#' bins_germancredit = woebin(germancredit, y="creditability")
#'
#' # converting the values in germancredit to woe
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
woebin_ply = function(dt, bins, no_cores=NULL, print_step=0L) {
  # start time
  start_time = proc.time()
  # global variables or functions
  . = V1 = bin = variable = woe = i = NULL

  # set dt as data.table
  dt = setDT(dt)
  # remove date/time col
  dt = rmcol_datetime_unique1(dt)
  # replace "" by NA
  dt = rep_blank_na(dt)
  # ncol of dt
  # if (ncol(dt) <=1 & !is.null(ncol(dt))) stop("Incorrect inputs; dt should have at least two columns.")
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
  xs_len = length(xs)

  # initial data set
  dt_init = copy(dt)[,(xs):=NULL]

  # loop on xs # https://www.r-bloggers.com/how-to-go-parallel-in-r-basics-tips/
  if (is.null(no_cores)) {
    no_cores = ifelse(xs_len < 10, 1, detectCores())
  }

  if (no_cores == 1) {
    dat = dt_init
    for (i in 1:xs_len) {
      x_i = xs[i]
      # print x
      if (print_step > 0 & i %% print_step == 0) cat(paste0(format(c(i,xs_len)),collapse = "/"), x_i,"\n")

      binx = bins[variable==x_i]
      dtx = dt[, x_i, with=FALSE]

      dat = cbind(dat, woepoints_ply1(dtx, binx, x_i, woe_points="woe"))
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
        .export = c("dt", "xs")
      ) %dopar% {
        x_i = xs[i]

        binx = bins[variable==x_i]
        dtx = dt[, x_i, with=FALSE]

        woepoints_ply1(dtx, binx, x_i, woe_points="woe")
      }
    # finish
    stopImplicitCluster()
  }

  # running time
  rs = proc.time() - start_time
  # hms
  if (rs[3] > 10) cat(sprintf("Woe transformating on %s rows and %s columns in %s",nrow(dt),xs_len,sec_to_hms(rs[3])),"\n")

  return(dat)
}


# required in woebin_plot
#' @import data.table ggplot2
plot_bin = function(bin, title, show_iv) {
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
  ggplot() +
    geom_bar(data=dat_melt, aes(x=bin, y=value, fill=goodbad), stat="identity") +
    geom_text(data=dat, aes(x = bin, y = count_distr2, label = paste0(round(count_distr2*100, 1), "%, ", count_num) ), vjust = 0.5) +
    geom_line(data=dat, aes(x = rowid, y = badprob2), colour = "blue") +
    geom_point(data=dat, aes(x = rowid, y=badprob2), colour = "blue", shape=21, fill="white") +
    geom_text(data=dat, aes(x = rowid, y = badprob2, label = paste0(round(badprob*100, 1), "%")), colour="blue", vjust = -0.5) +
    scale_y_continuous(limits = c(0,y_left_max), sec.axis = sec_axis(~./(y_left_max/y_right_max), name = "Bad probability")) +
    labs(title = title_string, x=NULL, y="Bin count distribution", fill=NULL) +
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
#' @param x Name of x variables. Default is NULL. If x is NULL, then all variables except y are counted as x variables.
#' @param title String added to the plot title. Default is NULL.
#' @param show_iv Logical. Default is TRUE, which means show information value in the plot title.
#' @return List of binning plot
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
#' print(p1)
#'
#' \dontrun{
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
woebin_plot = function(bins, x=NULL, title=NULL, show_iv = TRUE) {
  # global variables or functions
  variable = NULL
  xs = x


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
  for (i in xs) plotlist[[i]] = plot_bin(bins[variable==i], title, show_iv)


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

  text_woebin = paste0("bin_adj=woebin(dt[,c(\"",x_i,"\",\"",y,"\"),with=F], \"",y,"\", breaks_list=list(",x_i,"=c(",breaks,")), special_values =list(",x_i,"=c(", sv_i, ")), ", ifelse(stop_limit=="N","stop_limit = \"N\", ",NULL), "print_step=0L, method=\"",method,"\")")

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
#' @param bins A list or data frame. Binning information generated from \code{woebin}.
#' @param adj_all_var Logical, default is TRUE. If it is TRUE, all variables need to adjust binning breaks, otherwise, only include the variables that have more then one inflection point.
#' @param special_values the values specified in special_values will in separate bins. Default is NULL.
#' @param method optimal binning method, it should be "tree" or "chimerge". Default is "tree".
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
#'
woebin_adj = function(dt, y, bins, adj_all_var=TRUE, special_values=NULL, method="tree") {
  # global variables or functions
  . = V1 = badprob = badprob2 = bin2 = bin = bin_adj = count_distr = variable = x_breaks = x_class = NULL

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
  xs_all = bins[,unique(variable)]
  if (adj_all_var == FALSE) {
    xs_adj = bins[
      bin != "missing"
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

  # class of variables
  vars_class = data.table(
    variable = xs_all,
    x_class = dt[,sapply(.SD, class), .SDcols = xs_all]
  )
  # breakslist of bins
  bins_breakslist = bins[
    , bin2 := sub("^\\[(.*), *(.*)\\)((%,%missing)*)", "\\2\\3", bin)
    ][!(bin2 %in% c("-Inf","Inf","missing"))
    ][vars_class, on="variable"
    ][, .(
      x_breaks = paste(ifelse(x_class == "numeric", bin2, paste0("\"", bin2, "\"")), collapse = ", "),
      x_class=unique(x_class)), by=variable]


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
  return(breaks_list)
}
