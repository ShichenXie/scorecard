brk_txt2vector = function(brk) {
  if (is.null(brk) || is.na(brk)) return(brk)

  # v = strsplit(brk, "(?<!\\%),(?!\\%)", perl = T)[[1]]
  # v = trimws(v)
  # v = sub('[\'\"](.+)[\'\"]', '\\1', v)

  v = eval(parse(text = sprintf('c(%s)', brk)))

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
brk_numx_init = function(brk, xvalue, bin_close_right) {
  brk = as.numeric(brk)
  brk = setdiff(unique(brk), c(NA, Inf, -Inf))
  xval = unique(xvalue)

  if (isTRUE(bin_close_right)) {
    brk=sort(brk[(brk< max(xval, na.rm=TRUE)) & (brk>=min(xval, na.rm=TRUE))])
  } else {
    brk=sort(brk[(brk<=max(xval, na.rm=TRUE)) & (brk> min(xval, na.rm=TRUE))])
  }

  brk = unique(c(-Inf, brk, Inf))
  return(brk)
}


getarg = function(arg, ...) {
  x = paste0('scorecard.', arg)
  kwargs = list(...)

  arg_val = kwargs[[arg]]
  if (is.null(arg_val)) arg_val = getOption(x)

  return(arg_val)
}

# bin pattern, isbin/left_brkp/leftright_brkp/leftrightbrkp_missing/multibin
binpattern = function(type, bin_close_right = NULL) {
  bin_close_right = getarg('bin_close_right', bin_close_right=bin_close_right)

  pstr_left = list(
    isbin = '\\[',
    left_brkp = "^\\[(.*),.+",
    leftright_brkp = "^\\[(.*), *(.*)\\)",
    leftrightbrkp_missing = "^\\[(.*), *(.*)\\)((%,%missing)*)",
    multibin = "^(\\[.+?,).+,(.+?\\))$"
  )

  pstr_right = list(
    isbin = '\\(',
    left_brkp = "^\\((.*),.+",
    leftright_brkp = "^\\((.*), *(.*)\\]",
    leftrightbrkp_missing = "^\\((.*), *(.*)\\]((%,%missing)*)",
    multibin = "^(\\(.+?,).+,(.+?\\])$"
  )

  if (bin_close_right) {
    pstr_right[[type]]
  } else {
    pstr_left[[type]]
  }
}


get_brkp_bin = function(bin, bin_close_right=NULL) {
  bin_close_right = getarg('bin_close_right', bin_close_right=bin_close_right)

  x = '\\1'
  if (bin_close_right) x = '\\2'
  as.numeric(sub(binpattern('leftright_brkp',bin_close_right), x, bin))
}

# get bin_close_right from bins
get_bcr_bin = function(bins) {
  . = bcr = bin = NULL
  if (inherits(bins, 'list')) bins = rbindlist(bins, fill=TRUE)
  bins[,.(bin)
  ][(grepl('\\[', bin) & grepl('\\)', bin)) | (grepl('\\(', bin) & grepl('\\]', bin))
  ][][grepl('\\[', bin), bcr := !grepl('^\\[', bin)
  ][][!is.na(bcr)
  ][, unique(bcr)]
}


check_bcr = function(bins) {
  bcr = get_bcr_bin(bins)
  bin_close_right = getarg('bin_close_right')
  if (length(bcr) == 1 && (bcr %in% c(TRUE, FALSE)) && bin_close_right != bcr) {
    bin_close_right = bcr
    warning(sprintf("The option bin_close_right was set to %s. Resetting by options(scorecard.bin_close_right = %s).\n", bcr, bcr))
  }
  return(bin_close_right)
}
