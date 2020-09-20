#' Scorecard Modeling Report
#'
#' \code{report} creates a scorecard modeling report and save it as a xlsx file.
#'
#' @param dt A data frame or a list of data frames that have both x (predictor/feature) and y (response/label) variables. If there are multiple data frames are provided, only the first data frame would be used for training, and the others would be used for testing/validation.
#' @param y Name of y variable.
#' @param x Name of x variables. Defaults to NULL. If x is NULL, then all columns except y are counted as x variables.
#' @param breaks_list A list of break points. It can be extracted from \code{woebin} and \code{woebin_adj} via the argument save_breaks_list.
#' @param special_values The values specified in special_values will be in separate bins. Defaults to NULL.
#' @param seed A random seed to split input data frame. Defaults to 618. If it is NULL, input dt will not split into two datasets.
#' @param save_report The name of xlsx file where the report is to be saved. Defaults to 'report'.
#' @param positive Value of positive class, default "bad|1".
#' @param ... Additional parameters.
#'
#' @examples
#' \dontrun{
#' data("germancredit")
#'
#' y = 'creditability'
#' x = c(
#'   "status.of.existing.checking.account",
#'   "duration.in.month",
#'   "credit.history",
#'   "purpose",
#'   "credit.amount",
#'   "savings.account.and.bonds",
#'   "present.employment.since",
#'   "installment.rate.in.percentage.of.disposable.income",
#'   "personal.status.and.sex",
#'   "property",
#'   "age.in.years",
#'   "other.installment.plans",
#'   "housing"
#' )
#'
#' special_values=NULL
#' breaks_list=list(
#'  status.of.existing.checking.account=c("... < 0 DM%,%0 <= ... < 200 DM",
#'    "... >= 200 DM / salary assignments for at least 1 year", "no checking account"),
#'  duration.in.month=c(8, 16, 34, 44),
#'  credit.history=c(
#'    "no credits taken/ all credits paid back duly%,%all credits at this bank paid back duly",
#'    "existing credits paid back duly till now", "delay in paying off in the past",
#'    "critical account/ other credits existing (not at this bank)"),
#'  purpose=c("retraining%,%car (used)", "radio/television",
#'    "furniture/equipment%,%domestic appliances%,%business%,%repairs",
#'    "car (new)%,%others%,%education"),
#'  credit.amount=c(1400, 1800, 4000, 9200),
#'  savings.account.and.bonds=c("... < 100 DM", "100 <= ... < 500 DM",
#'    "500 <= ... < 1000 DM%,%... >= 1000 DM%,%unknown/ no savings account"),
#'  present.employment.since=c("unemployed%,%... < 1 year", "1 <= ... < 4 years",
#'    "4 <= ... < 7 years", "... >= 7 years"),
#'  installment.rate.in.percentage.of.disposable.income=c(2, 3),
#'  personal.status.and.sex=c("male : divorced/separated", "female : divorced/separated/married",
#'    "male : single", "male : married/widowed"),
#'  property=c("real estate", "building society savings agreement/ life insurance",
#'    "car or other, not in attribute Savings account/bonds", "unknown / no property"),
#'  age.in.years=c(26, 28, 35, 37),
#'  other.installment.plans=c("bank%,%stores", "none"),
#'  housing=c("rent", "own", "for free")
#'  )
#'
#' # Example I
#' # input dt is a data frame
#' # split input data frame into two
#' report(germancredit, y, x, breaks_list, special_values, seed=618, save_report='report1',
#'   show_plot = c('ks', 'lift', 'gain', 'roc', 'lz', 'pr', 'f1', 'density'))
#'
#' # donot split input data
#' report(germancredit, y, x, breaks_list, special_values, seed=NULL, save_report='report2')
#'
#' # Example II
#' # input dt is a list
#' # only one dataset
#' report(list(dt=germancredit), y, x,
#'   breaks_list, special_values, seed=NULL, save_report='report3')
#'
#' # multiple datasets
#' report(list(dt1=germancredit[sample(1000,500)],
#'             dt2=germancredit[sample(1000,500)]), y, x,
#'  breaks_list, special_values, seed=NULL, save_report='report4')
#'
#' # multiple datasets
#' report(list(dt1=germancredit[sample(1000,500)],
#'             dt2=germancredit[sample(1000,500)],
#'             dt3=germancredit[sample(1000,500)]), y, x,
#'  breaks_list, special_values, seed=NULL, save_report='report5')
#'
#' }
#'
#' @import openxlsx
#' @importFrom stats as.formula glm predict.glm
#' @export
report = function(dt, y, x, breaks_list, special_values=NULL, seed=618, save_report='report', positive='bad|1', ...) {
  # info_value = gvif = . = variable = bin = woe = points = NULL
  . = bin = gvif = info_value = points = variable = woe = points = name = NULL

  kwargs = list(...)
  x_name = kwargs[['x_name']]
  if (!is.null(x_name)) x_name = setDT(list(variable=x, name = x_name))
  # data list
  dat_lst = list()
  if (is.data.frame(dt)) {
    if (is.null(seed)) {
      dat_lst[['dat']] = setDT(copy(dt))
    } else {
      dat_lst = split_df(dt, y, seed = seed)
    }

  } else if ((inherits(dt, 'list') & all(sapply(dt, is.data.frame)))) {
    dat_lst = lapply(dt, setDT)
  } else {
    stop('The input dt should be a data frame, or a list of two data frames.')
  }
  dat_lst = lapply(dat_lst, function(x) check_y(x, y, positive))
  # label list
  label_list = lapply(dat_lst, function(x) x[,y,with=FALSE])


  # binning
  bins_lst = lapply(dat_lst, function(dat) {
    suppressWarnings(woebin(dat, y = y, x = x, breaks_list = breaks_list, special_values = special_values, print_info=FALSE, no_cores = kwargs[['no_cores']]))
  })
  dat_woe_lst = lapply(dat_lst, function(dat) {
    woebin_ply(dat, bins_lst[[1]], print_info=FALSE, no_cores = kwargs[['no_cores']])
  })

  # fitting
  m = glm(as.formula(paste0(y, " ~ .")), family = "binomial",
          data = dat_woe_lst[[1]][,c(paste0(x,"_woe"),y),with=F])
  pred_lst = lapply(dat_woe_lst, function(d) {
    predict.glm(m, newdata = d, type='response')
  })

  binomial_metric = c("mse", "rmse", "logloss", "r2", "ks", "auc", "gini")
  if ('binomial_metric' %in% names(kwargs)) binomial_metric = kwargs$binomial_metric
  m_perf = perf_eva(pred = pred_lst, label = label_list, binomial_metric=binomial_metric, confusion_matrix = FALSE, show_plot = NULL)

  # scaling
  card <- do.call( scorecard, args = c(
    list(bins=bins_lst[[1]], model=m),
    kwargs[intersect(c('points0', 'odds0', 'pdo', 'basepoints_eq0'), names(kwargs))] ) )
  score_lst = lapply(dat_lst, function(x) scorecard_ply(x, card, print_step=0L))


  bin_num = ifelse('bin_num' %in% names(kwargs), kwargs$bin_num, 10)
  bin_type = ifelse('bin_type' %in% names(kwargs), kwargs$bin_type, 'freq')
  gains_tbl = gains_table(score = rbindlist(score_lst), label = rbindlist(label_list), bin_num = bin_num, bin_type=bin_type)
  gains_table_cols = c('dataset', 'bin', 'count', 'cumulative count', 'good', 'cumulative good', 'bad', 'cumulative bad', 'count distribution', 'bad probability', 'approval rate', 'cumulative bad probability')


  wb <- createWorkbook()
  # dataset information ------
  n = 1
  cat(sprintf("[INFO] sheet%s-dataset information\n", n))
  sheet  <- addWorksheet(wb, sheetName="dataset information")

  sample_info <- lapply(dat_lst, function(x) {
    data.table(`sample size` = nrow(x),
               `feature size` = ncol(x)-1,
               `bad rate` = sum(x[[y]])/nrow(x))
  })
  dt_dtinfo = rbindlist(sample_info, idcol = 'dataset')
  dt_xdea = var_describe(rbindlist(dat_lst))

  writeData(wb, sheet, dt_dtinfo, startRow=1, startCol=1, colNames=T)
  writeData(wb, sheet, dt_xdea, startRow=nrow(dt_dtinfo)+4, startCol=1, colNames=T)

  # model coefficients ------
  n = n+1
  cat(sprintf("[INFO] sheet%s-model coefficients\n", n))
  sheet  <- addWorksheet(wb, sheetName="model coefficients")

  dt_vif = vif(m, merge_coef = TRUE)[, gvif := round(gvif, 4)]
  dt_iv = iv(dat_woe_lst[[1]][,c(paste0(x,"_woe"), y),with=FALSE], y, order = FALSE)[, info_value := round(info_value, 4)]
  # dt_mr = data.table(variable=paste0(x,'_woe'), missing_rate=dat_lst[[1]][,x,with=FALSE][, sapply(.SD, function(x) sum(is.na(x))/.N)])

  sum_tbl = Reduce(
    function(x,y) merge(x,y, all.x=TRUE, by='variable', sort=FALSE), list(dt_vif, dt_iv, copy(dt_xdea)[, variable := paste0(variable, '_woe')])
  )[, variable := sub('_woe$', '', variable)]
  if (!is.null(x_name)) sum_tbl = merge(x_name, sum_tbl, by = 'variable', sort = FALSE, all = TRUE)[c(.N, seq_len(.N-1)),]

  writeData(wb,sheet, sprintf('Model coefficients based on %s dataset', names(dat_lst)[1]), startRow=1, startCol=1, colNames=F)
  writeData(wb,sheet, sum_tbl, startRow=2, startCol=1, colNames=T)



  # model performance ------
  n = n+1
  cat(sprintf("[INFO] sheet%s-model performance\n", n))
  sheet  <- addWorksheet(wb, sheetName="model performance")

  eva_tbl = rbindlist(m_perf$binomial_metric, idcol = 'dataset')
  writeData(wb, sheet, eva_tbl, startRow=1, startCol=1, colNames=T)

  show_plot = c("ks","roc")
  if ('show_plot' %in% names(kwargs)) show_plot = kwargs$show_plot
  perf_eva(pred = pred_lst, label = label_list, confusion_matrix = FALSE, binomial_metric = NULL, show_plot = show_plot)$pic
  Sys.sleep(2)
  plot_ncol = ceiling(sqrt(length(show_plot)))
  plot_nrow = ceiling(length(show_plot)/plot_ncol)
  insertPlot(wb, sheet, width = 8*plot_ncol, height = 7*plot_nrow, xy = NULL, startRow = nrow(eva_tbl)+4, startCol = 1, fileType = "png", units = "cm")



  # variable binning ------
  n = n+1
  cat(sprintf("[INFO] sheet%s-variable woe binning\n", n))
  sheet  <- addWorksheet(wb, sheetName="variable woe binning")

  names_dat = names(dat_lst)
  for (i in seq_along(names_dat)) {
    di = names_dat[i]
    # title row
    writeData(wb,sheet, sprintf('graphics of %s dataset', di), startRow=1, startCol=7*(i-1)+1, colNames=F)
    writeData(wb, sheet, sprintf('binning of %s dataset', di), startRow=1, startCol=7*length(names_dat)+1+14*(i-1), colNames=F)

    # binning
    binning_df = rbindlist(bins_lst[[i]])
    if (!is.null(x_name)) binning_df = merge(x_name, binning_df, by = 'variable', sort=FALSE, all = TRUE)
    writeData(wb,sheet, binning_df,
              startRow=2, startCol=7*length(names_dat)+1+14*(i-1), colNames=T)
  }


  # plots
  for (i in seq_along(names_dat)) {
    di = names_dat[i]
    plist = woebin_plot(bins_lst[[di]], title = di)

    for (j in seq_along(x)) {
      if (!is.null(x_name)) writeData(wb,sheet, x_name[variable == x[j], name], startRow = (j-1)*15+3, startCol = 7*(i-1)+1, rowNames = FALSE)
      print(plist[[j]])
      insertPlot(wb, sheet, width = 12, height = 7, xy = NULL,
                 startRow = (j-1)*15+4, startCol = 7*(i-1)+1,
                 fileType = "png", units = "cm")
    }
  }


  # scorecard ------
  n = n+1
  cat(sprintf("[INFO] sheet%s-scorecard\n", n))
  sheet  <- addWorksheet(wb, sheetName="scorecard")

  odds0 = ifelse('odds0' %in% names(kwargs), kwargs$odds0, 1/19)
  points0 = ifelse('points0' %in% names(kwargs), kwargs$points0, 600)
  pdo = ifelse('pdo' %in% names(kwargs), kwargs$pdo, 50)

  # add scorecard scaling rule
  writeData(wb,sheet, "scorecard scaling", startCol=1, startRow=1, colNames=F)
  writeData(wb,sheet, data.table( c("Target Odds", "Target Points", "Points to Double the Odds"), c(odds0, points0, pdo) ), startCol=1, startRow=2, colNames=F)

  # add scorecard datatable
  writeData(wb,sheet, "scorecard", startCol=1, startRow=7, colNames=F)
  card_df = rbindlist(card, fill = T)[,.(variable, bin, woe, points)]
  if (!is.null(x_name)) card_df = merge(x_name, card_df, by = 'variable', sort=FALSE, all = TRUE)[c(.N, seq_len(.N-1)),]
  writeData(wb,sheet, card_df, startCol=1, startRow=8, colNames=T)


  # population stability ------
  if (length(dat_lst) > 1) {
    n = n+1
    cat(sprintf("[INFO] sheet%s-population stability\n", n))
    sheet  <- addWorksheet(wb, sheetName="population stability")

    m_psi = perf_psi(score = score_lst, label = label_list, return_distr_dat = TRUE)

    # table in equal width
    psi_tbl = m_psi$dat[[1]]
    setnames(psi_tbl, gains_table_cols)
    writeData(wb, sheet, psi_tbl, startCol=1, startRow=1, colNames=T)
    # pic
    for (i in seq_len(length(dat_lst)-1)) {
      if (length(dat_lst)>2) {
        print(m_psi$pic$score[[i]])
      } else print(m_psi$pic$score)
      Sys.sleep(2)
      insertPlot(wb, sheet, width = 16, height = 7, xy = NULL, startRow=nrow(psi_tbl)+4+15*(i-1), startCol=1, fileType="png", units= "cm")
    }
  }


  # gains table ------
  n = n+1
  cat(sprintf("[INFO] sheet%s-gains table\n", n))
  sheet  <- addWorksheet(wb, sheetName="gains table")

  setnames(gains_tbl, gains_table_cols)
  writeData(wb, sheet, gains_tbl, startCol=1, startRow=1, colNames=T)

  # saving workbook ------
  report_name = sprintf('%s_%s.xlsx', save_report, format(Sys.time(),"%Y%m%d_%H%M%S"))
  saveWorkbook(wb, report_name, overwrite=TRUE)
  cat(sprintf('[INFO] The report is saved as %s\n', report_name))
}
