#' Scorecard Modeling Report
#'
#' \code{report} creates a scorecard model report and save it as xlsx file.
#'
#' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
#' @param y Name of y variable.
#' @param x Name of x variables. Default is NULL. If x is NULL, then all columns except y are counted as x variables.
#' @param breaks_list A list of break points. It can be extracted from \code{woebin} and \code{woebin_adj} via the argument save_breaks_list.
#' @param special_values The values specified in special_values will be in separate bins. Default is NULL.
#' @param save_report The name of xlsx file where the report is to be saved. Default is 'report'.
#' @param seed A random seed to split input dataframe. Default is 618.
#' @param show_plot The graphics used to evaluate model performance. Default is c('ks', 'roc'). Accepted values are c('ks', 'lift', 'gain', 'roc', 'lz', 'pr', 'f1', 'density').
#' @param bin_num The bins number in gains table.
#' @param positive Value of positive class, default "bad|1".
#' @param points0 Target points, default 600.
#' @param odds0 Target odds, default 1/19. Odds = p/(1-p).
#' @param pdo Points to Double the Odds. Default is 50.
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
#'  personal.status.and.sex=c("female : divorced/separated/married", "male : single",
#'    "male : married/widowed"),
#'  property=c("real estate", "building society savings agreement/ life insurance",
#'    "car or other, not in attribute Savings account/bonds", "unknown / no property"),
#'  age.in.years=c(26, 28, 35, 37),
#'  other.installment.plans=c("bank%,%stores", "none"),
#'  housing=c("rent", "own", "for free")
#'  )
#'
#' report(germancredit, y, x, breaks_list, special_values, seed=618, save_report='report')
#' }
#'
#' @import openxlsx
#' @importFrom stats as.formula glm predict
#' @export
report = function(dt, y, x, breaks_list, special_values=NULL, save_report='report', seed=618, show_plot=c('ks', 'roc'), bin_num=20, positive='bad|1', points0=600, odds0=1/19, pdo=50) {
  info_value = gvif = . = variable = bin = woe = points = NULL

  dt = check_y(dt, y, positive)
  dat_lst = split_df(dt, y, seed = seed)

  # binning
  bins_lst = lapply(dat_lst, function(dat) {
    suppressWarnings(woebin(dat, y = y, x = x, breaks_list = breaks_list, special_values = special_values, print_info=FALSE))
  })
  dat_woe_lst = lapply(dat_lst, function(dat) {
    woebin_ply(dat, bins_lst$train, print_info=FALSE)
  })

  # fitting
  m = glm(as.formula(paste0(y, " ~ .")), family = "binomial",
          data = dat_woe_lst$train[,c(paste0(x,"_woe"),y),with=F])
  pred_lst = lapply(dat_woe_lst, function(dat) {
    predict(m, type='response', dat)
  })

  m_perf = perf_eva(pred = pred_lst, label = lapply(dat_lst, function(x) x$creditability), confusion_matrix = FALSE, show_plot = NULL)

  # scaling
  card <- scorecard(bins_lst$train, m, points0, odds0, pdo, basepoints_eq0 = TRUE)
  score_lst = lapply(dat_lst, function(x) scorecard_ply(x, card, print_step=0L))

  m_psi = perf_psi(score = score_lst, label = lapply(dat_lst, function(x) x$creditability), return_distr_dat = TRUE)
  gains_tbl = gains_table(score = score_lst, label = lapply(dat_lst, function(x) x$creditability), bin_num = bin_num)



  wb <- createWorkbook()
  # dataset information ------
  cat("[INFO] sheet1-dataset information\n")
  sheet  <- addWorksheet(wb, sheetName="dataset information")

  sample_info <- lapply(dat_lst, function(x) {
    data.table(`sample size` = nrow(x),
    `feature size` = ncol(x)-1,
    `bad rate` = sum(x[[y]])/nrow(x))
  })

  writeData(wb, sheet, rbindlist(sample_info, idcol = 'dataset'), startRow=1, startCol=1, colNames=T)


  # model coefficients ------
  cat("[INFO] sheet2-model coefficients\n")
  sheet  <- addWorksheet(wb, sheetName="model coefficients")

  dt_vif = vif(m, merge_coef = TRUE)[, gvif := round(gvif, 4)]
  dt_iv = iv(dat_woe_lst$train[,c(paste0(x,"_woe"), y),with=FALSE], y, order = FALSE)[, info_value := round(info_value, 4)]
  dt_mr = data.table(variable=paste0(x,'_woe'), missing_rate=dt[,x,with=FALSE][, sapply(.SD, function(x) sum(is.na(x))/.N)])

  sum_tbl = Reduce(function(x,y) merge(x,y, all=TRUE, by='variable'), list(dt_vif, dt_iv, dt_mr))
  writeData(wb,sheet, sum_tbl, startRow=1, startCol=1, colNames=T)


  # model performance ------
  cat("[INFO] sheet3-model performance\n")
  sheet  <- addWorksheet(wb, sheetName="model performance")

  eva_tbl = rbindlist(m_perf$binomial_metric, idcol = 'dataset')
  writeData(wb, sheet, eva_tbl, startRow=1, startCol=1, colNames=T)

  perf_eva(pred = pred_lst, label = lapply(dat_lst, function(x) x$creditability), confusion_matrix = FALSE, binomial_metric = NULL, show_plot = show_plot)$pic
  Sys.sleep(2)
  plot_ncol = ceiling(sqrt(length(show_plot)))
  plot_nrow = ceiling(length(show_plot)/plot_ncol)
  insertPlot(wb, sheet, width = 8*plot_ncol, height = 7*plot_nrow, xy = NULL, startRow = nrow(eva_tbl)+4, startCol = 1, fileType = "png", units = "cm")



  # variable binning ------
  cat("[INFO] sheet4-variable woe binning\n")
  sheet  <- addWorksheet(wb, sheetName="variable woe binning")

  ## binning plots
  plist_train <- woebin_plot(bins_lst$train, title = "TRAIN")
  plist_test <- woebin_plot(bins_lst$test, title = "TEST")

  # binning information
  writeData(wb,sheet, "train dataset plots", startCol=1, startRow=1, colNames=F)
  writeData(wb,sheet, "test dataset plots", startCol=8, startRow=1, colNames=F)
  writeData(wb, sheet, "train dataset binning", startCol=15, startRow=1, colNames=F)
  writeData(wb, sheet, "test dataset binning", startCol=28, startRow=1, colNames=F)
  # writeData(wb,sheet, "valid dataset", startCol=50, startRow=1, colNames=F)

  # table
  writeData(wb,sheet, rbindlist(bins_lst$train), startCol=15, startRow=2, colNames=T)
  writeData(wb,sheet, rbindlist(bins_lst$test), startCol=28, startRow=2, colNames=T)
  # picture
  for (i in 1:length(x)) {
    # writeData(wb,sheet, var_exp[variable == x[i]], startCol = 1, startRow = (i-1)*15+2, rowNames = FALSE)

    print(plist_train[[i]])
    insertPlot(wb, sheet, width = 12, height = 7, xy = NULL, startRow = (i-1)*15+4, startCol = 1, fileType = "png", units = "cm")

    print(plist_test[[i]])
    insertPlot(wb, sheet, width = 12, height = 7, xy = NULL, startRow = (i-1)*15+4, startCol = 8, fileType = "png", units = "cm")

    # print(plist_valid[[i]])
    # insertPlot(wb, sheet, width = 12, height = 7, xy = NULL, startRow = (i-1)*15+4, startCol = 15, fileType = "png", units = "cm")

  }


  # scorecard ------
  cat("[INFO] sheet5-scorecard\n")
  sheet  <- addWorksheet(wb, sheetName="scorecard")

  # add scorecard scaling rule
  writeData(wb,sheet, "scorecard scaling", startCol=1, startRow=1, colNames=F)
  writeData(wb,sheet, data.table( c("Target Odds", "Target Points", "Points to Double the Odds"), c(odds0, points0, pdo) ), startCol=1, startRow=2, colNames=F)

  # add scorecard datatable
  writeData(wb,sheet, "scorecard", startCol=1, startRow=7, colNames=F)
  writeData(wb,sheet, rbindlist(card, fill = T)[,.(variable, bin, woe, points)], startCol=1, startRow=8, colNames=T)


  # gains table ------
  cat("[INFO] sheet6-gains table\n")
  sheet  <- addWorksheet(wb, sheetName="gains table")

  setnames(gains_tbl, c('dataset', 'bin', 'count', 'cumulative count', 'good', 'cumulative good', 'bad', 'cumulative bad', 'count distribution', 'bad probability', 'cumulative bad probability', 'approval rate'))
  writeData(wb, sheet, gains_tbl, startCol=1, startRow=1, colNames=T)

  # population stability ------
  cat("[INFO] sheet7-population stability\n")
  sheet  <- addWorksheet(wb, sheetName="population stability")

  psi_tbl = m_psi$dat$score
  setnames(psi_tbl, c('dataset', 'bin', 'count', 'cumulative count', 'good', 'cumulative good', 'bad', 'cumulative bad', 'count distribution', 'bad probability', 'cumulative bad probability', 'approval rate'))
  writeData(wb, sheet, psi_tbl, startCol=1, startRow=1, colNames=T)

  # pic
  print(m_psi$pic$score)
  Sys.sleep(2)
  insertPlot(wb, sheet, width = 16, height = 7, xy = NULL, startRow=nrow(psi_tbl)+4, startCol=1, fileType="png", units= "cm")


  # saving workbook ------
  report_name = sprintf('%s_%s.xlsx', save_report, format(Sys.time(),"%Y%m%d_%H%M%S"))
  saveWorkbook(wb, report_name, overwrite=TRUE)
  cat(sprintf('[INFO] The report is saved as %s\n', report_name))
}
