# renamed as perf_eva
#
# The function perf_plot has renamed as perf_eva.
#
# @param label Label values, such as 0s and 1s, 0 represent for good and 1 for bad.
# @param pred Predicted probability values.
# @param title Title of plot, default "train".
# @param groupnum The group number when calculating bad probability, default NULL.
# @param type Types of performance plot, such as "ks", "lift", "roc", "pr". Default c("ks", "roc").
# @param show_plot Logical value, default TRUE. It means whether to show plot.
# @param seed An integer. The specify seed is used for random sorting data, default: 186.
perf_plot = function(label, pred, title=NULL, groupnum=NULL, type=c("ks", "roc"), show_plot=TRUE, seed=186) {
  stop("This function has renamed as perf_eva.")
}



eva_dfkslift = function(df, groupnum = NULL) {
  # global variables
  pred=group=.=label=good=bad=ks=cumbad=cumgood=NULL

  if (is.null(groupnum)) groupnum = nrow(df)

  df_kslift = df[
    order(-pred)
  ][, group := ceiling(.I/(.N/groupnum))
  ][, .(good = sum(label==0), bad = sum(label==1)), by=group
  ][, `:=`(
    group = .I/.N,
    good_distri=good/sum(good), bad_distri=bad/sum(bad),
    badrate=bad/(good+bad), cumbadrate = (cumsum(bad)/cumsum(good+bad)),
    lift = (cumsum(bad)/cumsum(good+bad))/(sum(bad)/sum(good+bad)),
    cumgood=cumsum(good)/sum(good), cumbad=cumsum(bad)/sum(bad))
  ][, ks := abs(cumbad - cumgood)]

  df_kslift = rbind(data.table(group=0, good=0, bad=0, good_distri=0, bad_distri=0, badrate=0, cumbadrate=0, lift=0, cumgood=0, cumbad=0, ks=0), df_kslift)

  return(df_kslift)
}
eva_pks = function(dfkslift, title) {
  # global variables
  ks=group=.=cumgood=cumbad=value=variable=NULL

  dfks = dfkslift[ks == max(ks)][order(group)][1]

  pks = ggplot(melt(dfkslift[,.(group, cumgood, cumbad, ks)], id="group"), aes(x=group, y=value, colour=variable)) +
    geom_line() + coord_fixed() +
    geom_segment(aes(x = dfks$group, y = 0, xend = dfks$group, yend = dfks$ks), colour = "red", linetype = "dashed", arrow=arrow(ends="both", length=unit(.2,"cm"))) +
    labs(x = "% of population", y = "% of total Good/Bad") +
    # annotate("text", x=0.50, y=Inf, label="K-S", vjust=1.5, size=6)+
    ggtitle(paste0(title, 'K-S')) +
    annotate("text", x=dfks$group, y=dfks$ks, vjust = -0.2, label=paste0("KS: ", round(dfks$ks,4) ), colour = "blue") +
    annotate("text", x=0.20, y=0.80, vjust = -0.2, label="Bad", colour = "black") +
    annotate("text", x=0.80, y=0.55, vjust = -0.2, label="Good", colour = "black") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_colour_manual(values=c("black", "black", "blue")) +
    theme_bw() + theme(legend.position="none")

  return(pks)
}
eva_plift = function(dfkslift, title) {
  # global variables
  .=group=bad=model=badrate=cumbadrate=good=NULL
  badrate_avg = dfkslift[,sum(bad)/sum(good+bad)]

  plift = ggplot( dfkslift[-1][,.(group, cumbadrate, badrate)], aes(x=group) ) +
    # geom_line(aes(y=badrate), stat = "identity", fill=NA, colour = "black") +
    geom_line(aes(y=cumbadrate)) +
    # geom_point(aes(y=cumbadrate), shape=21, fill="white") +
    coord_fixed() +
    geom_segment(aes(x = 0, y = badrate_avg, xend = 1, yend = badrate_avg), colour = "red", linetype = "dashed") +
    labs(x="% of population", y="% of Bad") +
    # annotate("text", x=0.50,y=Inf, label="Lift", vjust=1.5, size=6)+
    ggtitle(paste0(title, 'Lift')) +
    annotate("text", x=0.75,y=mean(dfkslift$cumbadrate), label="cumulate badrate", vjust=1)+
    annotate("text", x=0.75,y=badrate_avg, label="average badrate", vjust=1, colour="red")+
    guides(fill=guide_legend(title=NULL)) +
    scale_fill_manual(values=c("white", "grey")) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) + theme_bw() +
    theme(legend.position=c(0.5, 0.9),legend.direction="horizontal")


  return(plift)
}

eva_dfrocpr = function(df) {
  # global variables
  pred=.=label=countP=countN=FN=TN=TP=FP=precision=recall=NULL

  dfrocpr = df[
    order(pred)
  ][, .(countpred = .N, countP = sum(label==1), countN = sum(label==0)), by=pred
  ][, `:=`(FN = cumsum(countP), TN = cumsum(countN) )
  ][, `:=`(TP = sum(countP) - FN, FP = sum(countN) - TN)
  ][, `:=`(TPR = TP/(TP+FN), FPR = FP/(TN+FP), precision = TP/(TP+FP), recall = TP/(TP+FN))
  ][, `:=`(F1 = 2*precision*recall/(precision+recall))]

  return(dfrocpr)
}
eva_proc = function(dfrocpr, title) {
  # global variables
  FPR=TPR=NULL

  auc = dfrocpr[order(FPR,TPR)][, sum(
    (TPR+shift(TPR, fill=0, type="lag"))/2*(FPR-shift(FPR, fill=0, type="lag"))
  )]

  proc = ggplot(dfrocpr, aes(x=FPR, y=TPR)) +
    geom_ribbon(aes(ymin=0, ymax=TPR), fill="blue", alpha=0.1) +
    geom_line() + coord_fixed() +
    geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype = "dashed", colour="red") +
    # annotate("text", x = 0.5, y=Inf, label="ROC", vjust=1.5, size=6) +
    ggtitle(paste0(title, 'ROC')) +
    annotate("text", x=0.55, y=0.45, label=paste0("AUC: ", round(auc,4)), colour = "blue") +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    theme_bw()

  return(proc)
}
eva_ppr = function(dfrocpr, title) {
  # global variables
  recall=precision=NULL

  ppr = ggplot(dfrocpr, aes(x=recall, y=precision)) +
    geom_line() + coord_fixed() +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour = "red", linetype="dashed") +
    labs(x = "Recall", y = "Precision") +
    # annotate("text", x = 0.5, y=Inf, label="P-R", vjust=1.5, size=6) +
    ggtitle(paste0(title, 'P-R')) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    theme_bw()

  return(ppr)
}
eva_pf1 = function(dfrocpr, title) {
  # # break-even point
  # BEP = dfrocpr[precision == recall][,precision]
  # F1： 1/F1 = 1/2*(1/P+1/R)
  # F_beta: 1/F_beta = 1/(1+beta^2)*(1/P+beta^2/R)

  # global variables
  pop=countpred=F1=pred=NULL
  dfrocpr = dfrocpr[, pop := cumsum(countpred)/sum(countpred)]
  # xy of F1 max
  F1max_pop = dfrocpr[F1==max(F1,na.rm = TRUE),pop]
  F1max_F1 = dfrocpr[F1==max(F1,na.rm = TRUE),F1]
  # pred
  pred_0=dfrocpr[1,pred]
  pred_1=dfrocpr[.N,pred]
  pred_F1max=dfrocpr[F1==F1max_F1,pred]
  if ( !(mean(dfrocpr$pred, na.rm=TRUE)<=1 & mean(dfrocpr$pred, na.rm=TRUE)>=0) ) {
    pred_0 = -pred_0
    pred_1 = -pred_1
    pred_F1max = -pred_F1max
  }


  pf1 = ggplot(dfrocpr) +
    geom_line(aes(x=pop, y=F1)) + coord_fixed() +
    geom_segment(aes(x = F1max_pop, y = 0, xend = F1max_pop, yend = F1max_F1), colour = "red", linetype="dashed") +
    labs(x = "% of population", y = "F1") +
    # annotate("text", x = 0.5, y=Inf, label="F1", vjust=1.5, size=6) +
    ggtitle(paste0(title, 'F1')) +
    annotate('text', x=0, y=0, label=paste0('pred\n',round(pred_0,4)), vjust=-0.2, hjust=0) +
    annotate('text', x=1, y=0, label=paste0('pred\n',round(pred_1,4)), vjust=-0.2, hjust=1) +
    annotate('text', x=dfrocpr[F1==max(F1,na.rm = TRUE),pop], y=0, label=paste0('pred\n',round(pred_F1max,4)), vjust=-0.2) +
    annotate(
      'text', x=dfrocpr[F1==max(F1,na.rm = TRUE),pop],
      y=dfrocpr[F1==max(F1,na.rm = TRUE),F1],
      label=paste0('F1 max: \n',dfrocpr[F1==max(F1,na.rm = TRUE),round(F1,4)]), vjust=-0.2, colour='blue') +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    theme_bw()

  return(pf1)
}
#' KS, ROC, Lift, PR
#'
#' \code{perf_eva} provides performance evaluations, such as kolmogorov-smirnow(ks), ROC, lift and precision-recall curves, based on provided label and predicted probability values.
#'
#' @name perf_eva
#' @param label Label values, such as 0s and 1s, 0 represent for good and 1 for bad.
#' @param pred Predicted probability or score.
#' @param title Title of plot, default is NULL.
#' @param groupnum The group number when calculating KS.  Default NULL, which means the number of sample size.
#' @param type Types of performance plot, such as "ks", "lift", "roc", "pr". Default c("ks", "roc").
#' @param show_plot Logical value, default is TRUE. It means whether to show plot.
#' @param positive Value of positive class, default is "bad|1".
#' @param seed Integer, default is 186. The specify seed is used for random sorting data.
#' @return ks, roc, lift, pr
#'
#' @details
#' Accuracy = true positive and true negative/total cases
#'
#' Error rate = false positive and false negative/total cases
#'
#' TPR, True Positive Rate(Recall or Sensitivity) = true positive/total actual positive
#'
#' PPV, Positive Predicted Value(Precision) = true positive/total predicted positive
#'
#' TNR, True Negative Rate(Specificity) = true negative/total actual negative
#'
#' NPV, Negative Predicted Value = true negative/total predicted negative
#'
#'
#'
#'
#'
# https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values
# ROC curve: Sensitivity ~ 1-Specificity with different threshold
# Lift chart: Lift(PV+/p1) ~ Depth with different threshold
# Gains chart: PV + ~ Depth with different threshold
#'
#' @seealso \code{\link{perf_psi}}
#'
#' @examples
#' \dontrun{
#' # load germancredit data
#' data("germancredit")
#'
#' # filter variable via missing rate, iv, identical value rate
#' dt_sel = var_filter(germancredit, "creditability")
#'
#' # woe binning ------
#' bins = woebin(dt_sel, "creditability")
#' dt_woe = woebin_ply(dt_sel, bins)
#'
#' # glm ------
#' m1 = glm( creditability ~ ., family = binomial(), data = dt_woe)
#' # summary(m1)
#'
#' # Select a formula-based model by AIC
#' m_step = step(m1, direction="both", trace=FALSE)
#' m2 = eval(m_step$call)
#' # summary(m2)
#'
#' # predicted proability
#' dt_pred = predict(m2, type='response', dt_woe)
#'
#' # performance ------
#' # Example I # only ks & auc values
#' perf_eva(dt_woe$creditability, dt_pred, show_plot=FALSE)
#'
#' # Example II # ks & roc plot
#' perf_eva(dt_woe$creditability, dt_pred)
#'
#' # Example III # ks, lift, roc & pr plot
#' perf_eva(dt_woe$creditability, dt_pred, type = c("ks","lift","roc","pr"))
#' }
#' @import data.table ggplot2 gridExtra
#' @export
#'
perf_eva = function(label, pred, title=NULL, groupnum=NULL, type=c("ks", "roc"), show_plot=TRUE, positive="bad|1", seed=186) {
  # global variables
  FPR = TPR = cumbad = group = ks = NULL

  # inputs checking
  if (is.factor(label)) label = as.character(label)
  if (!(is.vector(label) & is.vector(pred) & length(label) == length(pred))) stop("Incorrect inputs; label and pred should be vectors with the same length.")
  # if pred is score
  if ( !(mean(pred, na.rm=TRUE)<=1 & mean(pred, na.rm=TRUE)>=0) ) {
    warning("Since the average of pred is not in [0,1], it is treated as predicted score but not probability.")
    pred = -pred
  }
  # random sort datatable
  set.seed(seed)
  df = data.table(label=label, pred=pred)[sample(.N)]
  # remove NAs
  if (anyNA(label) || anyNA(pred)) {
    warning("The NAs in \"label\" or \"pred\" were removed.")
    df = df[!is.na(label) & !is.na(pred)]
  }
  # check label
  df = check_y(df, 'label', positive)
  # title
  if (!is.null(title)) title = paste0(title,': ')


  ### data ###
  # dfkslift ------
  if (any(c("ks","lift") %in% type)) {
    df_kslift = eva_dfkslift(df, groupnum)
    df_string1 = paste0('df_',intersect(c("ks","lift"), type), '=df_kslift')
    eval(parse(text=df_string1))
  }
  # dfrocpr ------
  if (any(c("roc","pr",'f1') %in% type)) {
    df_rocpr = eva_dfrocpr(df)
    df_string2 = paste0('df_',intersect(c("roc","pr",'f1'), type), '=df_rocpr')
    eval(parse(text=df_string2))
  }


  ###  return list ###
  rt = list()
  # KS ------
  if ("ks" %in% type) rt$KS = df_kslift[ks == max(ks)][order(group)][1,round(ks, 4)]
  # ROC ------
  if ("roc" %in% type) {
    auc = df_rocpr[order(FPR,TPR)][, sum(
      (TPR+shift(TPR, fill=0, type="lag"))/2*(FPR-shift(FPR, fill=0, type="lag"))
    )]
    # return list
    rt$AUC = round(auc,4)

    # gini
    # gini = dfkslift[, sum(
    #   (cumbad+shift(cumbad, fill=0, type="lag"))*(group-shift(group, fill=0, type="lag")) )]-1
    rt$Gini = round(2*auc-1, 4)
  }


  ###  export plot
  if (show_plot == TRUE) {
    plist = paste0(paste0('p',type,"=eva_p", type, '(df_',type,',title)'), collapse = ',')
    plist = eval(parse(text=paste0("list(",plist,")")))
    p_nrows = floor(length(type)/2)

    args.list <- c(plist, list(nrow=p_nrows, padding = 0))
    rt$pic = do.call(grid.arrange, args.list)
  }

  return(rt)
}

#' PSI
#'
#' \code{perf_psi} calculates population stability index (PSI) and provides credit score distribution based on credit score datasets.
#'
#' @param score A list of credit score for actual and expected data samples. For example, score = list(actual = score_A, expect = score_E), both score_A and score_E are dataframes with the same column names.
#' @param label A list of label value for actual and expected data samples. The default is NULL. For example, label = list(actual = label_A, expect = label_E), both label_A and label_E are vectors or dataframes. The label values should be 0s and 1s, 0 represent for good and 1 for bad.
#' @param title Title of plot, default is NULL.
#' @param x_limits x-axis limits, default is NULL.
#' @param x_tick_break x-axis ticker break, default is 50.
#' @param show_plot Logical, default is TRUE. It means whether to show plot.
#' @param return_distr_dat Logical, default is FALSE.
#' @param seed Integer, default is 186. The specify seed is used for random sorting data.
#'
#' @return a dataframe of psi & plots of credit score distribution
#'
#' @details The population stability index (PSI) formula is displayed below: \deqn{PSI = \sum((Actual\% - Expected\%)*(\ln(\frac{Actual\%}{Expected\%}))).} The rule of thumb for the PSI is as follows: Less than 0.1 inference insignificant change, no action required; 0.1 - 0.25 inference some minor change, check other scorecard monitoring metrics; Greater than 0.25 inference major shift in population, need to delve deeper.
#'
#' @seealso \code{\link{perf_eva}}
#'
#' @examples
#' \dontrun{
#' # load germancredit data
#' data("germancredit")
#'
#' # filter variable via missing rate, iv, identical value rate
#' dt_sel = var_filter(germancredit, "creditability")
#'
#' # breaking dt into train and test ------
#' dt_list = split_df(dt_sel, "creditability", ratio = 0.6, seed=21)
#' dt_train = dt_list$train; dt_test = dt_list$test
#'
#' # woe binning ------
#' bins = woebin(dt_train, "creditability")
#'
#' # converting train and test into woe values
#' train = woebin_ply(dt_train, bins)
#' test = woebin_ply(dt_test, bins)
#'
#' # glm ------
#' m1 = glm(creditability ~ ., family = binomial(), data = train)
#' # summary(m1)
#'
#' # Select a formula-based model by AIC
#' m_step = step(m1, direction="both", trace=FALSE)
#' m2 = eval(m_step$call)
#' # summary(m2)
#'
#' # predicted proability
#' train_pred = predict(m2, type='response', train)
#' test_pred = predict(m2, type='response', test)
#'
#' # # ks & roc plot
#' # perf_eva(train$creditability, train_pred, title = "train")
#' # perf_eva(test$creditability, test_pred, title = "test")
#'
#' #' # scorecard
#' card = scorecard(bins, m2)
#'
#' # credit score, only_total_score = TRUE
#' train_score = scorecard_ply(dt_train, card)
#' test_score = scorecard_ply(dt_test, card)
#'
#' # Example I # psi
#' psi = perf_psi(
#'   score = list(train = train_score, test = test_score),
#'   label = list(train = train$creditability, test = test$creditability)
#' )
#' # psi$psi  # psi dataframe
#' # psi$pic  # pic of score distribution
#'
#' # Example II # specifying score range
#' psi_s = perf_psi(
#'   score = list(train = train_score, test = test_score),
#'   label = list(train = train$creditability, test = test$creditability),
#'   x_limits = c(200, 750),
#'   x_tick_break = 50
#'   )
#'
#' # Example III # credit score, only_total_score = FALSE
#' train_score2 = scorecard_ply(dt_train, card, only_total_score=FALSE)
#' test_score2 = scorecard_ply(dt_test, card, only_total_score=FALSE)
#'
#' # psi
#' psi2 = perf_psi(
#'   score = list(train = train_score2, test = test_score2),
#'   label = list(train = train$creditability, test = test$creditability)
#' )
#' # psi2$psi  # psi dataframe
#' # psi2$pic  # pic of score distribution
#' }
#' @import data.table ggplot2 gridExtra
#' @export
#'
perf_psi = function(score, label=NULL, title=NULL, x_limits=NULL, x_tick_break=50, show_plot=TRUE, seed=186, return_distr_dat = FALSE) {
  # psi = sum((Actual% - Expected%)*ln(Actual%/Expected%))

  # global variables
  . = A = ae = E = PSI = bad = badprob = badprob2 = bin = bin1 = bin2 = count = distr = logAE = midbin = test = train = y = NULL
  # return list
  rt = rt_psi = rt_pic = rt_dat = list()



  # inputs checking
  # score
  if (!is.list(score)) {
    stop("Incorrect inputs; score should be a list.")
  } else {
    if (length(score) != 2) {
      stop("Incorrect inputs; the length of score should be 2.")
    } else {
      if (!is.data.frame(score[[1]]) || !is.data.frame(score[[2]])) stop("Incorrect inputs; score is a list of two dataframes.")

      if (!identical( names(score[[1]]), names(score[[2]]) )) stop("Incorrect inputs; the column names of two dataframes in score should be the same.")
    }
  }

  # label
  if ( !is.null(label) ) {
    if (!is.list(label)) {
      stop("Incorrect inputs; label should be a list.")
    } else {
      if (length(label) != 2) {
        stop("Incorrect inputs; the length of label should be 2.")
      } else {
        if (!identical(names(score), names(label))) stop("Incorrect inputs; the names of score and label should be the same. ")

        if (is.data.frame(label[[1]])) {
          if (ncol(label[[1]]) == 1) {
            label[[1]] = label[[1]][[1]]
          } else (
            stop("Incorrect inputs; the number of columns in label should be 1.")
          )
        }
        if (is.data.frame(label[[2]])) {
          if (ncol(label[[2]]) == 1) {
            label[[2]] = label[[2]][[1]]
          } else {
            stop("Incorrect inputs; the number of columns in label should be 1.")
          }
        }
      }
    }
  }

  # score dataframe column names
  score_names = names(score[[1]])

  # dataframe with score & label
  for (i in names(score)) {
    if (!is.null(label)) {
      score[[i]]$y = label[[i]]
    } else {
      score[[i]]$y = NA
    }
  }
  # dateset of score and label
  dt_sl = cbind(rbindlist(score, idcol = "ae")) # ae refers to 'Actual & Expected'

  # PSI function
  psi = function(dat) {
    # dat = copy(dat)[,y:=NULL][complete.cases(dat),]
    AE = bin_PSI = NULL

    # dataframe of bin, actual, expected
    dt_bae = dcast(
      dat[,.(count=.N), keyby=c("ae", "bin")
        ],#[,distr := count/sum(count), by="ae"][],
      bin ~ ae, value.var="count", fill = 0.9
    )
    # dt_bae[dt_bae == 0] = 0.9 # replace 0 by 0.9

    names_ae = setdiff(names(dt_bae), "bin")
    psi_dt = dt_bae[
      , (c("A","E")) := lapply(.SD, function(x) x/sum(x)), .SDcols = names_ae
    ][, `:=`(AE = A-E, logAE = log(A/E))
    ][, `:=`(bin_PSI = AE*logAE)
    ][][, sum(bin_PSI)]

    return(psi_dt)
  }


  set.seed(seed)
  for ( sn in score_names ) {
    # data manipulation to calculating psi and plot
    if (length(unique(dt_sl[[sn]])) > 10) {
      if (is.null(x_limits)) {
        x_limits = quantile(dt_sl[[sn]], probs=c(0.02,0.98))
        x_limits = round(x_limits/x_tick_break)*x_tick_break
      }
      # breakpoints
      brkp = unique(c(
        floor(min(dt_sl[[sn]])/x_tick_break)*x_tick_break,
        seq(x_limits[1], x_limits[2], by=x_tick_break),
        ceiling(max(dt_sl[[sn]])/x_tick_break)*x_tick_break
      ))

      # random sort datatable
      dat = dt_sl[sample(1:nrow(dt_sl))][, c("ae", "y", sn), with = FALSE]

      dat$bin = cut(dat[[sn]], brkp, right = FALSE, dig.lab = 10, ordered_result = F)

    } else {
      # random sort datatable
      dat = dt_sl[sample(1:nrow(dt_sl))][, c("ae", "y", sn), with = FALSE]
      dat$bin = factor(dat[[sn]])

    }


    # psi ------
    # rt[[paste0(sn, "_psi")]] = round(psi(dat), 4)
    rt_psi[[sn]] = data.frame(PSI = psi(dat))#round(psi(dat), 4))



    # plot ------
    # distribution of scorecard probability
    if (show_plot) {
      # score distribution and bad probability
      distr_prob = dat[
        order(bin)
        ][,.(count=.N, bad=sum(y==1)), keyby=c("ae", "bin")
          ][,`:=`(
            distr = count/sum(count),
            badprob=bad/count
          ), by = "ae"][,`:=`(badprob2=badprob*max(distr)), by = "ae"][, `:=`(
            bin1 = as.integer(sub("\\[(.+),(.+)\\)", "\\1", bin)),
            bin2 = as.integer(sub("\\[(.+),(.+)\\)", "\\2", bin))
          )][, midbin := (bin1+bin2)/2 ]


      # plot
      p_score_distr =
        ggplot(distr_prob) +
        geom_bar(aes(x=bin, y=distr, fill=ae), alpha=0.6, stat="identity", position="dodge") +
        geom_line(aes(x=bin, y=badprob2, group=ae, colour=ae, linetype=ae)) +
        geom_point(aes(x=bin, y=badprob2, colour=ae), shape=21, fill="white") +
        guides(fill=guide_legend(title="Distribution"), colour=guide_legend(title="Probability"), linetype=guide_legend(title="Probability")) +
        scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~./max(distr_prob$distr), name = "Bad probability")) +
        labs(x=NULL, y="Score distribution") +
        theme_bw() +
        # theme(legend.position="bottom", legend.direction="horizontal") +
        theme(plot.title=element_text(vjust = -2.5), legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_blank())


      if (!is.null(title)) {
        p_score_distr = p_score_distr + ggtitle(paste0(title, " PSI: ", round(psi(dat), 4)))
      } else {
        p_score_distr = p_score_distr + ggtitle(paste0(sn, "_PSI: ", round(psi(dat), 4)))
      }


      # return of pic
      rt_pic[[sn]] = p_score_distr

      if (return_distr_dat) {
        rt_dat[[sn]] = dcast(
          distr_prob[,.(ae=factor(ae,levels=dt_sl[,unique(ae)]),bin,count,bad,badprob)],
          bin ~ ae, value.var=c("count","bad","badprob"), sep="_"
        )[,c(1,2,4,6,3,5,7)]
      }
    } # end of show plot
  } # end of for loop


  # return
  rt$psi = rbindlist(rt_psi, idcol = "variable")
  rt$pic = rt_pic
  if (return_distr_dat) rt$dat = rt_dat

  return(rt)
}
