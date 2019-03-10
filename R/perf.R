# evaluation metric ------
# dt = data.table(label=label, pred=pred)[!(is.na(label) | is.na(pred))]
## mean squared error, MSE
mse = function(dt, ...) {
  label = pred = NULL
  setDT(dt)
  return(dt[,mean((label-pred)^2)])
}
## root mean squared error, RMSE
rmse = function(dt, ...) {
  setDT(dt)
  return(sqrt(mse(dt)))
}

## mean absolute error, MAE
mae = function(dt, ...) {
  label = pred = NULL
  setDT(dt)
  return(dt[,mean(abs(label-pred))])
}

## mean square logarithmic error, MSLE
msle = function(dt, ...) {
  label = pred = NULL
  setDT(dt)
  return(dt[,mean((log1p(label)-log1p(pred))^2)]) # log1p = log(1+x)
}
## root mean square logarithmic error, RMSLE
rmsle = function(dt, ...) {
  setDT(dt)
  return(sqrt(msle(dt)))
}

## coefficient of determination, R2
r2 = function(dt, ...) {
  label = pred = NULL
  setDT(dt)
  # total sum of squares, SST
  sst = dt[, sum((label-mean(label))^2)]
  ## regression sum of squares, SSR
  sse = dt[, sum((label-pred)^2)]
  # ## error sum of squares, SSE
  # ssr = dt[, sum((pred-mean(label))^2)]
  return(1-sse/sst)
}

## log loss for binary classification
## http://wiki.fast.ai/index.php/Log_Loss
logloss = function(dt, ...) {
  ll = label = pred = NULL
  setDT(dt)
  return(dt[,ll:=label*log(pred)+(1-label)*log(1-pred)][,mean(-ll)])
}


## aera under curve (ROC), AUC
auc = function(dt_ev, ...) {
  . = FPR = TPR = NULL
  rbind(dt_ev[,.(FPR,TPR)], data.table(FPR=0:1,TPR=0:1), fill=TRUE
  )[order(FPR,TPR)
    ][, sum((TPR+shift(TPR, fill=0, type="lag"))/2*(FPR-shift(FPR, fill=0, type="lag")))]
}
## Gini
gini = function(dt_ev, ...) {
  2*auc(dt_ev)-1
}
## KS
ks = function(dt_ev, ...) {
  TN = totN = FN = totP = NULL
  dt_ev[, max(abs(TN/totN - FN/totP))]
}
## Lift
lift = function(dt_ev, threshold = 0.5, ...) {
  precision = totP = totN = cumpop = NULL
  # badrate of rejected sample / of overalll
  # threshold: approval rate
  dt_ev[, lift := precision/(totP/(totP+totN))][][cumpop >= 1-threshold,][1,lift]
}


# optimal cutoff ------
# ks
cutoff_ks = function(dt_ev) {
  TN = totN = FN = totP = . = cumpop =  pred = NULL

  dt_ev[, ks := abs(TN/totN - FN/totP)
        ][ks == max(ks, na.rm = TRUE)
          ][,.(cumpop, pred, ks)]
}
# roc
cutoff_roc = function(dt_ev) {
  . = cumpop = pred = TPR = FPR = co = NULL
  dt_ev[, .(cumpop,pred,TPR,FPR)
        ][, co := (TPR-FPR)^2/2
          ][co == max(co, na.rm = TRUE)
            ][, .(cumpop, pred, TPR, FPR)][1]
}
# fbeta
cutoff_fbeta = function(dt_ev, beta=1, ...) {
  . = cumpop = pred = precision =  recall = f = NULL

  fb = dt_ev[, .(cumpop, pred,precision,recall)
             ][, f := 1/(1/(1+beta^2)*(1/precision+beta^2/recall))
               ][which.max(f)][1]
  setnames(fb, c('cumpop', 'pred', 'precision', 'recall', paste0('f',beta)))
}

## confusion matrix
confusionMatrix = function(dt, threshold=0.5, ...) {
  pred_label = pred = . = label = error = pred_1 = pred_0 = NULL

  setDT(dt)
  # data of actual and predicted label
  dt_alpl = dt[, pred_label := pred >= threshold][, .N, keyby = .(label, pred_label)][, pred_label := paste0('pred_', as.integer(pred_label))]
  # confusion matrix
  cm = dcast(
    dt_alpl, label ~pred_label, value.var = 'N'
  )[1, error := pred_1/sum(pred_1+pred_0)
    ][2, error := pred_0/sum(pred_1+pred_0)]
  # total row
  cm = rbind(cm, data.frame(label = 'total', t(colSums(cm[,-1]))), fill=TRUE)
  cm[3, error := (cm[1,3]+cm[2,2])/sum(cm[3,2]+cm[3,3])]
  return(cm)
}


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
perf_plot = function(label, pred, title=NULL, groupnum=NULL, type=c("ks", "roc"), show_plot=TRUE, seed=618) {
  stop("This function has renamed as perf_eva.")
}


# plot ------
#Our transformation function
fmt_dcimals <- function(x) sprintf("%.1f", x)
number_ticks <- function(n) {function(limits) pretty(limits, n)}

#' @importFrom stats density
plot_density = function(dt_lst, title=NULL, positive, ...) {
  .=datset=dens=label=label_str=max_dens=pred=NULL

  dt_df = rbindlist(dt_lst, idcol = 'datset')[, label := factor(label)]

  # max pred
  if (dt_df[,mean(pred) < -1]) dt_df[, pred := abs(pred)]
  max_pred = dt_df[,max(pred)]
  if (max_pred < 1) max_pred = 1
  min_pred = dt_df[,min(pred)]
  if (max_pred == 1) min_pred = 0

  # data frame of max density by datset and label
  max_density_by_datset_label = dt_df[
    , .(pred=density(pred)$x, dens=density(pred)$y), by=c('datset','label')
  ][, max_dens := max(dens), by=c('datset','label')
  ][ dens == max_dens
  ][, max_dens := NULL]

  # max density
  max_density = max_density_by_datset_label[, ceiling2(max(dens))]

  # coord for label_string
  coord_label = max_density_by_datset_label[
    , .(pred=mean(pred), dens=mean(dens)), by=label
  ][, label_str := ifelse(grepl(positive, label), 'positive', 'negative')]

  # plot
  pdens = ggplot(data = dt_df) +
    # geom_histogram(aes(x=pred)) +
    # geom_density(aes(x=pred), linetype='dotted') +
    geom_density(aes(x=pred, color=datset, linetype=label), fill='gray', alpha=0.1, adjust = 1.618) +
    geom_text(data = coord_label, aes(x=pred, y=dens, label=label_str)) +
    # geom_vline(xintercept = threshold, linetype='dotted') +
    # geom_text(aes(label='cut-off', x=threshold, y = 0), vjust=0) +
    guides(linetype=FALSE, color=guide_legend(title=NULL)) +
    theme_bw() +
    theme(legend.position=c(1,1),
          legend.justification=c(1,1),
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.key.size = unit(1.5, 'lines'))

  # axis, labs
  pdens = pdens + ggtitle(paste0(title, 'Density')) +
    labs(x = "Prediction", y = "Density") +
    scale_y_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    scale_x_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    coord_fixed(ratio = (max_pred-min_pred)/(max_density), xlim = c(min_pred,max_pred), ylim = c(0,max_density), expand = FALSE)

  return(pdens)
}

plot_ks = function(dat_eva_lst, pm=NULL, co=NULL, title=NULL, ...) {
  . = datset = KS = metrics = pred_threshold = coord = maxks = oc = pred = cumpop = cumgood = cumbad = NULL

  # data for ks plot
  dt_ks = lapply(dat_eva_lst, function(x) {
    TN = totN = FN = totP = NULL
    x = x[, .(cumpop, pred, cumgood = TN/totN, cumbad = FN/totP, ks = abs(TN/totN - FN/totP))]
    x = rbind(x, data.table(cumpop=0), fill=TRUE)
    x[is.na(x)] <- 0
    return(x[order(cumpop)])
  })
  dt_ks = merge(
    rbindlist(dt_ks, fill = TRUE, idcol = 'datset'),
    merge(rbindlist(pm, idcol = 'datset')[,.(datset,maxks=KS)],
          rbindlist(co, idcol = 'datset')[metrics == 'ks',.(datset, pred_threshold,coord)], by = 'datset'),
    by = 'datset', all.x = TRUE
  )[, datset := sprintf('%s, KS=%.4f\np=%.4f, %s', format(datset), round(maxks,4), pred_threshold, coord)][]

  # max ks row
  dfks = dt_ks[, .SD[ks == max(ks)][1], by = 'datset'][, oc := sprintf('@%.4f', round(pred,4))][]#[, oc := sprintf('%.4f\n(%.4f,%.4f)', round(pred,4), round(cumpop,4), round(ks,4))]

  # plot
  pks = ggplot(data = dt_ks, aes(x=cumpop)) +
    geom_line(aes(y=cumgood, color=datset), linetype='dotted') +
    geom_line(aes(y=cumbad, color=datset), linetype='dotted') +
    geom_line(aes(y=ks, color=datset)) +
    geom_segment(data = dfks, aes(x = cumpop, y = 0, xend = cumpop, yend = ks, color=datset), linetype = "dashed") +
    geom_point(data = dfks, aes(x=cumpop, y=ks), color='red') +
    # geom_text(data = dfks, aes(x=cumpop, y=ks, label=oc, color=datset), vjust=0) +
    annotate("text", x=0.4, y=0.7, vjust = -0.2, label="Good", colour = "gray") +
    annotate("text", x=0.95, y=0.7, vjust = -0.2, label="Bad", colour = "gray") +
    theme_bw() +
    theme(legend.position=c(0,1),
          legend.justification=c(0,1),
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.key.size = unit(1.5, 'lines')) +
    guides(color=guide_legend(title=NULL))

  # axis, labs, theme
  pks = pks + ggtitle(paste0(title, 'K-S')) +
    labs(x = "% of population", y = "% of total Good/Bad") +
    scale_y_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    scale_x_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    coord_fixed(xlim = c(0,1), ylim = c(0,1), expand = FALSE)

  return(pks)
}

plot_lift = function(dat_eva_lst, pm=NULL, co=NULL, title=NULL, ...) {
  cumpop = datset = NULL
  dt_lift = lapply(dat_eva_lst, function(x) {
    . = precision = totP = totN = NULL
    x = x[, .(cumpop, lift = precision/(totP/(totP+totN)))]
    x = rbind(x, data.table(cumpop=0), fill=TRUE)
    return(x[order(cumpop)])
  })
  dt_lift = rbindlist(dt_lift, fill = TRUE, idcol = 'datset')

  max_lift = dt_lift[, ceiling(max(lift,na.rm = TRUE))]

  # plotting
  plift = ggplot(data = dt_lift, aes(x=cumpop, color = datset)) +
    geom_line(aes(y = lift), na.rm = TRUE) +
    theme_bw() +
    theme(legend.position=c(0,1),
          legend.justification=c(0,1),
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.key.size = unit(1.5, 'lines')) +
    guides(color=guide_legend(title=NULL))

  # axis, labs, theme
  plift = plift +
    ggtitle(paste0(title, 'Lift')) +
    labs(x = "% of population", y = "Lift") +
    scale_y_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    scale_x_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    coord_fixed(ratio = 1/(max_lift),xlim = c(0,1), ylim = c(0,max_lift), expand = FALSE)

  return(plift)
}

plot_gain = function(dat_eva_lst, pm=NULL, co=NULL, title=NULL, ...) {
  . = cumpop = datset = precision = NULL
  dt_gain = lapply(dat_eva_lst, function(x) {
    x = x[, .(cumpop, precision)]
    x = rbind(x, data.table(cumpop=0), fill=TRUE)
    return(x[order(cumpop)])
  })
  dt_gain = rbindlist(dt_gain, fill = TRUE, idcol = 'datset')

  # plotting
  pgain = ggplot(data = dt_gain, aes(x=cumpop, color = datset)) +
    geom_line(aes(y = precision), na.rm = TRUE) +
    theme_bw() +
    theme(legend.position=c(0,1),
          legend.justification=c(0,1),
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.key.size = unit(1.5, 'lines')) +
    guides(color=guide_legend(title=NULL))

  # axis, labs, theme
  pgain = pgain +
    ggtitle(paste0(title, 'Gain')) +
    labs(x = "% of population", y = "Precision / PPV") +
    scale_y_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    scale_x_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    coord_fixed(xlim = c(0,1), ylim = c(0,1), expand = FALSE)

  return(pgain)
}

plot_roc = function(dat_eva_lst, pm=NULL, co=NULL, title=NULL, ...) {
  . = datset = AUC = metrics = pred_threshold = coord = pred = TPR = FPR = ocFPR = ocTPR = NULL

  dt_roc = lapply(dat_eva_lst, function(x) {
    x = x[, .(TPR, FPR)]
    x = rbind(x, data.table(TPR=0:1, FPR=0:1), fill=TRUE)
    return(x[order(TPR, FPR)])
  })
  # merge with auc
  dt_roc = merge(
    rbindlist(dt_roc, fill = TRUE, idcol = 'datset'),
    merge(rbindlist(pm, idcol = 'datset')[,.(datset,auc=AUC)],
          rbindlist(co, idcol = 'datset')[metrics == 'roc',.(datset, pred_threshold,coord)], by = 'datset'),
    by = 'datset', all.x = TRUE
  )[, datset := sprintf('%s, AUC=%.4f\np=%.4f, %s', format(datset), round(auc,4), pred_threshold, coord)][]

  # optimal cutoff
  dt_cut = merge(
    rbindlist(pm, idcol = 'datset')[,.(datset,auc=AUC)],
    rbindlist(lapply(dat_eva_lst, cutoff_roc), idcol = 'datset'), by = 'datset'
  )[,.(datset, pred, ocTPR=TPR, ocFPR=FPR)
  ]#[, oc := sprintf('@%.4f', round(pred,4))]#[, oc := sprintf('%.4f\n(%.4f,%.4f)', round(pred,4), round(ocFPR,4), round(ocTPR,4))]

  # plot
  proc = ggplot(dt_roc, aes(x=FPR)) +
    geom_line(aes(y=TPR, color=datset)) +
    geom_line(aes(y=FPR), linetype = "dashed", colour="gray") +
    geom_ribbon(aes(ymin=0, ymax=TPR, fill=datset), alpha=0.1) +
    geom_point(data = dt_cut, aes(x=ocFPR, y=ocTPR), color='red') +
    # geom_text(data = dt_cut, aes(x=ocFPR, y=ocTPR, label=oc, color=datset), vjust=1) +
    # geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype = "dashed", colour="red") +
    theme_bw() +
    theme(legend.position=c(0,0),
          legend.justification=c(0,0),
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.key.size = unit(1.5, 'lines')) +
    guides(color=guide_legend(title=NULL), fill=FALSE)

  # axis, labs, theme
  proc = proc + ggtitle(paste0(title, 'ROC')) +
    labs(x = "1-Specificity / FPR", y = "Sensitivity / TPR") +
    scale_y_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    scale_x_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    coord_fixed(xlim = c(0,1), ylim = c(0,1), expand = FALSE)

  return(proc)
}

plot_pr = function(dat_eva_lst, pm=NULL, co=NULL, title=NULL, ...) {
  . = recall = precision = datset = NULL

  dt_pr = lapply(dat_eva_lst, function(x) x[, .(recall, precision)][order(precision, recall)])
  dt_pr = rbindlist(dt_pr, idcol = 'datset')

  # plot
  ppr = ggplot(dt_pr) +
    geom_line(aes(x=recall, y=precision, color=datset), na.rm = TRUE) +
    geom_line(aes(x=recall, y=recall), na.rm = TRUE, linetype = "dashed", colour="gray") +
    # geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour = "red", linetype="dashed") +
    theme_bw() +
    theme(legend.position=c(0,0),
          legend.justification=c(0,0),
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.key.size = unit(1.5, 'lines')) +
    guides(color=guide_legend(title=NULL))

  # axis, labs, theme
  ppr = ppr + ggtitle(paste0(title, 'P-R')) +
    labs(x = "Recall / TPR", y = "Precision / PPV") +
    scale_y_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    scale_x_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    coord_fixed(xlim = c(0,1), ylim = c(0,1), expand = FALSE)

  return(ppr)
}

plot_lz = function(dat_eva_lst, pm=NULL, co=NULL, title=NULL, ...) {
  FN = totP = . = datset = Gini = cumpop = cumbadrate = NULL
  dt_lz = lapply(dat_eva_lst, function(x) {
    x = x[, .(cumpop, cumbadrate = FN/totP)]
    x = rbind(x, data.table(cumpop=0, cumbadrate=0), fill=TRUE)
    return(x[order(cumpop)])
  })
  dt_lz = merge(
    rbindlist(dt_lz, fill = TRUE, idcol = 'datset'),
    rbindlist(pm, idcol = 'datset')[,.(datset,gini=Gini)], by = 'datset', all.x = TRUE
  )[, datset := sprintf('%s, Gini=%.4f', format(datset), round(gini,4))][]

  # plot
  plz = ggplot(dt_lz, aes(x=cumpop)) +
    geom_line(aes(y=cumbadrate, color=datset)) +
    geom_line(aes(y=cumpop), linetype = "dashed", colour="gray") +
    # geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype = "dashed", colour="red") +
    geom_ribbon(aes(ymin=cumpop, ymax=cumbadrate, fill=datset), alpha=0.1) +
    theme_bw() +
    theme(legend.position=c(0,1),
          legend.justification=c(0,1),
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.key.size = unit(1.5, 'lines')) +
    guides(color=guide_legend(title=NULL), fill=FALSE)

  # axis, labs, theme
  plz = plz + ggtitle(paste0(title, 'Lorenz')) +
    labs(x = "% of population", y = "% of total Good/Bad") +
    scale_y_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    scale_x_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    coord_fixed(xlim = c(0,1), ylim = c(0,1), expand = FALSE)

  return(plz)
}

plot_f1 = function(dat_eva_lst, pm=NULL, co=NULL, beta=1, title=NULL, ...) {
  . = datset = pred_threshold = coord = f1 = cumpop = metrics = NULL

  dt_f = lapply(dat_eva_lst, function(x) {
    . = cumpop = precision = recall = NULL
    x = x[, .(cumpop, f = 1/(1/(1+beta^2)*(1/precision+beta^2/recall)))]
    setnames(x, c('cumpop', paste0('f',beta)))
    return(x[order(cumpop)])
  })
  dt_f = merge(
    rbindlist(dt_f, fill = TRUE, idcol = 'datset'),
    rbindlist(co, idcol = 'datset')[metrics == paste0('max_f',beta),.(datset, pred_threshold,coord)],
    by = 'datset', all.x = TRUE
  )[, datset := sprintf('%s\np=%.4f, %s', format(datset), pred_threshold, coord)][]


  # optimal cutoff
  dt_cut = dt_f[, .SD[f1 == max(f1, na.rm = TRUE)][1], by = 'datset']#[, oc := sprintf('%.4f\n(%.4f,%.4f)', round(pred,4), round(cumpop,4), round(ks,4))]

  # plot
  pf = ggplot(data = dt_f, aes(x=cumpop)) +
    geom_line(aes_string(y=paste0('f',beta), color='datset'), na.rm = TRUE) +
    geom_point(data = dt_cut, aes_string(x='cumpop', y=paste0('f',beta)), color='red') +
    # geom_text(data = dt_cut, aes_string(x='cumpop', y=paste0('f',beta), label='oc', color='datset'), vjust=0) +
    geom_segment(data = dt_cut, aes_string(x = 'cumpop', y = 0, xend = 'cumpop', yend = paste0('f',beta), color='datset'), linetype = "dashed") +
    theme_bw() +
    theme(legend.position=c(0,1),
          legend.justification=c(0,1),
          legend.background=element_blank(),
          legend.key=element_blank(),
          legend.key.size = unit(1.5, 'lines')) +
    guides(color=guide_legend(title=NULL), fill=FALSE)

  # axis, labs, theme
  pf = pf + ggtitle(paste0(title, paste0("F",beta))) +
    labs(x = "% of population", y = paste0("F",beta)) +
    scale_y_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    scale_x_continuous(labels=fmt_dcimals, breaks=number_ticks(5)) +
    coord_fixed(xlim = c(0,1), ylim = c(0,1), expand = FALSE)

  return(pf)
}


# eva ------

# dataset label pred
#' @importFrom stats complete.cases
func_dat_labelpred = function(pred, label, title, positive, seed, ...) {
  # convert pred/label into list of list, such as:
  # pred = list(datset = list(pred = ...))
  lst_pl = list(pred=pred, label=label)
  # map on lst_pl
  lst_pl2 = Map(function(x, xname) {
    if (is.null(x)) return(x)

    # if not list, convert into list
    if (!inherits(x,'list')) {
      x = list(dat=x)
      if (!is.null(title)) names(x) <- title
    }

    # convert the objects in list of pred/label, into lists
    x = lapply(x, function(xi) {
      xi_lst = list()
      if (!is.data.frame(xi)) {
        xi_lst[[xname]] = xi
      } else {
        xi_lst = as.list(xi)
        if (xname == 'label') names(xi_lst) = xname
      }
      return(xi_lst)
    })

    # return pred/label
    return(x)
  }, x = lst_pl, xname = names(lst_pl))


  # convert list into datatable using setDT()
  dt_lst = list()
  for (ds in names(lst_pl2$pred)) { # ds = dataset
    lst_pred_ds = lst_pl2$pred[[ds]]

    lst_label_ds = lst_pl2$label[[ds]]
    if (is.null(lst_label_ds)) lst_label_ds[[ds]] = rep_len(NA, unique(sapply(lst_pred_ds, length)))

    dt_lst[[ds]] = setDT(c(lst_pred_ds, lst_label_ds))
  }


  # random sort datatable
  dt_lst = lapply(dt_lst, function(x) {
    set.seed(seed)
    return(x[sample(.N, .N)])
  })
  # # if pred is score
  # if (for_psi) {
  #   for (pn in setdiff(names(dt_lst[[1]]), 'label')) {
  #     if ( all(sapply(dt_lst, function(x) mean(x[[pn]],na.rm = TRUE)>1)) ) {
  #       warning("Since the average of pred is not in [0,1], it is treated as predicted score but not probability.")
  #       for (ds in names(dt_lst)) {
  #         dt_lst[[ds]][[pn]] = -dt_lst[[ds]][[pn]]
  #       }
  #     }
  #   }
  # }


  # check label & remove rows with missing values in pred
  dt_lst = lapply(dt_lst, function(x) {
    if (!is.null(label)) x = check_y(x, 'label', positive)
    if (anyNA(x)) {
      warning("The NAs in dataset have been removed.")
      x = x[complete.cases(copy(x)[,label:=NULL]), ]#x[!is.na(pred)]
    }
    return(x)
  })

  return(dt_lst)
}
# dataset evaluation
func_dat_eva = function(dt, groupnum=NULL, ...) {
  . = label = pred = nP = nN = totP = FN = totN = TN = TP = FP = group = NULL

  # nP, number of Positive samples in each predicted prob
  # nN, number of Negative samples in each predicted prob

  # totP, total Positive
  # totN, total Negative

  # FN, False Negative; cumsum of positive, cumbad
  # TN, True Negative;  cumsum of negative, cumgood

  # TP, True Positive;  totP-FN
  # FP, False Positive; totN-TN
  setDT(dt)
  total_num = dt[,.N]

  dt_ev = dt[, .(nP = sum(label==1), nN = sum(label==0)), keyby = pred]
  if (!(is.null(groupnum) || total_num <= groupnum)) {
    dt_ev = dt_ev[, pred := ceiling(cumsum(nP+nN)/(total_num/groupnum))
                ][, .(nP = sum(nP), nN = sum(nN)), keyby = pred ]
  }

  dt_ev = dt_ev[, `:=`(FN = cumsum(nP), TN = cumsum(nN),
                    totP = sum(nP), totN = sum(nN))
           ][, `:=`(TP = totP-FN, FP = totN-TN)
           ][, `:=`(TPR = TP/totP, FPR = FP/totN,
                    precision = TP/(TP+FP), recall = TP/totP,
                    cumpop = (FN+TN)/(totP+totN))][]

  return(dt_ev)
}


# performance metrics
pf_metrics = function(dt_lst, dt_ev_lst, all_metrics, sel_metrics) {
  metrics_to_names = function(bm) {
    bm = toupper(bm)
    if (bm=='LOGLOSS') {
      bm = 'LogLoss'
    } else if (bm == 'GINI') {
      bm = 'Gini'
    }
    return(bm)
  }

  # all metrics
  all_pm = list()
  for (n in names(dt_lst)) {
    d1 = dt_lst[[n]]
    d2 = dt_ev_lst[[n]]

    allpm_list = list()
    for (i in all_metrics) {
      allpm_list[[metrics_to_names(i)]] = do.call(i, args = list(dt = d1, dt_ev=d2))
    }
    all_pm[[n]] = setDT(allpm_list)
  }

  # selected metrics
  sel_pm = lapply(all_pm, function(x) x[,sapply(sel_metrics, metrics_to_names),with=FALSE])

  return(list(all_pm=all_pm, sel_pm=sel_pm))
}
# optimal cutoffs
pf_cutoffs = function(dt_ev_lst) {
  . = pred = cumpop = f1 = f2 = FPR = TPR = NULL
  co = list()
  for (n in names(dt_ev_lst)) {
    con = list(
      max_f1  = cutoff_fbeta(dt_ev_lst[[n]],1)[,.(pred_threshold=pred, coord = sprintf('(%.2f,%.2f)',cumpop,f1))],
      max_f2  = cutoff_fbeta(dt_ev_lst[[n]],2)[,.(pred_threshold=pred, coord = sprintf('(%.2f,%.2f)',cumpop,f2))],
      ks  = cutoff_ks(dt_ev_lst[[n]])[,.(pred_threshold=pred, coord = sprintf('(%.2f,%.2f)',cumpop,ks))],
      roc = cutoff_roc(dt_ev_lst[[n]])[,.(pred_threshold=pred, coord = sprintf('(%.2f,%.2f)',FPR,TPR))] )

    co[[n]] = rbindlist(con, idcol = 'metrics')
  }
  return(co)
}


#' Binomial Metrics
#'
#' \code{perf_eva} calculates metrics to evaluate the performance of binomial classification model. It can also creates confusion matrix and model performance graphics.
#'
#' @param pred A list or vector of predicted probability or score.
#' @param label A list or vector of label values.
#' @param title The title of plot. Default is NULL.
#' @param binomial_metric Default is c('mse', 'rmse', 'logloss', 'r2', 'ks', 'auc', 'gini'). If it is NULL, then no metric will calculated.
#' @param confusion_matrix Logical, whether to create a confusion matrix. Default is TRUE.
#' @param threshold Confusion matrix threshold. Default is the pred on maximum F1.
#' @param show_plot Default is c('ks', 'roc'). Accepted values including c('ks', 'lift', 'gain', 'roc', 'lz', 'pr', 'f1', 'density').
#' @param positive Value of positive class, default is "bad|1".
#' @param ... Additional parameters.
#'
#' @return A list of binomial metric, confusion matrix and graphics
#' @seealso \code{\link{perf_psi}}
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
#' TNR, True Negative Rate(Specificity) = true negative/total actual negative = 1-FPR
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
#'
#' @examples
#' \dontrun{
#' # data preparing ------
#' # load germancredit data
#' data("germancredit")
#' # filter variable via missing rate, iv, identical value rate
#' dt_f = var_filter(germancredit, "creditability")
#' # breaking dt into train and test
#' dt_list = split_df(dt_f, "creditability")
#' label_list = lapply(dt_list, function(x) x$creditability)
#'
#' # woe binning ------
#' bins = woebin(dt_list$train, "creditability")
#' # converting train and test into woe values
#' dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins))
#'
#' # glm ------
#' m1 = glm(creditability ~ ., family = binomial(), data = dt_woe_list$train)
#' # vif(m1, merge_coef = TRUE)
#' # Select a formula-based model by AIC
#' m_step = step(m1, direction="both", trace=FALSE)
#' m2 = eval(m_step$call)
#' # vif(m2, merge_coef = TRUE)
#'
#' # predicted proability
#' pred_list = lapply(dt_woe_list, function(x) predict(m2, type = 'response', x))
#'
#' # scorecard ------
#' card = scorecard(bins, m2)
#'
#' # credit score, only_total_score = TRUE
#' score_list = lapply(dt_list, function(x) scorecard_ply(x, card))
#' # credit score, only_total_score = FALSE
#' score_list2 = lapply(dt_list, function(x) scorecard_ply(x, card, only_total_score=FALSE))
#'
#'
#' ###### perf_eva examples ######
#' # Example I, one datset
#' ## predicted p1
#' perf_eva(pred = pred_list$train, label=dt_list$train$creditability, title = 'train')
#' ## predicted score
#' # perf_eva(pred = score_list$train, label=dt_list$train$creditability, title = 'train')
#'
#' # Example II, multiple datsets
#' ## predicted p1
#' perf_eva(pred = pred_list, label = label_list)
#' ## predicted score
#' # perf_eva(score_list, label_list)
#'
#'
#' ###### perf_psi examples ######
#' # Example I # only total psi
#' psi1 = perf_psi(score = score_list, label = label_list)
#' psi1$psi  # psi data frame
#' psi1$pic  # pic of score distribution
#'
#' # Example II # both total and variable psi
#' psi2 = perf_psi(score = score_list, label = label_list)
#' # psi2$psi  # psi data frame
#' # psi2$pic  # pic of score distribution
#'
#'
#' ###### gains_table examples ######
#' # Example I, input score and label can be a list or a vector
#' gains_table(score = score_list$train, label = label_list$train)
#' gains_table(score = score_list, label = label_list)
#'
#' # Example II, specify the bins number and type
#' gains_table(score = score_list, label = label_list, bin_num = 20)
#' gains_table(score = score_list, label = label_list, bin_type = 'width')
#' }
#'
#' @import data.table ggplot2 gridExtra
#' @export
#'
perf_eva = function(pred, label, title=NULL, binomial_metric=c('mse', 'rmse', 'logloss', 'r2', 'ks', 'auc', 'gini'), confusion_matrix=TRUE, threshold=NULL, show_plot=c('ks', 'roc'), positive="bad|1", ...) {
  . = f1 = NULL

  # arguments
  seed = list(...)[['seed']]
  if (is.null(seed)) seed = 618

  # list of data with label and pred
  dt_lst = suppressMessages(
    func_dat_labelpred(pred=pred, label=label, title=title, positive=positive, seed=seed))
  dt_lst = lapply(dt_lst, function(x) x[,.(label, pred=x[[setdiff(names(x),'label')]])])

  # if pred is score
  pred_is_score = all(sapply(dt_lst, function(x) mean(x$pred, na.rm = TRUE)>1))
  dt_lst = lapply(dt_lst, function(x) {
    nP = nN = NULL
    if (pred_is_score) {
      # make sure the positive samples are locate at below when order by pred
      rid19 = quantile(x[,.I], c(0.1, 0.9))
      x2 = x[, .(nP = sum(label==1), nN = sum(label==0)), keyby = pred]
      if (x2[1:rid19[1], sum(nP) > sum(nN)] && x2[rid19[2]:.N, sum(nP) < sum(nN)]) {
        x = x[, pred := -pred]
        # warning("Since the average of pred is not locate in [0,1], it is treated as predicted score but not probability.")
      }
    }
    return(x)
  })
  # datasets for evaluation
  dt_ev_lst = lapply(dt_lst, function(x) func_dat_eva(x, groupnum = NULL))
  # cutoff, Maximum Metrics
  co = pf_cutoffs(dt_ev_lst)


  # return list
  rt_list = list()
  ###### performance metric
  all_metrics = c('mse','rmse','logloss','r2','ks','auc','gini')
  if (pred_is_score) all_metrics = c('ks', 'auc', 'gini')
  sel_metrics = intersect(binomial_metric, all_metrics)

  pm_lst = pf_metrics(dt_lst, dt_ev_lst, all_metrics, sel_metrics)
  if (length(sel_metrics)>0) rt_list[['binomial_metric']] = pm_lst$sel_pm
  # cat('Binomial Metrics:\n')
  # print(pm)

  ###### confusion matrix
  if (confusion_matrix) {
    # if threshold is not provided, set it as max F1
    if (is.null(threshold) || !is.numeric(threshold)) threshold = cutoff_fbeta(dt_ev_lst[[1]])[,pred]
    if (pred_is_score) threshold_abs = abs(threshold)
    # confusion matrix
    cat(sprintf('[INFO] The threshold of confusion matrix is %.4f.\n', threshold_abs))
    rt_list[['confusion_matrix']] = lapply(dt_lst, function(x) confusionMatrix(dt=x, threshold=threshold))
  }
  # cat(sprintf('Confusion Matrix with threshold=%s:\n', round(threshold,4)))
  # print(cm)

  ###### plot
  # title
  if (!is.null(title)) title = paste0(title,': ')
  # type
  type = list(...)[["type"]]
  if (isTRUE(show_plot) & !is.null(show_plot) & !is.null(type)) show_plot = type
  # show_plot
  show_plot = intersect(show_plot, c('ks', 'lift', 'gain', 'roc', 'lz', 'pr', 'f1', 'density'))
  # pic
  if (length(show_plot)>0) {
    # datasets for visualization
    groupnum = list(...)[["groupnum"]]
    if (is.null(groupnum)) groupnum = 1000
    dt_ev_lst_plot = lapply(dt_lst, function(x) func_dat_eva(x, groupnum = groupnum))
    # plot
    plist = lapply(paste0('plot_', show_plot), function(x) do.call(x, args = list(dat_eva_lst = dt_ev_lst_plot, dt_lst=dt_lst, pm=pm_lst$all_pm, co=co, title=title, positive=positive)))
    # return
    rt_list[['pic']] = do.call(grid.arrange, c(plist, list(ncol=ceiling(sqrt(length(show_plot))), padding = 0)))
  }

  return(rt_list)
}


# psi ------
# PSI function
psi_metric = function(dt_sn, names_datset) {
  A=E=logAE=bin_psi=NULL
  # psi = sum((Actual% - Expected%)*ln(Actual%/Expected%))

  # dat = copy(dat)[,y:=NULL][complete.cases(dat),]
  AE = bin_PSI = NULL

  # data frame of bin, actual, expected
  dt_bae = dcast(
    dt_sn[, .N, keyby = c('datset', 'bin')],
    bin ~ datset, value.var="N", fill = 0.99
  )

  # psi
  psi_dt = dt_bae[
    , (c("A","E")) := lapply(.SD, function(x) x/sum(x)), .SDcols = names_datset
  ][, `:=`(AE = A-E, logAE = log(A/E))
  ][, `:=`(bin_psi = AE*logAE)
  ][, sum(bin_psi)]

  return(psi_dt)
}

# psi plot
psi_plot = function(dt_psi, psi_sn, title, sn) {
  . = label = N = bad = badprob = distr = bin = midbin = bin1 = bin2 = datset = badprob2 = NULL

  title = paste0(ifelse(is.null(title), sn, title), " PSI: ", round(psi_sn, 4))

  distr_prob =
    dt_psi[, .(.N, bad = sum(label==1)), keyby = c('datset','bin')
       ][, `:=`(distr = N/sum(N), badprob = bad/N), by = 'datset'
       ][, `:=`(badprob2 = badprob*max(distr)), by = "datset"
       ][, `:=`(
         bin1 = as.numeric(sub("\\[(.+),(.+)\\)", "\\1", bin)),
         bin2 = as.numeric(sub("\\[(.+),(.+)\\)", "\\2", bin))
      )][, midbin := (bin1+bin2)/2 ][]

  # plot
  p_score_distr =
    ggplot(distr_prob) +
    geom_bar(aes(x=bin, y=distr, fill=datset), alpha=0.6, stat="identity", position="dodge", na.rm=TRUE) +
    geom_line(aes(x=bin, y=badprob2, group=datset, linetype=datset), colour='blue', na.rm=TRUE) +
    geom_point(aes(x=bin, y=badprob2), colour='blue', shape=21, fill="white", na.rm=TRUE) +
    guides(fill=guide_legend(title="Distribution"), colour=guide_legend(title="Probability"), linetype=guide_legend(title="Probability")) +
    scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~./max(distr_prob$distr), name = "Bad probability")) +
    ggtitle(title) +
    labs(x=NULL, y="Score distribution") +
    # geom_text(aes(label="@http://shichen.name/scorecard", x=Inf, y=Inf), vjust = -1, hjust = 1, color = "#F0F0F0") +
    theme_bw() +
    theme(
      plot.title=element_text(vjust = -2.5),
      legend.position=c(1,1),
      legend.justification=c(1,1),
      legend.background=element_blank(),
      axis.title.y.right = element_text(colour = "blue"),
      axis.text.y.right  = element_text(colour = "blue",angle=90, hjust = 0.5),
      axis.text.y = element_text(angle=90, hjust = 0.5))

  return(p_score_distr)
}




gains_table_format = function(dt_distr) {
  . = good = bad = bin = count = datset = NULL

  dt_distr = dt_distr[, .(
    bin,
    count, cum_count = cumsum(count),
    good,  cum_good = cumsum(good),
    bad,   cum_bad = cumsum(bad),
    count_distr = count/sum(count),
    badprob=bad/count, cum_badprob = cumsum(bad)/cumsum(count),
    approval_rate = cumsum(count)/sum(count) ), by = datset]

  return(dt_distr)
}
#' Gains Table
#'
#' \code{gains_table} creates a data frame including distribution of total, good, bad, bad rate and approval rate by score bins. It provides both equal width and equal frequency intervals on score binning.
#'
#' @param score A list of credit score for actual and expected data samples. For example, score = list(actual = scoreA, expect = scoreE).
#' @param label A list of label value for actual and expected data samples. For example, label = list(actual = labelA, expect = labelE).
#' @param bin_num Integer, the number of score bins. Default is 10. If it is 'max', then individual scores are used as bins.
#' @param bin_type The score is binning by equal frequency or equal width. Accepted values are 'freq' and 'width'. Default is 'freq'.
#' @param positive Value of positive class, default is "bad|1".
#' @param ... Additional parameters.
#'
#' @return A data frame
#' @seealso \code{\link{perf_eva}} \code{\link{perf_psi}}
#'
#' @examples
#' \dontrun{
#' # data preparing ------
#' # load germancredit data
#' data("germancredit")
#' # filter variable via missing rate, iv, identical value rate
#' dt_f = var_filter(germancredit, "creditability")
#' # breaking dt into train and test
#' dt_list = split_df(dt_f, "creditability")
#' label_list = lapply(dt_list, function(x) x$creditability)
#'
#' # woe binning ------
#' bins = woebin(dt_list$train, "creditability")
#' # converting train and test into woe values
#' dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins))
#'
#' # glm ------
#' m1 = glm(creditability ~ ., family = binomial(), data = dt_woe_list$train)
#' # vif(m1, merge_coef = TRUE)
#' # Select a formula-based model by AIC
#' m_step = step(m1, direction="both", trace=FALSE)
#' m2 = eval(m_step$call)
#' # vif(m2, merge_coef = TRUE)
#'
#' # predicted proability
#' pred_list = lapply(dt_woe_list, function(x) predict(m2, type = 'response', x))
#'
#' # scorecard ------
#' card = scorecard(bins, m2)
#'
#' # credit score, only_total_score = TRUE
#' score_list = lapply(dt_list, function(x) scorecard_ply(x, card))
#' # credit score, only_total_score = FALSE
#' score_list2 = lapply(dt_list, function(x) scorecard_ply(x, card, only_total_score=FALSE))
#'
#'
#' ###### perf_eva examples ######
#' # Example I, one datset
#' ## predicted p1
#' perf_eva(pred = pred_list$train, label=dt_list$train$creditability, title = 'train')
#' ## predicted score
#' # perf_eva(pred = score_list$train, label=dt_list$train$creditability, title = 'train')
#'
#' # Example II, multiple datsets
#' ## predicted p1
#' perf_eva(pred = pred_list, label = label_list)
#' ## predicted score
#' # perf_eva(score_list, label_list)
#'
#'
#' ###### perf_psi examples ######
#' # Example I # only total psi
#' psi1 = perf_psi(score = score_list, label = label_list)
#' psi1$psi  # psi data frame
#' psi1$pic  # pic of score distribution
#'
#' # Example II # both total and variable psi
#' psi2 = perf_psi(score = score_list, label = label_list)
#' # psi2$psi  # psi data frame
#' # psi2$pic  # pic of score distribution
#'
#'
#' ###### gains_table examples ######
#' # Example I, input score and label can be a list or a vector
#' gains_table(score = score_list$train, label = label_list$train)
#' gains_table(score = score_list, label = label_list)
#'
#' # Example II, specify the bins number and type
#' gains_table(score = score_list, label = label_list, bin_num = 20)
#' gains_table(score = score_list, label = label_list, bin_type = 'width')
#' }
#'
#' @export
gains_table = function(score, label, bin_num=10, bin_type='freq', positive='bad|1', ...) {
  . = V1 = V2 = bad = bin = count = datset = group = NULL

  # arguments
  # seed
  seed = list(...)[['seed']]
  if (is.null(seed)) seed = 618
  # title
  title = list(...)[['title']]
  if (is.null(title)) title = NULL
  # return_dt_psi
  return_dt_psi = list(...)[['return_dt_psi']]
  if (is.null(return_dt_psi)) return_dt_psi = FALSE

  # bin_num
  if (bin_num != 'max' & bin_num <= 1) bin_num = 10
  # bin_type
  if (!(bin_type %in% c('freq', 'width'))) bin_type = 'freq'



  # data frame of score and label
  dt_sl = list(...)[['dt_sl']]
  if (is.null(dt_sl) & !is.null(score) & !is.null(label)) {

    # dateset list of score and label
    dt_sl = suppressWarnings( func_dat_labelpred(
      pred=score, label=label, title=title, positive=positive, seed=seed) )
    # rename dt_sl as c('label','score')
    dt_sl = lapply(dt_sl, function(x) {
      x[,.(label, score=x[[setdiff(names(x),'label')]])]
    })
    # rbind the list into a data frame, and set datset column as factor
    names_datset = names(dt_sl)
    dt_sl = rbindlist(dt_sl, idcol = 'datset')[, datset := factor(datset, levels = names_datset)]
  }



  is_score = dt_sl[,mean(score)>1]
  # breaks
  if ( bin_num=='max' || bin_num >= dt_sl[, length(unique(score))] ) {
    # in each value
    dt_psi = copy(dt_sl)[, bin := factor(score)]
  } else {
    if (bin_type == 'freq') {
      # in equal frequency
      brkp = copy(dt_sl)[order(score)
                         ][, group := ceiling(.I/(.N/bin_num))
                         ][, .(score = score[1]), by = group
                         ][, c(-Inf, score[-1], Inf)]

    } else if (bin_type == 'width') {
      # in equal width
      minmax = dt_sl[, sapply(.SD, function(x) list(min(x), max(x))), by=datset, .SDcols=c('score')
                   ][,.(mins = max(V1), maxs = min(V2))] # choose min of max value, and max of min value by dataset
      brkp = seq(minmax$mins, minmax$maxs, length.out = bin_num+1)
      if (is_score) brkp = round(brkp)
      brkp = c(-Inf, brkp[-c(1, length(brkp))], Inf)
    }
    dt_psi = dt_sl[, bin := cut(score, unique(brkp), right = FALSE, dig.lab = 10, ordered_result = F)]
  }
  if (return_dt_psi) return(dt_psi) # innter result usded in perf_psi function

  # distribution table
  dt_distr = dt_psi[, .(count=.N, good = sum(label==0), bad = sum(label==1)), keyby = .(datset,bin)
                  ][order(datset, -bin)]
  if (!is_score) dt_distr = dt_distr[order(datset, bin)] #is predicted probability
  # gains table
  dt_distr = gains_table_format(dt_distr)
  return(dt_distr)
}

# @param bin_type Whether in equal frequency or width when preparing dataset to calculates psi. Default is 'width'.
# @param return_distr_dat Logical. Default is FALSE. Whether to return a list of data frames including distribution of total, good, bad cases by score bins in both equal width and equal frequency. This table is also named gains table.
# @param bin_num Integer. Default is 10. The number of score bins in distribution tables.

#' PSI
#'
#' \code{perf_psi} calculates population stability index (PSI) for both total credit score and variables. It can also creates graphics to display score distribution and bad rate trends.
#'
#' @param score A list of credit score for actual and expected data samples. For example, score = list(actual = scoreA, expect = scoreE).
#' @param label A list of label value for actual and expected data samples. For example, label = list(actual = labelA, expect = labelE). Default is NULL.
#' @param title Title of plot, default is NULL.
#' @param show_plot Logical. Default is TRUE.
#' @param positive Value of positive class, default is "bad|1".
#' @param threshold_variable Integer. Default is 20. If the number of unique values > threshold_variable, the provided score will be counted as total credit score, otherwise, it is variable score.
#' @param var_skip Name of variables that are not score, such as id column. It should be the same with the var_kp in scorecard_ply function. Default is NULL.
#' @param ... Additional parameters.
#'
#' @return A data frame of psi and graphics of credit score distribution
#' @seealso \code{\link{perf_eva}} \code{\link{gains_table}}
#'
#' @details The population stability index (PSI) formula is displayed below: \deqn{PSI = \sum((Actual\% - Expected\%)*(\ln(\frac{Actual\%}{Expected\%}))).} The rule of thumb for the PSI is as follows: Less than 0.1 inference insignificant change, no action required; 0.1 - 0.25 inference some minor change, check other scorecard monitoring metrics; Greater than 0.25 inference major shift in population, need to delve deeper.
#'
#'
#' @examples
#' \dontrun{
#' # data preparing ------
#' # load germancredit data
#' data("germancredit")
#' # filter variable via missing rate, iv, identical value rate
#' dt_f = var_filter(germancredit, "creditability")
#' # breaking dt into train and test
#' dt_list = split_df(dt_f, "creditability")
#' label_list = lapply(dt_list, function(x) x$creditability)
#'
#' # woe binning ------
#' bins = woebin(dt_list$train, "creditability")
#' # converting train and test into woe values
#' dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins))
#'
#' # glm ------
#' m1 = glm(creditability ~ ., family = binomial(), data = dt_woe_list$train)
#' # vif(m1, merge_coef = TRUE)
#' # Select a formula-based model by AIC
#' m_step = step(m1, direction="both", trace=FALSE)
#' m2 = eval(m_step$call)
#' # vif(m2, merge_coef = TRUE)
#'
#' # predicted proability
#' pred_list = lapply(dt_woe_list, function(x) predict(m2, type = 'response', x))
#'
#' # scorecard ------
#' card = scorecard(bins, m2)
#'
#' # credit score, only_total_score = TRUE
#' score_list = lapply(dt_list, function(x) scorecard_ply(x, card))
#' # credit score, only_total_score = FALSE
#' score_list2 = lapply(dt_list, function(x) scorecard_ply(x, card, only_total_score=FALSE))
#'
#'
#' ###### perf_eva examples ######
#' # Example I, one datset
#' ## predicted p1
#' perf_eva(pred = pred_list$train, label=dt_list$train$creditability, title = 'train')
#' ## predicted score
#' # perf_eva(pred = score_list$train, label=dt_list$train$creditability, title = 'train')
#'
#' # Example II, multiple datsets
#' ## predicted p1
#' perf_eva(pred = pred_list, label = label_list)
#' ## predicted score
#' # perf_eva(score_list, label_list)
#'
#'
#' ###### perf_psi examples ######
#' # Example I # only total psi
#' psi1 = perf_psi(score = score_list, label = label_list)
#' psi1$psi  # psi data frame
#' psi1$pic  # pic of score distribution
#'
#' # Example II # both total and variable psi
#' psi2 = perf_psi(score = score_list, label = label_list)
#' # psi2$psi  # psi data frame
#' # psi2$pic  # pic of score distribution
#'
#'
#' ###### gains_table examples ######
#' # Example I, input score and label can be a list or a vector
#' gains_table(score = score_list$train, label = label_list$train)
#' gains_table(score = score_list, label = label_list)
#'
#' # Example II, specify the bins number and type
#' gains_table(score = score_list, label = label_list, bin_num = 20)
#' gains_table(score = score_list, label = label_list, bin_type = 'width')
#' }
#' @import data.table ggplot2 gridExtra
#' @export
#'
perf_psi = function(score, label=NULL, title=NULL, show_plot=TRUE, positive="bad|1", threshold_variable=20, var_skip=NULL, ...) {
  # # global variables
  . = datset = group = V1 = bin = NULL

  # arguments
  bin_type = list(...)[['bin_type']]
  if (is.null(bin_type) || !(bin_type %in% c('freq', 'width'))) bin_type='width'

  seed = list(...)[['seed']]
  if (is.null(seed)) seed = 618

  return_distr_dat = list(...)[['return_distr_dat']]
  if (is.null(return_distr_dat)) return_distr_dat = FALSE



  # dateset list of score and label
  dt_sl = suppressWarnings( func_dat_labelpred(
    pred=score, label=label, title=title, positive=positive, seed=seed) )
  names_datset = names(dt_sl) # names of dataset
  dt_sl = rbindlist(dt_sl, idcol = 'datset')[, datset := factor(datset, levels = names_datset)]


  rt = list() # return list
  for (sn in setdiff(names(dt_sl), c('datset', 'label', var_skip))) { # sn: score names
    # dataset for sn
    dt_sn = dt_sl[,.(datset, label, score=dt_sl[[sn]])]

    sn_is_totalscore = dt_sn[,length(unique(score)) > threshold_variable]
    bin_num <- ifelse(sn_is_totalscore, 10, 'max')

    dt_psi = gains_table(score=NULL, label=NULL, bin_num=10, bin_type=bin_type, positive = positive, return_dt_psi=TRUE, dt_sl=dt_sn)


    # return list
    temp_psi = list()
    for (i in names_datset[-1]) {
      # population stability index
      names_dts = c(names_datset[1], i)
      psi_sn = psi_metric(dt_psi[datset %in% names_dts], names_dts)
      temp_psi[[paste0(names_dts, collapse = '_')]] = data.table(psi = psi_sn)

      # pic
      temp_pic = NULL
      if (show_plot) {
        temp_pic = psi_plot(dt_psi[datset %in% names_dts], psi_sn, title, sn)
        if (length(names_datset) > 2) {
          rt[['pic']][[sn]][[paste0(names_dts, collapse = '_')]] = temp_pic
        } else {
          rt[['pic']][[sn]] = temp_pic
        }
      }
    }
    rt[['psi']][[sn]] = rbindlist(temp_psi, idcol = 'dataset')

    # equal freq / width data frame
    if (return_distr_dat) rt[['dat']][[sn]] = gains_table(score=NULL, label=NULL, bin_num=10, bin_type=bin_type, positive = positive, return_dt_psi=FALSE, dt_sl=dt_sn)
  }

  rt$psi = rbindlist(rt$psi, idcol = "variable", fill = TRUE)
  # rt$dat = rbindlist(rt$dat, idcol = "variable")
  return(rt)
}


