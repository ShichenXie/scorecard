#' ks, roc, lift, pr
#'
#' \code{perf_plot} provides performace evaluations, such as kolmogorov-smirnow(ks), ROC, lift and precision-recall curves, based on provided label and predicted probability values.
#'
#' @name perf_plot
#' @param label Label values, such as 0s and 1s.
#' @param pred Predicted probability values.
#' @param title Title of plot, default "train".
#' @param groupnum The group numbers when calculating bad probability, default 20.
#' @param type Types of performance plot, such as "ks", "lift", "roc", "pr". Default c("ks", "roc").
#' @param positive Value of positive class, default "bad|1".
#' @param show_plot Logical value, default TRUE. It means whether to show plot.
#' @param seed An integer. The specify seed is used for random sorting data, default: 186.
#' @return ks, roc, lift, pr
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(scorecard)
#'
#' # Traditional Credit Scoring Using Logistic Regression
#' # load germancredit data
#' data("germancredit")
#'
#' # rename creditability as y
#' dt <- data.table(germancredit)[, `:=`(
#'   y = ifelse(creditability == "bad", 1, 0),
#'   creditability = NULL
#' )]
#'
#' # woe binning ------
#' bins <- woebin(dt, "y")
#' dt_woe <- woebin_ply(dt, bins)
#'
#' # glm ------
#' m1 <- glm( y ~ ., family = "binomial", data = dt_woe)
#' # summary(m1)
#'
#' # Select a formula-based model by AIC
#' m_step <- step(m1, direction="both")
#' m2 <- eval(m_step$call)
#' # summary(m2)
#'
#' # performance ------
#' # predicted proability
#' dt_woe$pred <- predict(m2, type='response', dt_woe)
#'
#' # performace
#' # only ks & auc values
#' perf_plot(dt_woe$y, dt_woe$pred, show_plot=FALSE)
#'
#' # ks & roc plot
#' perf_plot(dt_woe$y, dt_woe$pred)
#'
#' # ks, lift, roc & pr plot
#' perf_plot(dt_woe$y, dt_woe$pred, type = c("ks","lift","roc","pr"))
#' }
#' @import data.table ggplot2 gridExtra
#' @export
#'
perf_plot <- function(label, pred, title="train", groupnum=20, type=c("ks", "roc"), positive="bad|1", show_plot=TRUE, seed=186) {
  group = . = good = bad = ks = cumbad = cumgood = value = variable = model = countP = countN = FN = TN = TP = FP = FPR = TPR = precision = recall = NULL # no visible binding for global variable

  # inputs checking
  if (!is.vector(label) | !is.vector(pred)) break
  if (length(label) != length(pred)) break

  # random sort datatable
  set.seed(seed)
  df1 <- data.table(
    label=ifelse(grepl("bad|1", as.character(label)), 1, 0),
    pred=pred
  )[!is.na(label)][sample(1:length(pred))]

  # data, dfkslift ------
  if ("ks" %in% type | "lift" %in% type) {
    if (groupnum == "N") groupnum <- length(pred)

    dfkslift <-
      df1[order(-pred)
        ][, group := ceiling(as.integer(row.names(.SD))/(.N/groupnum))
        ][,.(good = sum(label==0), bad = sum(label==1)), by=group
        ][,`:=`(group= as.integer(row.names(.SD))/.N,
                good = good/sum(good), bad  = bad/sum(bad),
                cumgood= cumsum(good)/sum(good), cumbad = cumsum(bad)/sum(bad))
        ][, ks := cumbad - cumgood]

    dfkslift <- rbind(data.table(group=0, good=0, bad=0, cumgood=0, cumbad=0, ks=0), dfkslift)

  }


  # plot, KS ------
  if ("ks" %in% type) {
    dfks <- dfkslift[ks == max(ks)][order(group)][1]
    print(paste0("KS: ", round(dfks$ks, 4) ))

    if (show_plot == TRUE) {
      pks <- ggplot(melt(dfkslift[,.(group, cumgood, cumbad, ks)], id="group"), aes(x=group, y=value, colour=variable)) +
        geom_line() + coord_fixed() +
        geom_segment(aes(x = dfks$group, y = 0, xend = dfks$group, yend = dfks$ks), colour = "red", linetype = "dashed", arrow=arrow(ends="both", length=unit(.2,"cm"))) +
        labs(x = "% of population", y = "% of total Good/Bad") +
        annotate("text", x=0.50, y=Inf, label="K-S", vjust=1.5, size=6)+
        annotate("text", x=dfks$group, y=dfks$ks, vjust = -0.2, label=paste0("KS: ", round(dfks$ks,4) ), colour = "blue") +
        annotate("text", x=0.20, y=0.80, vjust = -0.2, label="Bad", colour = "black") +
        annotate("text", x=0.80, y=0.55, vjust = -0.2, label="Good", colour = "black") +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_colour_manual(values=c("black", "black", "blue")) +
        theme_bw() + theme(legend.position="none")
    }
  }

  # plot, Lift ------
  if ("lift" %in% type) {
    plift <- ggplot(dfkslift[-1][,.(group, model = bad)], aes(x=group, y=model)) +
      geom_bar(stat = "identity", fill=NA, colour = "black") + coord_fixed() +
      geom_segment(aes(x = 0, y = 1/groupnum, xend = 1, yend = 1/groupnum), colour = "red", linetype = "dashed") +
      labs(x="% of population", y="%of total Bad") +
      annotate("text", x = 0.50, y=Inf, label="Lift", vjust=1.5, size=6)+
      guides(fill=guide_legend(title=NULL)) +
      scale_fill_manual(values=c("white", "grey")) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
      theme_bw() + theme(legend.position=c(0.5, 0.9), legend.direction="horizontal")

  }

  # data, dfrocpr ------
  if ("roc" %in% type | "pr" %in% type) {
    dfrocpr <-
      df1[order(pred)
      ][, .(countpred = .N, countP = sum(label==1), countN = sum(label==0)), by=pred
      ][, `:=`(FN = cumsum(countP), TN = cumsum(countN) )
      ][, `:=`(TP = sum(countP) - FN, FP = sum(countN) - TN)
      ][, `:=`(TPR = TP/(TP+FN), FPR = FP/(TN+FP), precision = TP/(TP+FP), recall = TP/(TP+FN)) ]

  }

  # plot, ROC ------
  if ("roc" %in% type) {
    AUC <- dfrocpr[, sum(TP/(TP+FN)*(FP/(TN+FP)-shift(FP/(TN+FP), fill=0, type="lead")))]
    print(paste0("AUC: ", round(AUC,4)))

    if (show_plot == TRUE) {
      proc <- ggplot(dfrocpr, aes(x=FPR, y=TPR)) +
        geom_ribbon(aes(ymin=0, ymax=TPR), fill="blue", alpha=0.1) +
        geom_line() + coord_fixed() +
        geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype = "dashed", colour="red") +
        annotate("text", x = 0.5, y=Inf, label="ROC", vjust=1.5, size=6) +
        annotate("text", x=0.55, y=0.45, label=paste0("AUC: ", round(AUC,4)), colour = "blue") +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        theme_bw()
    }
  }


  # plot, P-R ------
  if ("pr" %in% type) {
    dfpr <- dfrocpr[precision == recall]
    # print(paste0("BEP: ", round(dfpr$recall, 4)))
    if (show_plot == TRUE) {
      ppr <- ggplot(dfrocpr, aes(x=recall, y=precision)) +
        geom_line() + coord_fixed() +
        geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), colour = "red", linetype="dashed") +
        labs(x = "Recall", y = "Precision") +
        annotate("text", x = 0.5, y=Inf, label="P-R", vjust=1.5, size=6) +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        theme_bw()
    }
  }


  # export plot
  if (show_plot == TRUE) {
    plist <- paste0("p", type)
    # add title for first plot
    eval(parse(text = paste0(plist[1], " = ", plist[1], " + ggtitle(title)")))

    if (length(plist) == 1) {
      eval(parse(text = plist))
    } else if (length(plist) > 1) {
      # add title for second plot
      title=""
      eval(parse(text = paste0(plist[2:length(plist)], " = ", plist[2:length(plist)], " + ggtitle(title)")))

      # Arrange multiple plots
      p <- eval(parse(
        text = paste0("grid.arrange(", paste0(plist, collapse = ", "), ", nrow=", length(plist) %/% 2,", padding = 0)")
      ))
    }
  }

  return(p)
}

#' psi
#'
#' \code{perf_psi} provides population stability index (PSI).
#'
#' @param label_train label values of training dataset, such as 0s and 1s.
#' @param score_train credit score of training dataset.
#' @param label_test label values of testing dataset, such as 0s and 1s.
#' @param score_test credit score of testing dataset.
#' @param type Plot of stability type, such as "psi", "score_distr". Default c("psi", "score_distr"), which means to export both psi and score distibution plot.
#' @param title Title of plot, default "".
#' @param positive Value of positive class, default "bad|1".
#' @param x_limits x-axis limits, default c(0, 800)
#' @param x_tick_break xaxis ticker break, default 100
#' @param only_total logical value, default FALSE, which means whether to show total score only.
#' @param seed An integer. The specify seed is used for random sorting data, default: 186.
#' @return psi
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' library(scorecard)
#'
#' # Traditional Credit Scoring Using Logistic Regression
#' # load germancredit data
#' data("germancredit")
#'
#' # rename creditability as y
#' dt <- data.table(germancredit)[, `:=`(
#'   y = ifelse(creditability == "bad", 1, 0),
#'   creditability = NULL
#' )]
#'
#' # breaking dt into train and test ------
#' set.seed(125)
#' dt <- dt[sample(nrow(dt))]
#' # rowname of train
#' set.seed(345)
#' rn <- sample(nrow(dt), nrow(dt)*0.6)
#' # train and test dt
#' dt_train <- dt[rn]; dt_test <- dt[-rn];
#'
#' # woe binning ------
#' bins <- woebin(dt_train, "y")
#'
#' train <- woebin_ply(dt_train, bins)
#' test <- woebin_ply(dt_test, bins)
#'
#' # glm ------
#' m1 <- glm( y ~ ., family = "binomial", data = train)
#' # summary(m1)
#'
#' # Select a formula-based model by AIC
#' m_step <- step(m1, direction="both")
#' m2 <- eval(m_step$call)
#' # summary(m2)
#'
#' # performance ------
#' # predicted proability
#' train$pred <- predict(m2, type='response', train)
#' test$pred <- predict(m2, type='response', test)
#'
#' # ks & roc plot
#' perf_plot(train$y, train$pred, title = "train")
#' perf_plot(train$y, train$pred, title = "test")
#'
#' # score
#' card <- scorecard(bins, m2)
#'
#' # score
#' train$score <- scorecard_ply(dt_train, card)
#' test$score <- scorecard_ply(dt_test, card)
#'
#' # psi
#' perf_psi(train$y, train$score, test$y, test$score,
#'   x_limits = c(0, 700), x_tick_break = 100)
#'
#' perf_psi(train$y, train$score, test$y, test$score,
#'   type = "psi", x_limits = c(0, 700), x_tick_break = 100)
#'
#' perf_psi(train$y, train$score, test$y, test$score,
#'   type = "score_distr", x_limits = c(0, 700), x_tick_break = 100)
#' }
#' @import data.table ggplot2 gridExtra
#' @export
#'
perf_psi <- function(label_train, score_train, label_test, score_test, type=c("psi", "score_distr"), title="", positive="bad|1", x_limits=c(100,700), x_tick_break=50, only_total=FALSE, seed=186) {
  # psi = sum((Actual% - Expected%)*ln(Actual%/Expected%))
  . = id = label = score = bin = distr = count = train = test = A = E = AE = logAE = PSI = goodbad = count = bad = badprob = distr = midbin = bin1 = bin2 = badprob2 = NULL # no visible binding for global variable

  # inputs checking
  if (!is.vector(label_train) | !is.vector(score_train) | !is.vector(label_test) | !is.vector(score_test)) break
  if (length(label_train) != length(score_train) | length(label_test) != length(score_test)) break

  # breakpoints
  brkp <- unique(c(
    floor(min(c(score_train, score_test))/x_tick_break)*x_tick_break,
    seq(x_limits[1]+x_tick_break, x_limits[2]-x_tick_break, by=x_tick_break),
    ceiling(max(c(score_train, score_test))/x_tick_break)*x_tick_break
  ))

  # random sort datatable
  set.seed(seed)
  dat <- rbindlist(
    list(
      train=data.table(label=label_train, score=score_train),
      test=data.table(label=label_test, score=score_test)
    ), idcol = "id"
  )[
    sample(1:(length(label_train)+length(label_test)))
  ][,.(id, label, score)
  ][, `:=`(id=factor(id, levels=c("train", "test")),
           goodbad=ifelse(label==1, "bad", "good"))
  ][, bin:=cut(score, brkp, right = FALSE, dig.lab = 10, ordered_result = F)]


  # PSI function
  psi <- function(dat) {
    dcast(
      dat[,.(count=.N), keyby=c("id", "bin")
        ][,distr := count/sum(count), by="id"],
      bin ~ id, value.var="count", fill = 0
    )[
      # , (c("train", "test")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("train", "test")][
        , `:=`(A=train/sum(train), E=test/sum(test))
      ][, `:=`(AE = A-E, logAE = log(A/E))
      ][, `:=`(PSI = AE*logAE)
      ][, `:=`(PSI = ifelse(PSI==Inf, 0, PSI))][, sum(PSI)]

  }


  # plot PSI ------
  if ("psi" %in% type) {
    # print psi
    print(paste0(
      "PSI: Total=", round(psi(dat), 4),
      "; Good=", round(psi(dat[label==0]), 4),
      "; Bad=", round(psi(dat[label==1]), 4), ";"
    ))

    # plot PSI
    if (only_total) {
      p_psi <-
        ggplot() +
        geom_density(data = dat, aes(score, linetype=id), colour="grey50") +
        annotate("text", x = x_limits[1], y=Inf, label=paste0("Total=", round(psi(dat), 4), ";" ), vjust=5, hjust=0, size=3)
      # x = (x_limits[1]+x_limits[2])/2

    } else {
      p_psi <-
        ggplot() +
        geom_density(data=dat, aes(score, linetype=id, colour=goodbad)) +
        annotate("text", x = x_limits[1], y=Inf, label=paste0("Total=", round(psi(dat), 4), "; Good=", round(psi(dat[label==0]), 4), "; Bad=", round(psi(dat[label==1]), 4), ";" ), vjust=5, hjust=0, size=3)

    }

    p_psi <-
      p_psi +
      annotate("text", x = x_limits[1], y=Inf, label="PSI", vjust=1.5, hjust=0, size=6) +
      # ggtitle(title, subtitle = paste0(
      #   "Total=", round(psi(dat), 4),
      #   "; Good=", round(psi(dat[label==0]), 4),
      #   "; Bad=", round(psi(dat[label==1]), 4), ";"
      # ) ) +
      labs(x=NULL, y="Score distribution", linetype=NULL, colour=NULL) +
      scale_x_continuous(expand = c(0, 0), breaks = seq(x_limits[1], x_limits[2], by=x_tick_break), limits = x_limits) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_bw() +
      theme(plot.title=element_text(vjust = -2.5), legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_blank())

    if (title != "" & !is.na(title)) p_psi <- p_psi + ggtitle(title)
  }


  # plot score distribution and bad probability ------
  if ("score_distr" %in% type) {
    # score distribution and bad probability
    if (only_total) {
      distr_prob <- dat[
        order(bin)
        ][,.(count=.N, bad=sum(label==1)), keyby=c("bin")
          ][,`:=`(
            distr = count/sum(count),
            badprob=bad/count
          )][,`:=`(badprob2=badprob*max(distr))]
    } else {
      distr_prob <- dat[
        order(bin)
        ][,.(count=.N, bad=sum(label==1)), keyby=c("id", "bin")
          ][,`:=`(
            distr = count/sum(count),
            badprob=bad/count
          ), by = "id"][,`:=`(badprob2=badprob*max(distr)), by = "id"]
    }
    distr_prob <-
      distr_prob[, `:=`(
        bin1 = as.integer(sub("\\[(.+),(.+)\\)", "\\1", bin)),
        bin2 = as.integer(sub("\\[(.+),(.+)\\)", "\\2", bin))
      )][, midbin := (bin1+bin2)/2 ]


    # plot
    if (only_total) {
      p_score_distr <-
        ggplot(distr_prob) +
        geom_bar(aes(x=bin, y=distr), stat="identity", fill="lightblue") +
        geom_line(aes(x=bin, y=badprob2, group=1), colour = "blue") +
        geom_point(aes(x=bin, y=badprob2), colour = "blue", shape=21, fill="white")

    } else {
      p_score_distr <-
        ggplot(distr_prob[, id:=factor(id, levels=c( "train", "test"))]) +
        geom_bar(aes(x=bin, y=distr, fill=id), alpha=0.6, stat="identity", position="dodge") +
        geom_line(aes(x=bin, y=badprob2, group=id, colour=id, linetype=id)) +
        geom_point(aes(x=bin, y=badprob2, colour=id), shape=21, fill="white") +
        guides(fill=guide_legend(title="Distribution"), colour=guide_legend(title="Probability"), linetype=guide_legend(title="Probability"))
    }
    p_score_distr <-
      p_score_distr +
      scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~./max(distr_prob$distr), name = "Bad probability")) +
      labs(x=NULL, y="Score distribution") +
      theme_bw() +
      # theme(legend.position="bottom", legend.direction="horizontal") +
      theme(plot.title=element_text(vjust = -2.5), legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_blank())


    if (title != "" & !is.na(title)) p_score_distr <- p_score_distr + ggtitle(title)
  }

  plist <- paste0("p_", type)

  # Arrange multiple plots
  if (length(type)==1) {
    if (type == "score_distr") {
      return(p_score_distr)
      # return(list(p=p_score_distr, d=distr_prob))
    } else if (type == "psi") {
      return(p_psi)
    } else {
      break
    }
  } else {
    return(grid.arrange(p_psi, p_score_distr, nrow=1, padding=0))
  }
}
