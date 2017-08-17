#' ks, roc, lift, pr
#'
#' This function provides kolmogorov-smirnow(ks), ROC, lift and precision-recall curves based on label and predicted probability values.
#'
#' @name perf_plot
#' @param label label values, such as 0s and 1s.
#' @param pred predicted probability values.
#' @param title plot title, default "train".
#' @param groupnum the number of group numbers, default: 20.
#' @param type performance plot types, such as "ks","lift","roc","pr", default: c("ks", "roc").
#' @param positive value of positive class, defaults: bad or 1.
#' @param plot logical value, default TRUE.
#' @param seed seed value for random sort data frame, defalut: 186.
#' @return ks, roc, lift, pr
#' @export
#' @examples
#' # # Traditional Credit Scoring Using Logistic Regression
#' # library(woebin)
#' # # load germancredit data
#' # data("germancredit")
#' #
#' # # rename creditability as y
#' # dt <- data.table(germancredit)[, `:=`(
#' #   y = ifelse(creditability == "bad", 1, 0),
#' #   creditability = NULL
#' # )]
#' #
#' # # woe binning ------
#' # bins <- woebin(dt, "y")$bins
#' # dt_woe <- woebin_ply(dt, "y", bins)
#' #
#' # # glm ------
#' # m1 <- glm( y ~ ., family = "binomial", data = dt_woe)
#' # # summary(m1)
#' #
#' # # Select a formula-based model by AIC
#' # m_step <- step(m1, direction="both")
#' # m2 <- eval(m_step$call)
#' # # summary(m2)
#' #
#' # # performance ------
#' # # predicted proability
#' # dt_woe$pred <- predict(m2, type='response', dt_woe)
#' #
#' # # performace
#' # # only ks & auc values
#' # perf_plot(dt_woe$y, dt_woe$pred, plot=FALSE)
#' #
#' # # ks & roc plot
#' # perf_plot(dt_woe$y, dt_woe$pred)
#' #
#' # # ks, lift, roc & pr plot
#' # perf_plot(dt_woe$y, dt_woe$pred, type = c("ks","lift","roc","pr"))
#'
perf_plot <- function(label, pred, title="train", groupnum=20, type=c("ks", "roc"), positive="bad|1", plot=TRUE, seed=186) {
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

    if (plot == TRUE) {
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

    if (plot == TRUE) {
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
    if (plot == TRUE) {
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
  if (plot == TRUE) {
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
      eval(parse(
        text = paste0("grid.arrange(", paste0(plist, collapse = ", "), ", nrow=", length(plist) %/% 2,", padding = 0)")
      ))
    }
  }


}

#' psi
#'
#' This function provides population stability index (PSI).
#'
#' @name perf_psi
#' @param label_train label values of training dataset, such as 0s and 1s.
#' @param score_train credit score of training dataset.
#' @param label_test label values of testing dataset, such as 0s and 1s.
#' @param score_test credit score of testing dataset.
#' @param title plot title, default "train".
#' @param positive Name of positive class, defaults: bad or 1.
#' @param plot logical value, default TRUE. It means whether to display plot.
#' @param xlimits xaxis limits, default c(0, 800)
#' @param x_tick_break xaxis ticker break, default 100
#' @param line_total logical value, default FALSE, which means not display the line of total PSI
#' @param seed seed value for random sort data frame, defalut: 186.
#' @return psi
#' @export
#' @examples
#' # # Traditional Credit Scoring Using Logistic Regression
#' # library(woebin)
#' # # load germancredit data
#' # data("germancredit")
#' #
#' # # rename creditability as y
#' # dt <- data.table(germancredit)[, `:=`(
#' #   y = ifelse(creditability == "bad", 1, 0),
#' #   creditability = NULL
#' # )]
#' #
#' # # breaking dt into train and test ------
#' # set.seed(125)
#' # dt <- dt[sample(nrow(dt))]
#' # # rowname of train
#' # set.seed(345)
#' # rn <- sample(nrow(dt), nrow(dt)*0.6)
#' # # train and test dt
#' # dt_train <- dt[rn]; dt_test <- dt[-rn];
#' #
#' # # woe binning ------
#' # bins <- woebin(dt_train, "y")$bins
#' #
#' # train <- woebin_ply(dt_train, "y", bins)
#' # test <- woebin_ply(dt_test, "y", bins)
#' #
#' # # glm ------
#' # m1 <- glm( y ~ ., family = "binomial", data = train)
#' # # summary(m1)
#' #
#' # # Select a formula-based model by AIC
#' # m_step <- step(m1, direction="both")
#' # m2 <- eval(m_step$call)
#' # # summary(m2)
#' #
#' # # performance ------
#' # # predicted proability
#' # train$pred <- predict(m2, type='response', train)
#' # test$pred <- predict(m2, type='response', test)
#' #
#' # # ks & roc plot
#' # perf_plot(train$y, train$pred, title = "train")
#' # perf_plot(train$y, train$pred, title = "test")
#' #
#' # # score
#' # train$score <- scorecards(train, "y", bins, m2)$score
#' # test$score <- scorecards(test, "y", bins, m2)$score
#' #
#' # # psi
#' # perf_psi(train$y, train$score, test$y, test$score, x_limits = c(0, 700), x_tick_break = 100)
#'
perf_psi <- function(label_train, score_train, label_test, score_test, title="PSI", positive="bad|1", plot=TRUE, x_limits=c(100,650), x_tick_break=50, line_total=FALSE, seed=186) {
  # psi = sum((Actual% - Expected%)*ln(Actual%/Expected%))

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
        ][,dist := count/sum(count), by="id"],
      bin ~ id, value.var="count", fill = 0
    )[
      # , (c("train", "test")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("train", "test")][
        , `:=`(A=train/sum(train), E=test/sum(test))
      ][, `:=`(AE = A-E, logAE = log(A/E))
      ][, `:=`(PSI = AE*logAE)
      ][, `:=`(PSI = ifelse(PSI==Inf, 0, PSI))][, sum(PSI)]

  }

  # print psi
  print(paste0(
    "PSI: Total=", round(psi(dat), 4),
    "; Good=", round(psi(dat[label==0]), 4),
    "; Bad=", round(psi(dat[label==1]), 4), ";"
  ))

  # plot PSI
  if (plot) {
    # score distribution and bad probability
    distr_prob <- dat[
      order(bin)
      ][,.(count=.N, bad=sum(label==1)),by="bin"
        ][,`:=`(
          dist = count/sum(count), badprob=bad/count,
          bin1 = as.integer(sub("\\[(.+),(.+)\\)", "\\1", bin)),
          bin2 = as.integer(sub("\\[(.+),(.+)\\)", "\\2", bin))
        )][,`:=`(midbin=(bin1+bin2)/2, badprob2=badprob*max(dist))]

    p_dp <-
      ggplot(distr_prob) +
      geom_bar(aes(x=bin, y=dist), stat="identity", fill="lightblue") +
      geom_line(aes(x=bin, y=badprob2, group=1), colour = "blue") +
      geom_point(aes(x=bin, y=badprob2), colour = "blue", shape=21, fill="white") +
      scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~./max(distr_prob$dist), name = "Bad probability")) +
      labs(x=NULL, y="Score distribution", fill=NULL) +
      theme_bw() +
      theme(legend.position="bottom", legend.direction="horizontal")

    p_psi <-
      ggplot() +
      geom_density(data=dat, aes(score, linetype=id, colour=goodbad)) +
      annotate("text", x = (x_limits[1]+x_limits[2])/2, y=Inf, label="PSI", vjust=1.5, size=6) +
      annotate("text", x = (x_limits[1]+x_limits[2])/2, y=Inf, label=paste0("Total=", round(psi(dat), 4), "; Good=", round(psi(dat[label==0]), 4), "; Bad=", round(psi(dat[label==1]), 4), ";" ), vjust=5, size=3) +
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



    if (line_total) {
      p_psi <- p_psi + geom_density(data = dat, aes(score, linetype=id))
    }

    # grid.arrage
    p<- grid.arrange(p_psi, p_dp, nrow=1, padding = 0)

  }


  return(p)
}

