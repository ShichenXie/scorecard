#' ks value and ks curve
#'
#' This function calculate kolmogorov-smirnow(ks) value and plot ks curve based on provided label and predicted probability values.
#'
#' @param label label values, such as 0s and 1s.
#' @param pred predicted probability values.
#' @param groupnum the number of group numbers, default: 20.
#' @param type performance plot types: "ks","lift","roc","pr". Default: c("ks").
#' @param plot logical value, default: TRUE.
#' @param seed seed value for random sort data frame, defalut: 186.
#' @return ks value and ks curve.
#' @export
#' @examples
#' # # load germancredit data
#' # data("germancredit")
#' # xynames <- names(germancredit)
#' # # data.table
#' # set.seed(1255)
#' # dt <- setnames(
#' #   data.table(germancredit)[sample(nrow(germancredit))],
#' #   c(paste0("x",1:20), "y")
#' # )[, y:=ifelse(y=="bad", 1, 0)]
#' #
#' # # woe binning
#' # bins <- woebin(dt, "y", stop_limit = 0.05)
#' # dt_woe <- woebin_ply(dt, bins, "y")
#' #
#' # # Breaking Data into Training and Test Sample
#' # set.seed(345)
#' # d <- sample(nrow(dt), nrow(dt)*0.6)
#' # train <- dt_woe[d]; test <- dt_woe[-d];
#' #
#' # # Traditional Credit Scoring Using Logistic Regression
#' # # model I
#' # m1 <- glm(y~., family=binomial(), data=train)
#' # # summary(m1)
#' # # Select a formula-based model by AIC
#' # step(m1, direction="both")
#' # # model II
#' # m2 <- glm(formula = y ~
#' #         x20_woe + x18_woe + x14_woe + x13_woe + x10_woe + x9_woe +
#' #         x8_woe + x6_woe + x5_woe + x4_woe + x3_woe + x2_woe + x1_woe,
#' #       family = binomial(), data = train)
#' # # summary(m2)
#' #
#' # # score test data set
#' # train$score <- predict(m2, type='response', train)
#' # test$score <- predict(m2, type='response', test)
#' #
#' # # performace plot
#' # perf_plot(train$y, train$score)
#' # perf_plot(test$y, test$score)
#'
perf_plot <- function(label, pred, groupnum=20, type=c("ks", "roc"), plot=TRUE, seed=186) {
  set.seed(seed)

  df1 <- data.table(label=ifelse(grepl("bad|1", as.character(label)), 1, 0), pred=pred)[!is.na(label)][sample(1:length(pred))]

  # data, dfkslift ------
  if ("ks" %in% type | "lift" %in% type) {
    if (groupnum == "N") groupnum <- length(pred)

    dfkslift <- df1[order(-pred)
                    ][, group := ceiling(as.integer(row.names(.SD))/(.N/groupnum))
                      ][,.(good = sum(label==0), bad = sum(label==1)), by=group
                        ][,`:=`(group= as.integer(row.names(.SD))/.N,
                                good = good/sum(good), bad  = bad/sum(bad),
                                cumgood= cumsum(good)/sum(good), cumbad = cumsum(bad)/sum(bad))
                          ][, ks := cumbad - cumgood]

    dfkslift <- rbind(data.table(group=0, good=0, bad=0, cumgood=0, cumbad=0, ks=0), dfkslift)

  }


  # KS ------
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

  # Lift ------
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
    dfrocpr <- df1[order(pred)
                   ][, .(countpred = .N, countP = sum(label==1), countN = sum(label==0)), by=pred
                     ][, `:=`(FN = cumsum(countP), TN = cumsum(countN) )
                       ][, `:=`(TP = sum(countP) - FN, FP = sum(countN) - TN)
                         ][, `:=`(TPR = TP/(TP+FN), FPR = FP/(TN+FP), precision = TP/(TP+FP), recall = TP/(TP+FN)) ]

  }

  # ROC ------
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


  # P-R ------
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


  if (plot == TRUE) {
    plist <- paste0("p", type)
    if (length(plist) == 1) {
      eval(parse(text = plist))
    } else if (length(plist) > 1) {
      eval(parse(text = paste0("grid.arrange(", paste0(plist, collapse = ", "), ", nrow=", length(plist) %/% 2,", padding = 0)")))
    }
  }


}











