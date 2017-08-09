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
#' @param positive Name of positive class, defaults: bad or 1.
#' @param plot logical value, default: TRUE.
#' @param seed seed value for random sort data frame, defalut: 186.
#' @return ks, roc, lift, pr
#' @export
#' @examples
#' # library(woebin)
#' # # load germancredit data
#' # data("germancredit")
#' # dt <- setnames(
#' #   data.table(germancredit), c(paste0("x",1:20), "y")
#' # )[, y:=ifelse(y=="bad", 1, 0)]
#' # # x y names
#' # y <- "y"
#' #
#' # # iv woe filter ------
#' # # variable filter I
#' # dt_filter <- var_filter(dt, y)
#' #
#' # # woe binning
#' # bins <- woebin(dt_filter, y, stop_limit = 0.1)$bins
#' # dt_woe <- woebin_ply(dt_filter, bins, y)
#' #
#' # # variable filter II
#' # dt_woe_filter <- var_filter(dt_woe, y)
#' #
#' # # # lasso filter ------
#' # # library(h2o)
#' # # # h2o data
#' # # localH2O <- h2o.init()
#' # # dth2o <- as.h2o(dt_woe_filter)
#' # #
#' # # # Breaking Data into Training and Test Sample
#' # # set.seed(345)
#' # # dt.split <- h2o.splitFrame(data=dth2o, ratios=0.6)
#' # # dt.train <- dt.split[[1]]; dt.test <- dt.split[[2]];
#' # #
#' # # # h2o.glm lasso
#' # # fit <- h2o.glm(x=names(dt_woe_filter), y, dt.train, validation_frame=dt.test, family = "binomial", nfolds = 0, alpha = 1, lambda_search = TRUE)
#' # # # summary(fit)
#' # # h2o_var <- data.table(h2o.varimp(fit))[!is.na(coefficients) & coefficients > 0]
#' # # dt_woe_lasso <- dt_woe_filter[, c(h2o_var$names, y), with=FALSE]
#' #
#' # # glm ------
#' # # Breaking Data into Training and Test Sample
#' # set.seed(1255)
#' # dat <- data.table(dt_woe_filter)[sample(nrow(dt_woe_filter))]
#' # set.seed(456)
#' # d <- sample(nrow(dat), nrow(dat)*0.6)
#' # train <- dat[d]; test <- dat[-d];
#' #
#' # # Traditional Credit Scoring Using Logistic Regression ######
#' # # a. model I
#' # # remove variables that coefficients == NEG or Pr_z > 0.1
#' # rm_var_num <- 1
#' # sel_var <- names(train) #names(xy_selected_lasso)
#' # while (rm_var_num > 0) {
#' #   print(rm_var_num)
#' #
#' #   m1 <- glm(
#' #     y ~ ., family = "binomial",
#' #     data = train[, sel_var, with=FALSE]
#' #   )
#' #
#' #   # coefficients
#' #   m1_coef <- data.frame(summary(m1)$coefficients)
#' #   m1_coef$var <- row.names(m1_coef)
#' #   m1_coef <- data.table(m1_coef)[var != "(Intercept)"]
#' #   setnames(m1_coef, c("Estimate", "Std_Error", "z_value", "Pr_z", "var"))
#' #
#' #   # selected variables
#' #   sel_var <- c(m1_coef[Estimate > 0 & Pr_z < 0.1, var], y)
#' #
#' #   # number of variables that coefficients == NEG or Pr_z > 0.1
#' #   rm_var_num <- m1_coef[Estimate <= 0 | Pr_z > 0.1][, .N]
#' # }
#' # # summary(m1)
#' #
#' # # b. model II
#' # # Select a formula-based model by AIC
#' # m_step <- step(m1, direction="both")
#' # m2 <- eval(m_step$call)
#' # # summary(m2)
#' #
#' # # score & performance ------
#' # # predicted proability
#' # train$pred <- predict(m2, type='response', train)
#' # test$pred <- predict(m2, type='response', test)
#' #
#' # # credit score
#' # train_score <- scorecards(train, y, bins, m2)$score
#' # test_score <- scorecards(test, y, bins, m2)$score
#' #
#' # # performace plot of ks & roc
#' # perf_plot(train$y, train$pred, title="train")
#' # perf_plot(test$y, test$pred, title="test")
#' #
#' # perf_psi(train_score$y, train_score$score, test_score$y, test_score$score)
#' #
#' # # scorecards
#' # cards <- scorecards(train, y, bins, m2)$cards
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
      eval(parse(text = paste0(plist[2], " = ", plist[2], " + ggtitle(title)")))

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
#' @param groupnum the number of group numbers, default: 20.
#' @param positive Name of positive class, defaults: bad or 1.
#' @param plot logical value, default TRUE. It means whether to display plot.
#' @param plot_total logical value, default FALSE, which means not display the line of total PSI
#' @param seed seed value for random sort data frame, defalut: 186.
#' @return psi
#' @export
#' @examples
#' # library(woebin)
#' # # load germancredit data
#' # data("germancredit")
#' # dt <- setnames(
#' #   data.table(germancredit), c(paste0("x",1:20), "y")
#' # )[, y:=ifelse(y=="bad", 1, 0)]
#' # # x y names
#' # y <- "y"
#' #
#' # # iv woe filter ------
#' # # variable filter I
#' # dt_filter <- var_filter(dt, y)
#' #
#' # # woe binning
#' # bins <- woebin(dt_filter, y, stop_limit = 0.1)$bins
#' # dt_woe <- woebin_ply(dt_filter, bins, y)
#' #
#' # # variable filter II
#' # dt_woe_filter <- var_filter(dt_woe, y)
#' #
#' # # # lasso filter ------
#' # # library(h2o)
#' # # # h2o data
#' # # localH2O <- h2o.init()
#' # # dth2o <- as.h2o(dt_woe_filter)
#' # #
#' # # # Breaking Data into Training and Test Sample
#' # # set.seed(345)
#' # # dt.split <- h2o.splitFrame(data=dth2o, ratios=0.6)
#' # # dt.train <- dt.split[[1]]; dt.test <- dt.split[[2]];
#' # #
#' # # # h2o.glm lasso
#' # # fit <- h2o.glm(x=names(dt_woe_filter), y, dt.train, validation_frame=dt.test, family = "binomial", nfolds = 0, alpha = 1, lambda_search = TRUE)
#' # # # summary(fit)
#' # # h2o_var <- data.table(h2o.varimp(fit))[!is.na(coefficients) & coefficients > 0]
#' # # dt_woe_lasso <- dt_woe_filter[, c(h2o_var$names, y), with=FALSE]
#' #
#' # # glm ------
#' # # Breaking Data into Training and Test Sample
#' # set.seed(1255)
#' # dat <- data.table(dt_woe_filter)[sample(nrow(dt_woe_filter))]
#' # set.seed(456)
#' # d <- sample(nrow(dat), nrow(dat)*0.6)
#' # train <- dat[d]; test <- dat[-d];
#' #
#' # # Traditional Credit Scoring Using Logistic Regression ######
#' # # a. model I
#' # # remove variables that coefficients == NEG or Pr_z > 0.1
#' # rm_var_num <- 1
#' # sel_var <- names(train) #names(xy_selected_lasso)
#' # while (rm_var_num > 0) {
#' #   print(rm_var_num)
#' #
#' #   m1 <- glm(
#' #     y ~ ., family = "binomial",
#' #     data = train[, sel_var, with=FALSE]
#' #   )
#' #
#' #   # coefficients
#' #   m1_coef <- data.frame(summary(m1)$coefficients)
#' #   m1_coef$var <- row.names(m1_coef)
#' #   m1_coef <- data.table(m1_coef)[var != "(Intercept)"]
#' #   setnames(m1_coef, c("Estimate", "Std_Error", "z_value", "Pr_z", "var"))
#' #
#' #   # selected variables
#' #   sel_var <- c(m1_coef[Estimate > 0 & Pr_z < 0.1, var], y)
#' #
#' #   # number of variables that coefficients == NEG or Pr_z > 0.1
#' #   rm_var_num <- m1_coef[Estimate <= 0 | Pr_z > 0.1][, .N]
#' # }
#' # # summary(m1)
#' #
#' # # b. model II
#' # # Select a formula-based model by AIC
#' # m_step <- step(m1, direction="both")
#' # m2 <- eval(m_step$call)
#' # # summary(m2)
#' #
#' # # score & performance ------
#' # # predicted proability
#' # train$pred <- predict(m2, type='response', train)
#' # test$pred <- predict(m2, type='response', test)
#' #
#' # # credit score
#' # train_score <- scorecards(train, y, bins, m2)$score
#' # test_score <- scorecards(test, y, bins, m2)$score
#' #
#' # # performace plot of ks & roc
#' # perf_plot(train$y, train$pred, title="train")
#' # perf_plot(test$y, test$pred, title="test")
#' #
#' # perf_psi(train_score$y, train_score$score, test_score$y, test_score$score)
#' #
#' # # scorecards
#' # cards <- scorecards(train, y, bins, m2)$cards
#'
perf_psi <- function(label_train, score_train, label_test, score_test, title="PSI", groupnum=20, positive="bad|1", plot=TRUE, plot_total=FALSE, seed=186) {
  # psi = sum((Actual% - Expected%)*ln(Actual%/Expected%))

  # inputs checking
  if (!is.vector(label_train) | !is.vector(score_train) | !is.vector(label_test) | !is.vector(score_test)) break
  if (length(label_train) != length(score_train) | length(label_test) != length(score_test)) break

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
  ][, id:=factor(id, levels=c("train", "test"))]


  # PSI function
  psi <- function(dat, groupnum) {
    # groupnum
    if (groupnum == "N") groupnum <- min(length(label_train), length(label_test))

    brk <- pretty(dat$score, groupnum)
    brk_p <- brk[which(brk>0)] # positive breakpoints
    brk <- unique(c(-Inf, brk_p[-length(brk_p)], Inf))

    # dat2
    dat2 <- dat[
      order(id, score)
      ][, group := cut(score, brk, right = FALSE, dig.lab = 10, ordered_result = F)
      ][,.(count=.N), by=c("id", "group")
      ][,.(group, count, dist = count/sum(count)), by="id"]

    dcast(dat2, group ~ id, value.var="count", fill = 0)[
      # , (c("train", "test")) := lapply(.SD, function(x) ifelse(x==0, 0.99, x)), .SDcols = c("train", "test")][
        , `:=`(A=train/sum(train), E=test/sum(test))
      ][, `:=`(AE = A-E, logAE = log(A/E))
      ][, `:=`(PSI = AE*logAE)
      ][, `:=`(PSI = ifelse(PSI==Inf, 0, PSI))][][, sum(PSI)]

  }


  # print psi
  print(paste0(
    "PSI: Total=", round(psi(dat, groupnum), 4),
    "; Good=", round(psi(dat[label==0], groupnum), 4),
    "; Bad=", round(psi(dat[label==1], groupnum), 4), ";"
  ))

  # plot PSI
  if (plot) {
    p <- ggplot(rbindlist(list(bad=dat[label==1], good=dat[label==0]), idcol = "goodbad"), aes(score)) +
      geom_density(aes(linetype=id, colour=goodbad)) +
      ggtitle(title, subtitle = paste0(
        "Total=", round(psi(dat, groupnum), 4),
        "; Good=", round(psi(dat[label==0], groupnum), 4),
        "; Bad=", round(psi(dat[label==1], groupnum), 4), ";"
      )) +
      labs(linetype=NULL, colour=NULL) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 800)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_bw() + theme(legend.position=c(1,1), legend.justification=c(1,1), legend.background=element_blank())

  }
  if (plot_total) {
    p <- p + geom_density(data = dat, aes(score, linetype=id))
  }

  return(p)
}

