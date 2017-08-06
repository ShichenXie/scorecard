#' ks, roc, lift, pr
#'
#' This function provides kolmogorov-smirnow(ks), ROC, lift and precision-recall curves based on label and predicted probability values.
#'
#' @name perf_plot
#' @param dt_labelpred data with only label and predicted probability values.
#' @param y name of y variable.
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
#' # # score
#' # train_score <- scorecards(train, y, bins, m2)$score
#' # test_score <- scorecards(test, y, bins, m2)$score
#' #
#' # # performace plot
#' # perf_plot(train[,.(y,pred)], y, title="train")
#' # perf_plot(test[,.(y,pred)], y, title="test")
#' #
#' # perf_psi(train_score[,.(y, score)], test_score[,.(y, score)], y)
#' #
#' # # scorecards
#' # cards <- scorecards(train, y, bins, m2)$scorecards
#'
perf_plot <- function(dt_labelpred, y, title="train", groupnum=20, type=c("ks", "roc"), positive="bad|1", plot=TRUE, seed=186) {
  # inputs checking
  if (ncol(dt_labelpred) != 2 ) break
  if (!(y %in% names(dt_labelpred))) break
  kdt <- copy(data.table(dt_labelpred))

  set.seed(seed)
  df1 <- data.table(
    label=ifelse(grepl(positive, as.character(kdt[[y]])), 1, 0),
    pred=kdt[[setdiff(names(kdt),y)]]
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
#' @param train_labelscore train data with only label and score
#' @param test_labelscore test data with only label and score
#' @param y Name of y variable.
#' @param title plot title, default "train".
#' @param groupnum the number of group numbers, default: 20.
#' @param positive Name of positive class, defaults: bad or 1.
#' @param plot logical value, default TRUE. It means whether to display plot.
#' @param plot_total logical value, default FALSE, which means not display the line of total PSI
#' @param seed seed value for random sort data frame, defalut: 186.
#' @return psi
#' @export
perf_psi <- function(train_labelscore, test_labelscore, y, title="PSI", groupnum=20, positive="bad|1", plot=TRUE, plot_total=FALSE, seed=186) {
  # psi = sum((实际占比-预期占比)* ln(实际占比/预期占比))
  # inputs checking
  if (ncol(train_labelscore) != 2 | ncol(test_labelscore) != 2 ) break
  if (!(y %in% names(train_labelscore) | y %in% names(test_labelscore))) break

  # random order datatable
  set.seed(seed)
  dat <- rbindlist(
    list(train=train_labelscore, test=test_labelscore), idcol = "id"
  )[
    sample(1:(nrow(train_labelscore)+nrow(test_labelscore)))
  ][,.(id, label=y, score)
  ][, id:=factor(id, levels=c("train", "test"))]


  # PSI function
  psi <- function(dat, groupnum) {
    # groupnum
    if (groupnum == "N") groupnum <- min(nrow(train_labelscore), nrow(test_labelscore))

    brk <- pretty(dat$score, groupnum)
    brk <- unique(c(-Inf, brk[2:(length(brk)-1)], Inf))

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



# scorecards ------
ab <- function(p0=600, odds0=1/60, pdo=50) {
  # sigmoid function
  # library(ggplot2)
  # ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = function(x) 1/(1+exp(-x)))

  # p(y=1) <- 1/(1+exp(-z)), # z = beta0+beta1*x1+...+betar*xr = beta*x
  #==># z <- log(p/(1-p)), # odds = p/(1-p) # bad/good
  #==># z <- log(odds)
  #==># score = a - b*log(odds)

  b <- pdo/log(2)
  a <- p0 + b*log(odds0/(1+odds0))

  return(list(a=a, b=b))
}
#' credit score & scorecards
#'
#' This function calculates credit score based on predited probability.
#' @name scorecards
#' @param dt_woe input data that converted into woe
#' @param y name of y variable.
#' @param bins Binning information generated from woebin function
#' @param model fitted glm model
#' @param p0 target points, default 600
#' @param odds0 target odds, default 1/60
#' @param pdo Points to Double the Odds, default 50
#' @return score and scorecards
#' @export
#'
scorecards <- function(dt_woe, y, bins, model, p0=600, odds0=1/60, pdo=50) {
  aabb <- ab(p0, odds0, pdo)
  a <- aabb$a; b <- aabb$b;
  # odds <- pred/(1-pred); score <- a - b*log(odds)

  # coefficients
  coef <- data.frame(summary(model)$coefficients)
  coef$var <- row.names(coef)
  coef <- setnames(data.table(coef)[,c(1,5),with=FALSE], c("Estimate", "var_woe"))[, var := gsub("_woe$", "", var_woe) ][]


  # scorecards & score
  scorecards <- list()
  dt_score <- data.table(y=dt_woe[[y]])
  scorecards[["basepoints"]] <- data.table( variable = "basepoints", bin = NA, woe = NA, points = round(a - b*coef[1,Estimate]) )

  for (i in coef[-1,var]) {
    scorecards[[i]] <- bins[[i]][ ,.(
      variable, bin, woe, points = round(-b*coef[var==i, Estimate]*woe)
    ) ]

    dt_score[,(paste0(i,"_points")) := round(-b*coef[var==i, Estimate]*dt_woe[[paste0(i, "_woe")]])]
  }

  # total score
  dt_score[["score"]] <- scorecards[["basepoints"]][,points] + rowSums(dt_score[, paste0(coef[-1,var], "_points"), with=FALSE])
  # dt_score <- dt_woe[,c(paste0(coef[-1,var], "_points"), y, "score"), with=FALSE]

  return(list(scorecards=scorecards, score = dt_score))

}


# reference
# https://cn.mathworks.com/help/finance/case-study-for-a-credit-scorecard-analysis.html?requestedDomain=www.mathworks.com#zmw57dd0e33220
