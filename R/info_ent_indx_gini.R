# information entropy ------
# ent(D) = -\sum_k(p_k*log_2(p_k)), if p_k=0 then p_k*log_2(p_k)=0

# information gain (ID3)
# gain = ent(D) - \sum(abs(\frac{D^v}{D})*ent(D^v))

# gain ratio (C4.5)
# gain_ratio(D,a) = Gain(D,a)/IV(a)
# instrinsic value: IV(a) = -\sum_v( abs(\frac{D^v}{D})*log_2(abs(\frac{D^v}{D})) )


# #' Information Entropy
# #'
# #' This function calculates information entropy (ie) for multiple x variables.
# #'
# #' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
# #' @param y Name of y variable.
# #' @param x Name of x variables. Defaults to NULL. If x is NULL, then all variables except y are counted as x variables.
# #' @param order Logical, Defaults to TRUE. If it is TRUE, the output will descending order via ie.
# #'
# #' @return Information Entropy
# # #' @details
# #'
# #' @examples
# #' # Load German credit data
# #' data(germancredit)
# #'
# #' # Information Entropy
# #' dt_info_ent = ie(germancredit, y = "creditability")
# #'
# #' @import data.table
# #' @export
ie = function(dt, y, x=NULL, order=TRUE) {
  info_ent = label = NULL # no visible binding for global variable

  # set dt as data.table
  dt = setDT(dt)
  # # remove date/time col
  # dt = rmcol_datetime_unique1(dt)
  # # replace "" by NA
  # dt = rep_blank_na(dt)
  # check y
  # dt = check_y(dt, y, positive)
  # x variable names
  x = x_variable(dt, y, x)

  # data prep
  dt = dt[
    , x, with = FALSE
    ][, `:=`(
      rowid = .I, label = dt[[y]]
    )]

  # info_ent
  ielist = dt[, sapply(.SD, ie_xy, label), .SDcols = x]

  ielist = data.table(variable=names(ielist), info_ent=ielist)
  if (order) ielist = ielist[order(-info_ent)]

  return(ielist)
}

# #' @import data.table
ie_xy = function(x=NULL, y) {
  # . = p1 = p0 = count = count_distr = bin_ie = NULL
  . =x_count =xy_N =x_count_distr =p =ent = bin_ie =xN_distr =x_N =NULL

  if (is.null(x)) x = 0

  data.table(x=x, y=y)[
    , .(xy_N = .N), by=c("x","y")
    ][, x_N := sum(xy_N), keyby=c("x")
    ][, p := xy_N/x_N
    ][, .(ent = -sum(ifelse(p==0, 0, p*log2(p))), x_N=sum(xy_N)), by='x'
    ][, xN_distr := x_N/sum(x_N)
    ][, sum(ent*xN_distr)]

  # data.table(x=x, y=y)[
  #   , .(p0=sum(y==0)/.N, p1=sum(y==1)/.N, count=.N), keyby="x"
  #   ][, `:=`(
  #     bin_ie = -(p0*log2(p0) + p1*log2(p1)),
  #     count_distr = count/sum(count)
  #  )][][, bin_ie := ifelse(is.infinite(bin_ie) | is.nan(bin_ie), 0, bin_ie)
  #   ][, sum(bin_ie*count_distr)]

}


# #' Information Entropy
# #'
# #' calculating ie of total based on good and bad vectors
# #'
# #' @param good vector of good numbers
# #' @param bad vector of bad numbers
# #'
# #' @examples
# #' # ie_01(good, bad)
# #' dtm = melt(dt, id = 'creditability')[, .(
# #' good = sum(creditability=="good"), bad = sum(creditability=="bad")
# #' ), keyby = c("variable", "value")]
# #'
# #' dtm[, .(ie = lapply(.SD, ie_01, bad)), by="variable", .SDcols# ="good"]
# #'
# #' @import data.table
#' @import data.table
#'
ie_01 = function(good, bad) {
  # global variables
  . =bin_ie =count =count_distr =p0 =p1 =NULL

  data.table(
    good = good, bad = bad
  )[, .(p0=good/(good+bad), p1=bad/(good+bad), count=good+bad)
  ][, `:=`(
    bin_ie = -(p0*log2(p0) + p1*log2(p1)),
    count_distr = count/sum(count)
 )][, bin_ie := ifelse(bin_ie==Inf | bin_ie== -Inf, 0, bin_ie)
  ][, sum(bin_ie*count_distr)]

}




# gini impurity (CART) ------
# gini(D) = 1-\sum_k(p_k^2)
# gini_impurity(D) = \sum_v(abs(\frac{D^v}{D})*gini(D^v))

# #' Impurity Gini
# #'
# #' This function calculates gini impurity (used by the CART Decision Tree) for multiple x variables.
# #'
# #' @param dt A data frame with both x (predictor/feature) and y (response/label) variables.
# #' @param y Name of y variable.
# #' @param x Name of x variables. Defaults to NULL. If x is NULL, then all variables except y are counted as x variables.
# #' @param order Logical, Defaults to TRUE. If it is TRUE, the output will descending order via gini
# #'
# #' @return gini impurity
# # #' @details
# #'
# #' @examples
# #' # Load German credit data
# #' data(germancredit)
# #'
# #' # gini impurity
# #' dt_gini = ig(germancredit, y = "creditability")
# #'
# #' @import data.table
# #' @export
ig = function(dt, y, x=NULL, order=TRUE) {
  gini_impurity = label = NULL

  # set dt as data.table
  dt = setDT(dt)
  # # remove date/time col
  # dt = rmcol_datetime_unique1(dt)
  # # replace "" by NA
  # dt = rep_blank_na(dt)
  # check y
  # dt = check_y(dt, y, positive)
  # x variable names
  x = x_variable(dt, y, x)

  # data prep
  dt = dt[
    , x, with = FALSE
    ][, `:=`(
      rowid = .I, label = dt[[y]]
    )]

  # index gini
  gini_vec = dt[, sapply(.SD, ig_xy, label), .SDcols = x]

  gini_df = data.table(variable=names(gini_vec), gini_impurity=gini_vec)
  if (order) gini_df = gini_df[order(-gini_impurity)]

  return(gini_df)
}

#' @import data.table
ig_xy = function(x, y) {
  . =x_count =xy_N =x_count_distr =p =bin_ig =xN_distr =x_N =NULL

  data.table(x=x, y=y)[
    , .(xy_N = .N), by=c("x","y")
  ][, x_N := sum(xy_N), keyby=c("x")
  ][, p := xy_N/x_N
  ][, .(bin_ig = 1-sum(p^2), x_N=sum(xy_N)), by='x'
  ][, xN_distr := x_N/sum(x_N)
  ][, sum(bin_ig*xN_distr)]


  # data.table(x=x, y=y)[
  #   , .(p0=sum(y==0)/.N, p1=sum(y==1)/.N, count=.N), keyby="x"
  #   ][, `:=`(
  #     bin_ig = 1-(p0^2 + p1^2),
  #     count_distr = count/sum(count)
  #  )][, sum(bin_ig*count_distr)]

}



#' @import data.table
ig_01 = function(good, bad) {
  # global variables
  . =bin_ig =count =count_distr =p0 =p1 =NULL

  data.table(
    good = good, bad = bad
  )[, .(p0=good/(good+bad), p1=bad/(good+bad), count=good+bad)
    ][, `:=`(
      bin_ig = 1-(p0^2 + p1^2),
      count_distr = count/sum(count)
    )][, sum(bin_ig*count_distr)]

}


