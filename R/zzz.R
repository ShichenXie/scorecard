scorecard_default_options <- list(
  scorecard.bin_close_right = FALSE,
  scorecard.ignore_const_cols = TRUE,
  scorecard.ignore_datetime_cols = TRUE,
  scorecard.check_cate_num = TRUE,
  scorecard.replace_blank_inf = TRUE
)

.onLoad <- function(libname, pkgname) {

  op <- options()
  toset <- !(names(scorecard_default_options) %in% names(op))
  if(any(toset)) options(scorecard_default_options[toset])

  invisible()
}
