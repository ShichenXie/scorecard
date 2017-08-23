## Test environments
* local OS X install, R 3.4.0
* ubuntu 12.04 (on travis-ci), R 3.4.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 2 note

* checking dependencies in R code ... NOTE
Packages in Depends field not imported from:
  ‘data.table’ ‘ggplot2’ ‘gridExtra’
  These packages need to be imported from (in the NAMESPACE file)
  for when this namespace is loaded but not attached.

* checking R code for possible problems ... NOTE
iv: no visible global function definition for ‘data.table’
iv: no visible global function definition for ‘:=’
iv: no visible binding for global variable ‘.SD’
iv: no visible global function definition for ‘melt’
iv: no visible global function definition for ‘.’
iv: no visible binding for global variable ‘.N’
iv: no visible binding for global variable ‘good’
iv: no visible binding for global variable ‘bad’
iv: no visible binding for global variable ‘DistrBad’
... 272 lines ...
  geom_bar geom_density geom_line geom_point geom_ribbon geom_segment
  geom_text ggplot good goodbad group guide_legend guides id
  is.data.table ks label labs logAE melt miv model patterns points
  precision quantile rbindlist recall rowid scale_colour_manual
  scale_fill_manual scale_x_continuous scale_y_continuous score
  sec_axis setDT setnames shift test theme theme_bw total_iv train unit
  value var var_woe variable woe
Consider adding
  importFrom("graphics", "points")
  importFrom("stats", "IQR", "dist", "quantile", "var")
to your NAMESPACE file.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

---

* I have run R CMD check on the NUMBER downstream dependencies.
  (Summary at ...). 
  
* FAILURE SUMMARY

* All revdep maintainers were notified of the release on RELEASE DATE.
