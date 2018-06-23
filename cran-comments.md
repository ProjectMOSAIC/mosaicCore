## Test environments

* local

  * R version 3.5.0 (2018-04-23)
  * Platform: x86_64-apple-darwin15.6.0 (64-bit)
  * Running under: macOS High Sierra 10.13.5

* win-builder

## R CMD check results

0 errors | 0 warnings | 0 notes

This is a minor update, but part of a plan to submit several packages in the mosaic suite. 

## Reverse Dependencies

Checked with `devtools::revdep_check()`.  There is an error in `ggformula`, but
that should go away when `ggformula` is submitted.  (`ggformula` builds and checks
cleanly on my system.)

