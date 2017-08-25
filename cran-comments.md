## Test environments

* local OS X install, R version 3.4.1 Patched (2017-07-09 r72910)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

This package was released for the first time earlier this summer.  The main reverse
dependencies are mosaic and ggformula.  The development versions of these 
packages check cleanly locally. I'll be posting new versions of those pacakges as 
soon as this one clears CRAN.

Some functions are moving between packages for (a) better modularity and (b)
reduction in size of the mosaic package.
