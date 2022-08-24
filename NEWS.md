# mosaicCore 

# mosaicCore 0.9.1 

* added support for one-sided formulas in `makeFun()`. 
* cleaned up a bug in `df_stats(fromat = "long")`

# mosaicCore 0.9

* new function: `compare()`, a pretty version of `sign()`
* internal adjustments to `count()` and `tally()` allow `mosaicCore` and `dplyr` to co-exist after changes in `dplyr` 1.0.3.
* more careful use of suggested packages in tests, etc.

# mosaicCore 0.8

* `df_stats()` has been updated to handle multiple response variables on the LHS.
* some internal changes to improve use of rlang
* minor bug fixes
* some updates to documenation and examples

# mosaicCore 0.6

* API for for `fit_distr_fun()` relaxed a small amount.
* `ci.mean()`, `ci.median()`, etc. now coerce inputs to numeric rather than
throwing an error when they are not.
  
# mosaicCore 0.5

* **breaking change:** Naming conventions for `prop()` and `df_stats()` have
been adjusted somewhat.  While this has the potential to break old code that
relied on the names, the new system is clearer, supports a wider range of
applications, and fixes a bug.

* A `counts()` method for factors has been added.  This method includes counts
of 0 for levels that do not occur.

* Formulas in `df_stats()` are preprosessed so that `I()` is now seldom (if ever) 
needed when doing on-the-fly calculations.

* Added `n_not_missing()` and `n_total()` similar to `n_missing()`.

* Added `ci.mean()`, `ci.median()`, `ci.prop()`, and `ci.sd()` for computing confidence intervals.

# mosaicCore 0.4.2

* Add `fit_distr_fun()` which fits distribtuions using `MASS::fitdistr()` but
returns a function (density, cdf, quantile, or random sampling) instead of just
the parameters of the fit.

# mosaicCore 0.4.0

* Additional functions from `mosaic` have been moved here.

* Improvements to `df_stats()` to handle a wider range of stat functions.

# mosaicCore 0.2.0

* Some functions from `mosaic` and `ggformula` have been moved here to support a better dependency structure among the Project MOSAIC packages.  This package is unlikely to be needed in isolation, but functions from this package will be imported into the other packages as needed.  Some will also be re-exported.



