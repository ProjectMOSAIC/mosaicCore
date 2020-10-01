#' Campare two numeric vectors
#'
#' This wrapper around `sign()` provides a more intuitive labeling.
#'
#' @param x,y numeric verctors to be compared item by item
#' @param verbose a logical indicating whether verbose labeling is desired
#' @return a factor with three levels (`<`, `=`, and `>` if `verbose` is `FALSE`)
#' @export
#' @examples
#' tally( ~ compare(mcs, pcs), data = mosaicData::HELPrct)
#' tally( ~ compare(mcs, pcs, verbose = TRUE), data = mosaicData::HELPrct)
#' tally( ~ compare(sexrisk, drugrisk), data = mosaicData::HELPrct)
compare <- function(x, y, verbose = FALSE) {
  sx <- substitute(x)
  sy <- substitute(y)
  res <- c('<', '=', '>')[2 + sign(x - y)]
  if (verbose) {
    res <- paste(deparse(sx), res, deparse(sy))
  }
  res
}

