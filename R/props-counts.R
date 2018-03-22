
#' Compute all proportions or counts
#'
#' Compute vector of counts, proportions, or percents for each unique value (and `NA` if there
#' is missing data) in a vector.
#'
#' @param x A vector or a formula.
#' @param format One of `"count"`, `"proportion"`, or `"percent"`.  May be abbreviated.
#' @param data A data frame.
#' @param ... Arguments passed to methods.
#'
#' @seealso [mosaic::prop()]
#' @seealso [mosaic::count()]
#' @examples
#' if (require(mosaicData)) {
#'   props(HELPrct$substance)
#'   # numeric version tallies missing values as well
#'   props(HELPmiss$link)
#'   # Formula version omits missing data with warning (by default)
#'   props( ~ link, data = HELPmiss)                       # omit NAs with warning
#'   props( ~ link, data = HELPmiss, na.action = na.pass)  # no warning; tally NAs
#'   props( ~ link, data = HELPmiss, na.action = na.omit)  # no warning, omit NAs
#'   props( ~ substance | sex, data = HELPrct)
#'   props( ~ substance | sex, data = HELPrct, format = "percent")
#'   percs( ~ substance | sex, data = HELPrct)
#'   counts( ~ substance | sex, data = HELPrct)
#'   df_stats( ~ substance | sex, data = HELPrct, props, counts)
#'   df_stats( ~ substance | sex, data = HELPmiss, props, na.action = na.pass)
#' }
#'
#' @export
#' @rdname props

counts <- function(x, ...) {
  UseMethod("counts")
}

#' @rdname props
#' @export

counts.factor <-
  function(x, ..., format = c("count", "proportion", "percent")) {
    format = match.arg(format)
    uval <- levels(x)

    res <- sapply(uval, function(v) base::sum(x == v, na.rm = TRUE))
    names (res) <-
      paste0(
        switch(format, count = "n_", proportion = "prop_", percent = "perc_"),
        as.character(uval)
      )

    n_missing <- base::sum(is.na(x), na.rm = TRUE)
    if (n_missing > 0L) {
      names(n_missing) <-
        switch(format, count = "n_NA", proportion = "prop_NA", percent = "perc_NA")
      res <- c(res, n_missing)
    }
    # do arithmetic to convert to proportions or percents, and return result
    switch(
      format,
      count = res,
      proportion =   res / length(x),
      percent =  100 * res / length(x)
    )
  }


#' @rdname props
#' @export

counts.default <-
  function(x, ..., format = c("count", "proportion", "percent")) {
    return(counts(factor(x), ..., format = format))
    format = match.arg(format)
    uval <- sort(unique(x))

    res <- sapply(uval, function(v) base::sum(x == v, na.rm = TRUE))
    names (res) <-
      paste0(
        switch(format, count = "n_", proportion = "prop_", percent = "perc_"),
        as.character(uval)
      )

    n_missing <- base::sum(is.na(x), na.rm = TRUE)
    if (n_missing > 0L) {
      names(n_missing) <-
        switch(format, count = "n_NA", proportion = "prop_NA", percent = "perc_NA")
      res <- c(res, n_missing)
    }
    # do arithmetic to convert to proportions or percents, and return result
    switch(
      format,
      count = res,
      proportion =   res / length(x),
      percent =  100 * res / length(x)
    )
  }

#' @rdname props
#' @export
counts.formula <- function(x, data, ..., format = "count") {
  mosaicCore::df_stats(x, data = data, "counts", fargs = list(format = format), ...)
}

#' @rdname props
#' @export
props <- function(x, ..., format = "proportion") {
  counts(x, ..., format = format)
}

#' @rdname props
#' @export
percs <- function(x, ..., format = "percent") {
  counts(x, ..., format = format)
}
