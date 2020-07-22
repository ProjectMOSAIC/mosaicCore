
#' Counting missing/non-missing elements
#'
#' Counting missing/non-missing elements
#'
#' @param type one of `"any"` (default) or `"all"`.
#' @param ... vectors of equal length to be checked in parallel for missing values.
#' @return a vector of counts of missing or non-missing values.
#'
#' @export
#' @examples
#' if (require(NHANES) && require(mosaic) && require(dplyr)) {
#'   mosaic::tally( ~ is.na(Height) + is.na(Weight), data = NHANES, margins = TRUE)
#'   NHANES %>%
#'     summarise(
#'       mean.wt = mean(Weight, na.rm = TRUE),
#'       missing.Wt = n_missing(Weight),
#'       missing.WtAndHt = n_missing(Weight, Height, type = "all"),
#'       missing.WtOrHt = n_missing(Weight, Height, type = "any")
#'       )
#'     }

n_missing <- function( ..., type = c("any", "all")) {
  type <- match.arg(type)
  M <- cbind(...)
  base::sum(
    apply(M, 1, function(x) do.call(type, list(is.na(x)))),
    na.rm = TRUE)
}

#' @rdname n_missing
#' @export
n_not_missing <- function( ..., type = c("any", "all")) {
  type <- match.arg(type)
  M <- cbind(...)
  base::sum(
    1 - apply(M, 1, function(x) do.call(type, list(is.na(x)))),
    na.rm = TRUE)
}

#' @rdname n_missing
#' @export
n_total <- function( ..., type = c("any", "all")) {
  M <- cbind(...)
  nrow(M)
}