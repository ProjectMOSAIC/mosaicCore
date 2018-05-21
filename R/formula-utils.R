
# allow for some operations in formulas without requiring the use of I()

#' Insert Inhibition of Interpretation/Conversion into formulas
#'
#' `model.frame()` assumes that certain operations (e.g. `/`, `*`, `^`) have special
#' meanings.  These can be inhibited using `I()`.  This function inserts `I()` into
#' a formula when encountering a specified operator or parens.
#'
#' @param x a formula (or a call of length 2 or 3, for recursive processing of formulas).
#'   Other objects are returned unchanged.
#' @param ops a vector of character representions of operators to be inhibited.
#' @return a formula with `I()` inserted where required to inhibit interpretation/conversion.
#' @examples
#' reop_formula(y ~ x * y)
#' reop_formula(y ~ (x * y))
#' reop_formula(y ~ x ^ y)
#' reop_formula(y ~ x * y ^ z)
#'
#' @export
reop_formula <- function(x, ops = c("/", "*", "^")) {
  if (length(x) == 2) {
    if (x[[1]] == as.name("I")){
      return(x)
    }
    if (x[[1]] == as.name("(")){
      x[[1]] <- as.name("I")
      return(x)
    }
    x[[2]] <- reop_formula(x[[2]])
  } else if (length(x) == 3) {
    if (as.character(x[[1]]) %in% ops) {
      x[[2]] <- x
      x[[3]] <- NULL
      x[[1]] <- as.name("I")
      return(x)
    }
    x[[2]] <- reop_formula(x[[2]])
    x[[3]] <- reop_formula(x[[3]])
  }
  return(x)
}