#' Infer a Back-Transformation
#'
#' For a handful of transformations on y, infer the reverse transformation.  If the
#' transformation is not recognized, return the identity function.  This is primarily
#' intended to be used for setting a default value in other functions.
#'
#' @param formula A formula as used by, for example, \code{\link{lm}}.
#' @param warn A logical.
#' @return A function.
#'
#' @export
infer_transformation <- function(formula, warn = TRUE) {
  transformation <- identity
  left <- lhs(formula)
  if (length(left) == 2) {       # foo ( stuff )
    if (is.name(left[[2]])) {    # stuff is a name
      transformation <-
        switch(
          as.character(left[[1]]),
          "log" = exp,
          "log10" = function(x) {10^x},
          "log2" = function(x) {2^x},
          "sqrt" = function(x) x^2,
          identity
        )
    }   # could have identity if stuff is not a name or foo is not a known function
    if (warn && identical(transformation, identity)) {
      warning("You may need to specify transformation to get the desired results.", call. = FALSE)
    }
  }
  if (warn && length(left) > 2) {
    warning("You may need to specify transformation to get the desired results.", call. = FALSE)
  }
  transformation
}
