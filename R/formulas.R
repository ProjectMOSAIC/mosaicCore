#' Convert formulas into standard shapes
#'
#' These functions convert formulas into standard shapes, including by
#' incorporating a groups argument.
#' @rdname mosaicformula
#' @param formula a formula
#' @param groups a name used for grouping
#' @param max.slots an integer specifying the maximum number of slots for the resulting formula.
#' An error results from trying to create a formula that is too complex.
#' @param envir the environment in which the resulting formula may be evaluated.
#' May also be `NULL`, a list, a data frame, or a pairlist.
#' @param groups.first a logical indicating whether groups should be inserted
#' ahead of the condition (else after).
#'
#' @details
#' `mosaic_formula_q` uses nonstandard evaluation of `groups` that may be
#' necessary for use within other functions.  `mosaic_formula` is a wrapper
#' around `mosaic_formula_q` and quotes `groups` before passing it along.
#' @examples
#' mosaic_formula( ~ x | z )
#' mosaic_formula( ~ x, groups=g )
#' mosaic_formula( y ~ x, groups=g )
#' # this is probably not what you want for interactive use.
#' mosaic_formula_q( y ~ x, groups=g )
#' # but it is for programming
#' foo <- function(x, groups=NULL) {
#'     mosaic_formula_q(x, groups=groups, envir=parent.frame())
#' }
#' foo( y ~ x , groups = g)
#' @export

mosaic_formula <- function(
  formula,
  groups=NULL,
  envir=parent.frame(),
  max.slots=3,
  groups.first = FALSE
)
{
  mosaic_formula_q(
    formula=formula,
    groups=rlang::enexpr(groups),
    envir=envir,
    max.slots=max.slots,
    groups.first = groups.first)
}

#' Convert lazy objects into formulas
#'
#' Convert lazy objects into a formula
#'
#' @param lazy_formula an object of class `lazy`
#' @param envir an environment that will be come the environment of the returned formula
#' @return a formula
#' @details The expression of the lazy object is evaluated in its environment.  If the
#' result is not a formula, then the formula is created with an empty left hand side
#' and the expression on the right hand side.
#'
#' @examples
#'
#' formularise(rlang::quo(foo))
#' formularise(rlang::quo(y ~ x))
#' bar <- a ~ b
#' formularise(rlang::quo(bar))
#' @export

formularise <- function(lazy_formula, envir = parent.frame()) {
  safe_formula <-
    tryCatch(eval(lazy_formula$expr, lazy_formula$env),
             error = function(e) e)

  new_formula <- ~ placeholder
  new_formula[[2]] <- lazy_formula$expr
  environment(new_formula) <- envir  # lazy_formula$env

  if (inherits(safe_formula, "formula"))
    safe_formula else new_formula
}



#' @rdname mosaicformula
#' @param ... additional arguments (currently ignored)
#' @importFrom rlang enexpr !!
#' @export
mosaic_formula_q <- function( formula,
                              groups = NULL,
                              # envir = parent.frame(),
                              max.slots = 3,
                              groups.first = FALSE,
                              ...
) {
  slots <- alist()
  if (groups.first) {
    slots <- c(slots,
               lhs(formula), rhs(formula),
               rhs_or_expr(!!rlang::enexpr(groups)), condition(formula))
  } else {
    slots <- c(slots,
               lhs(formula), rhs(formula),
               condition(formula), rhs_or_expr(!!enexpr(groups)))
  }

  if (length(slots) > max.slots) {
    stop("Invalid formula specification.  Too many slots (",
         length(slots), ">", max.slots, ").")
    return(NULL)
  }

  if (length(slots) == 1) {
    res <- ~ x
    res[[2]] <- slots[[1]]
  } else if (length(slots)==2) {
    res <- y ~ x
    res[[2]] <- slots[[1]]
    res[[3]] <- slots[[2]]
  } else if (length(slots)==3) {
    res <- y ~ x | z
    res[[2]] <- slots[[1]]
    res[[3]][[2]] <- slots[[2]]
    res[[3]][[3]] <- slots[[3]]
  } else {
    res <- formula
  }
  environment(res) <- environment(formula)
  res
}

