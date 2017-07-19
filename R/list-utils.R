#' List extraction
#'
#' These functions create subsets of lists based on their names
#'
#'
#' @param l A list.
#' @param n A vector of character strings (potential names).
#' @return A sublist of \code{l} determined by \code{names(l)}.

#' @export
#' @rdname list-utils
named <-function(l)  if (is.null(names(l))) list() else l [ names(l) != "" ]

#' @export
#' @rdname list-utils
unnamed <-function(l)  if (is.null(names(l))) l else l [ names(l) == "" ]

#' @export
#' @rdname list-utils
named_among <- function(l, n)  l [ intersect( names(l), n ) ]
