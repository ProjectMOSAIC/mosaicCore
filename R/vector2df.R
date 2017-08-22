#' @importFrom stats setNames
NA

#' Convert a vector to a data frame
#'
#' Convert a vector into a 1-raw data frame using the names of the vector as
#' column names for the data frame
#'
#' @param x a vector
#' @param nice_names a logical indicating whether names should be nicified
#' @return a data frame
#' @export
#' @examples
#' vector2df(c(1, b = 2, `(Intercept)` = 3))
#' vector2df(c(1, b = 2, `(Intercept)` = 3), nice_names = TRUE)
#'
vector2df <- function(x, nice_names = FALSE) {
  if (!is.vector(x)) {
    stop("x is not a vector")
    return(x)
  }
  nn <- names(x)
  if (is.null(nn)) { nn <- list() }
  result <- data.frame(t(matrix(x, dimnames = list(nn, list()))), check.names = FALSE)
  if (nice_names) {
    names(result) <- nice_names(names(result))
  }
  result
}

#' Convert to a data frame
#'
#' A generic and several methods for converting objects into data frames.
#'
#' These methods are primarily for internal use inside \code{\link{df_stats}()},
#' but are exported in case they have other uses.  The conversion works as follows.
#' Data frames are left as is.  Matrices are converted with
#' \code{\link{as.data.frame}()}}, but the names are set to the column
#' names of \code{object}, even if that results in \code{NULL}.
#' A numeric vector is converted into a data frame with 1 column.
#' If \code{object} is a list,
#'
#' @param object An object to be converted into a data frame.
#' @param ... Additional arguments used by methods.
#' @rdname make_df
#'
#' @export
make_df <- function(object, ...) {
  UseMethod("make_df")
}

#' @rdname make_df
#' @export
make_df.list <- function(object, ...) {
  object %>% lapply(vector2df, ...) %>% bind_rows()
}

#' @rdname make_df
#' @export
make_df.matrix <- function(object, ...) {
  lapply(1:ncol(object), function(c) unlist(object[, c, drop = FALSE])) %>%
    as.data.frame() %>%
    setNames(colnames(object))
}

#' @rdname make_df
#' @export
make_df.numeric <- function(object, ...) {
  object %>% as.data.frame()
}

#' @rdname make_df
#' @export
make_df.default <- function(object, ...) {
  as.data.frame(object, ...)
}
