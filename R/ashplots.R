
#' Compute knot points of an average shifted histogram
#'
#' Mainly a utility for the \pkg{lattice} and \pkg{ggplot2} plotting
#' functions, `ash_points()` returns the points to be plotted.
#'
#' @param x A numeric vector
#' @param binwidth The width of the histogram bins.  If `NULL` (the default) the
#'   binwidth will be chosen so that approximately 10 bins cover the data.  `adjust`
#'   can be used to to increase or decrease `binwidth`.
#' @param adjust A number used to scale `binwidth`.
#' @return A data frame containing x and y coordinates of the resulting ASH plot.
#' @rdname ashplot
#' @export
ash_points <- function(x, binwidth = NULL, adjust = 1.0) {
  if (is.null(adjust)) adjust <- 1.0
  if (is.null(binwidth)) binwidth <- diff(range(x)) / (10.0)
  left <- x - binwidth
  right <- x + binwidth
  knots <- sort(unique(c(left, x, right)))
  y <- sapply(knots,
              function(k) sum( 1 / binwidth / length(x) * (1 - abs(x-k) / binwidth) * (abs(x-k) <= binwidth))
  )
  data.frame(x = knots, y = y)
}
