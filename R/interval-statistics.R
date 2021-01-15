#' Interval statistics
#'
#' Calculate coverage intervals and confidence intervals for the sample mean, median, sd, proportion, ...
#' Typically, these will be used within `df_stats()`. For the mean, median, and sd, the variable x must be
#' quantitative. For proportions, the x can be anything; use the `success` argument to specify what
#' value you want the proportion of. Default for `success` is `TRUE` for x logical, or the first level returned
#' by `unique` for categorical or numerical variables.
#'
#' @rdname interval_statistics
#' @aliases interval_statistics
#'
#' @param x a variable.
#'
#' @param success for proportions, this specifies the categorical level for which the calculation of proportion will
#' be done. Defaults: `TRUE` for logicals for which the proportion is to
#' be calculated.
#'
#' @param level number in 0 to 1 specifying the confidence level for the interval. (Default: 0.95)

#' @param na.rm if `TRUE` disregard missing data
#'
#' @param method for `ci.prop()`, the method to use in calculating the confidence
#' interval. See `mosaic::binom.test()` for details.
#'
#' @note When using these functions with `df_stats()`, omit the `x` argument, which
#' will be supplied automatically by `df_stats()`. See examples.
#'
#' @details Methods: `ci.mean()` uses the standard t confidence interval.
#' `ci.median()` uses the normal approximation method.
#' `ci.sd()` uses the chi-squared method.
#' `ci.prop()` uses the binomial method. In the usual situation where the `mosaic` package is available,
#' `ci.prop()` uses `mosaic::binom.test()` internally, which provides several
#' methods for the calculation. See the documentation
#' for `binom.test()` for details about the available methods. Clopper-Pearson is
#' the default method. When used with `df_stats()`, the confidence interval
#' is calculated for each group separately. For "pooled" confidence intervals, see methods
#' such as `lm()` or `glm()`.
#'
#' @return a named numerical vector with components `lower` and `upper`, and,
#' in the case of `ci.prop()`, `center`. When used the `df_stats()`, these components
#' are formed into a data frame.
#'
#' @keywords stats
#'
#' @importFrom stats qbinom qchisq qnorm
#'
#' @seealso [mosaicCore::df_stats()], [mosaic::binom.test()], [mosaic::t.test()]
#'
#' @aliases coverage ci.mean ci.median ci.sd ci.prop
#'
#' @examples
#' # The central 95% interval
#' df_stats(hp ~ cyl, data = mtcars, c95 = coverage(0.95))
#' # The confidence interval on the mean
#' df_stats(hp ~ cyl, data = mtcars, mean, ci.mean)
#' # What fraction of cars have 6 cylinders?
#' df_stats(mtcars, ~ cyl, six_cyl_prop = ci.prop(success = 6, level = 0.90))
#' # Use without `df_stats()` (rare)
#' ci.mean(mtcars$hp)
#'
#' @export
coverage <- function(x, level = 0.95, na.rm = TRUE) {
  x <- as.numeric(x)
  level <- check.level(level)
  bottom = (1 - level) / 2
  top = 1 - bottom
  res <- quantile(x, probs = c(bottom, top),
                  na.rm = na.rm, type = 7, names = TRUE)
  names(res) <- paste("coverage", c("lower", "upper"), sep = "_")
  names(res) <- c("lower", "upper") # Do we prefer a short form?

  res
}

#' @rdname interval_statistics
#' @export
ci.mean <- function(x, level = 0.95, na.rm = TRUE) {
  x <- as.numeric(x)
  level <- check.level(level)

  n <- length(x)
  lowerq <- stats::qt((1 - level) / 2, df = n - 1)
  res <-
    base::mean(x, na.rm = na.rm) +
    c(lowerq, -lowerq) *
    stats::sd(x, na.rm = na.rm)/sqrt(length(x))

  # names(res) <- paste("ci_mean", c("lower", "upper"), sep = "_")
  names(res) <- c("lower", "upper") # Do we prefer a short form?

  res
}
#' @rdname interval_statistics
#' @export
ci.median <- function(x, level = 0.9, na.rm = TRUE) {
  x <- as.numeric(x)
  level <- check.level(level)
  x <- sort(x)
  n <- length(x)
  bottom <- (1 - level) / 2
  top <- 1 - bottom
  ci_index <- (n + qnorm(c(bottom, top)) * sqrt(n))/2 + c(0,1)
  res <- as.numeric(x[round(ci_index)])

  names(res) <- c("lower", "upper")
  res
}


#' @rdname interval_statistics
#' @export
ci.sd <- function(x, level = 0.95, na.rm = TRUE) {
  x <- as.numeric(x)
  level <- check.level(level)
  n <- length(x)
  bottom <- (1 - level) / 2
  top <- 1 - bottom
  res = (n - 1) *
    stats::sd(x, na.rm = TRUE)^2 /
    qchisq(c(top, bottom), df = n - 1)
  res <- sqrt(res)

  names(res) <- c("lower", "upper")
  res
}

#' @rdname interval_statistics
#' @export
ci.prop <- function(x,  success = NULL, level = 0.95,
                    method = c("Clopper-Pearson",
                               "binom.test", "Score", "Wilson",
                               "prop.test", "Wald", "Agresti-Coull",
                               "Plus4")) {
  level <- check.level(level)
  if (requireNamespace("mosaic")) {
    # leave checking `ci.method` to binom.test()
    tmp <- mosaic::binom.test(~ x, conf.level = level, ci.method = method,
                              success = success)
    res <- numeric(3)
    res[c(1,3)] <- tmp$conf.int
    res[2] <- tmp$estimate
    names(res) <- c("lower", "center", "upper")
  } else {
    nm = success
    if (is.null(nm)) {
      nm <- if (is.logical(x)) TRUE else unique(x)[1]
    }

    prob <- mean(x == nm[1])
    n <- length(x)
    bottom <- (1 - level) / 2
    top <- 1 - bottom
    res <- qbinom(c(bottom, top), size = length(x), prob = prob) / n
    res[3] <- res[2]
    res[2] <- prob
    names(res) <- c("lower", "center", "upper")
  }

  res
}

# internal use
check.level <- function(level) {
  res <- ifelse(level > 1, 1, ifelse(level < 0, 0, level))
  if (level < 0 | level > 1) warning("level ", level, " is not between 0 and 1. Setting to ", res)

  res
}