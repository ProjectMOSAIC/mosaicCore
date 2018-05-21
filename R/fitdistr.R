
#' @importFrom stats IQR dexp dgeom dlnorm dnorm dpois median sd var
NA

#' Fit a distribution to data and return a function
#'
#' Given the name of a family of 1-dimensional distributions, this function chooses a
#' particular member  of the family that fits the data and returns a function in the
#' selected p, d, q, or r format. When analytical solutions do not exist, `MASS::fitdistr()`
#' is used to estimate the parameters by numerical maximum likelihood.
#'
#' @param data A data frame.
#' @param formula A formula.  A distribution will be fit to the data defined by the
#' right side and evaluated in `data`.
#' @param dist A distribution function or the equivalent character string
#' for the family desired,  e.g., `pnorm`, `rgamma`. (see MASS:fitdistr for the
#' distributions that are available)
#' what format the generated function should take.
#' @param start Starting values for the numerical maximum likelihood method
#' (passed to `MASS::fitdistr`).
#' @param ... Additional arguments to MASS::fitdistr()
#'
#' @return A function of one variable that acts like, say,
#' `pnorm()`, `dnorm()`, `qnorm()`, or `rnorm()`, but with the default
#' values of the parameters set to their maximum likelihood estimates.
#' @export
#' @importFrom MASS fitdistr
#' @importFrom rlang enquo eval_tidy expr_text

fit_distr_fun <- function(data, formula, dist, start = NULL, ... ) {

  dist_name <- deparse(substitute(dist))
  if (is.character(dist)) dist_name <- dist

  # distr_fun_name <- rlang::expr_text(enquo(distr_fun)[[2]])

  x <- rlang::eval_tidy(formula[[2]], data = data)

  ddist_name <- sub("^[pdqr]", "d", dist_name)
  params <- analytic_fit_distr(x, ddist_name)

  # print(c(dist_name = dist_name))
  # print(c(ddist_name = ddist_name))
  if (is.null(params)) {
    family <-
      switch(
        ddist_name,
        dbeta = "beta",
        dcauchy = "cauchy",
        dchisq = "chi-squared",
        dexp = "exponential",
        df = "f",
        dgamma = "gamma",
        dgeom = "geometric",
        dlnorm = "lognormal",
        dlogis = "dlogistic",
        dnbinom = "negative binomial",
        dnorm = "normal",
        dpois = "poisson",
        dt = "t",
        dweibull = "weibull"
      )
    # print(c(family = family))
    if (is.null(start)) start <- get_start_params(x, ddist_name)
    params <- MASS::fitdistr(x, family, start, ...)
  }

  # get function and modify default parameter values based on fitted estimates.
  result <- get(dist_name, mode = "function", envir = parent.frame())
  param_args <- as.list(params$estimate)
  args <- formals(dist_name)
  args[names(param_args)] <- param_args
  formals(result) <- args
  result
}

analytic_fit_distr <- function(x, distname) {
  n <- length(x)
  if (distname %in% c("dlnorm")) {
    if (any(x <= 0))
      stop("need positive values to fit a log-Normal")
    lx <- log(x)
    sd0 <- sqrt((n - 1)/n) * stats::sd(lx)
    mx <- mean(lx)
    estimate <- c(mx, sd0)
    sds <- c(sd0/sqrt(n), sd0/sqrt(2 * n))
    names(estimate) <- names(sds) <- c("meanlog", "sdlog")
    vc <- matrix(c(sds[1]^2, 0, 0, sds[2]^2), ncol = 2,
                 dimnames = list(names(sds), names(sds)))
    names(estimate) <- names(sds) <- c("meanlog", "sdlog")
    return(
      structure(
        list(estimate = estimate, sd = sds, vcov = vc, n = n,
             loglik = sum(stats::dlnorm(x, mx, sd0, log = TRUE))),
        class = "fitdistr"))
  }
  if (distname == "dnorm") {
    sd0 <- sqrt((n - 1)/n) * sd(x)
    mx <- mean(x)
    estimate <- c(mx, sd0)
    sds <- c(sd0/sqrt(n), sd0/sqrt(2 * n))
    names(estimate) <- names(sds) <- c("mean", "sd")
    vc <- matrix(c(sds[1]^2, 0, 0, sds[2]^2), ncol = 2,
                 dimnames = list(names(sds), names(sds)))
    return(
      structure(
        list(estimate = estimate, sd = sds, vcov = vc, n = n,
             loglik = sum(stats::dnorm(x, mx, sd0, log = TRUE))),
        class = "fitdistr"))
  }
  if (distname == "poisson") {
    estimate <- mean(x)
    sds <- sqrt(estimate/n)
    names(estimate) <- names(sds) <- "lambda"
    vc <- matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("lambda",
                                                            "lambda"))
    return(
      structure(
        list(estimate = estimate, sd = sds, vcov = vc, n = n,
             loglik = sum(stats::dpois(x, estimate, log = TRUE))),
        class = "fitdistr"))
  }
  if (distname == "dexp") {
    if (any(x < 0))
      stop("Exponential values must be >= 0")
    estimate <- 1/mean(x)
    sds <- estimate/sqrt(n)
    vc <- matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("rate", "rate"))
    names(estimate) <- names(sds) <- "rate"
    return(
      structure(
        list(estimate = estimate, sd = sds, vcov = vc, n = n,
             loglik = sum(stats::dexp(x, estimate, log = TRUE))),
        class = "fitdistr"))
  }
  if (distname == "dgeom") {
    estimate <- 1/(1 + mean(x))
    sds <- estimate * sqrt((1 - estimate)/n)
    vc <- matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("prob", "prob"))
    names(estimate) <- names(sds) <- "prob"
    return(
      structure(
        list(estimate = estimate, sd = sds, vcov = vc, n = n,
             loglik = sum(stats::dgeom(x, estimate, log = TRUE))),
        class = "fitdistr"))
  }

  NULL # signal that there was no analytic calculation
}

get_start_params <- function(x, distname, ...) {
  dots <- list(...)
  start <- list()
  if (distname == "dweibull") {
    if (any(x <= 0))
      stop("Weibull values must be > 0")
    lx <- log(x)
    m <- mean(lx)
    v <- stats::var(lx)
    shape <- 1.2/sqrt(v)
    scale <- exp(m + 0.572/shape)
    start <- list(shape = shape, scale = scale)
    start <- start[!is.element(names(start), dots)]
  }
  if (distname == "dgamma") {
    if (any(x < 0))
      stop("gamma values must be >= 0")
    m <- mean(x)
    v <- stats::var(x)
    start <- list(shape = m^2/v, rate = m/v)
    start <- start[!is.element(names(start), dots)]
    control <- list(parscale = c(1, start$rate))
  }
  if (distname == "dnbinom") {
    m <- mean(x)
    v <- stats::var(x)
    size <- if (v > m)
      m^2/(v - m)
    else 100
    start <- list(size = size, mu = m)
    start <- start[!is.element(names(start), dots)]
  }
  if (distname %in% c("dcauchy", "dlogis")) {
    start <- list(location = stats::median(x), scale = stats::IQR(x)/2)
    start <- start[!is.element(names(start), dots)]
  }
  if (distname == "t") {
    start <- list(m = stats::median(x), s = stats::IQR(x)/2, df = 10)
    start <- start[!is.element(names(start), dots)]
  }

  start
}

