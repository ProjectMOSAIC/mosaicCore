# this is a modification of aggregate that retains the name when FUN returns a single named value.
# intended for use in df_stats()

#' @importFrom stats aggregate complete.cases
NA

df_aggregate <-
  function (x, by, FUN, ..., simplify = TRUE, drop = TRUE)
{
  if (!is.data.frame(x))
    x <- as.data.frame(x)
  FUN <- match.fun(FUN)
  if (NROW(x) == 0L)
    stop("no rows to aggregate")
  if (NCOL(x) == 0L) {
    x <- data.frame(x = rep(1, NROW(x)))
    return(stats::aggregate(x, by, function(x) 0L)[seq_along(by)])
  }
  if (!is.list(by))
    stop("'by' must be a list")
  if (is.null(names(by)) && length(by))
    names(by) <- paste0("Group.", seq_along(by))
  else {
    nam <- names(by)
    ind <- which(!nzchar(nam))
    names(by)[ind] <- paste0("Group.", ind)
  }
  if (any(lengths(by) != NROW(x)))
    stop("arguments must have same length")
  y <- as.data.frame(by, stringsAsFactors = FALSE)
  keep <- stats::complete.cases(by)
  y <- y[keep, , drop = FALSE]
  x <- x[keep, , drop = FALSE]
  nrx <- NROW(x)
  ident <- function(x) {
    y <- as.factor(x)
    l <- length(levels(y))
    s <- as.character(seq_len(l))
    n <- nchar(s)
    levels(y) <- paste0(strrep("0", n[l] - n), s)
    as.character(y)
  }
  grp <- lapply(y, ident)
  multi.y <- !drop && ncol(y)
  if (multi.y) {
    lev <- lapply(grp, function(e) sort(unique(e)))
    y <- as.list(y)
    for (i in seq_along(y)) y[[i]] <- y[[i]][match(lev[[i]],
                                                   grp[[i]])]
    eGrid <- function(L) expand.grid(L, KEEP.OUT.ATTRS = FALSE,
                                     stringsAsFactors = FALSE)
    y <- eGrid(y)
  }
  grp <- if (ncol(y)) {
    names(grp) <- NULL
    do.call(paste, c(rev(grp), list(sep = ".")))
  }
  else integer(nrx)
  if (multi.y) {
    lev <- as.list(eGrid(lev))
    names(lev) <- NULL
    lev <- do.call(paste, c(rev(lev), list(sep = ".")))
    grp <- factor(grp, levels = lev)
  }
  else y <- y[match(sort(unique(grp)), grp, 0L), , drop = FALSE]
  nry <- NROW(y)
  z <- lapply(x, function(e) {
    ans <- lapply(X = split(e, grp), FUN = FUN, ...)
    if (simplify && length(len <- unique(lengths(ans))) ==
        1L) {
      if (len == 1L) {
        nm <- unique(unlist(lapply(ans, names)))
        ans <- matrix(unlist(ans, recursive = FALSE), ncol = 1L)
        if (length(nm) == 1L && !is.null(nm)) colnames(ans) <- nm
        # below is what aggregate() does
        # cl <- lapply(ans, oldClass)
        # cl1 <- cl[[1L]]
        # ans <- unlist(ans, recursive = FALSE)
        # if (!is.null(cl1) && all(vapply(cl, identical,
        #                                 NA, y = cl1)))
        #   class(ans) <- cl1
      }
      else if (len > 1L)
        ans <- matrix(unlist(ans, recursive = FALSE),
                      nrow = nry, ncol = len, byrow = TRUE, dimnames = if (!is.null(nms <- names(ans[[1L]])))
                        list(NULL, nms))
    }
    ans
  })
  len <- length(y)
  for (i in seq_along(z)) y[[len + i]] <- z[[i]]
  names(y) <- c(names(by), names(x))
  row.names(y) <- NULL
  y
}