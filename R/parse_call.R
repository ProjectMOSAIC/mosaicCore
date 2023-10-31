
# intended use case: parse_call(f_lhs(a + b + c ~ x + y))
# for use with df_stats() to compute stats based on multiple lhs variables
# in one call

# returns a list of names/calls splitting on +
# note: () block splitting

parse_call <- function(x) {
  res <- list()
  if (length(x) == 1) {
    return(list(x))
  }

  if (x[[1]] == "+") {
    return(
      c(
        parse_call(x[[2]]),
        list(x[[length(x)]])
      )
    )
  }

  return(list(x))
}

# parse_call(rlang::f_lhs(a + b ~ x + y)) |> lapply(class)
# parse_call(rlang::f_lhs(a + b + c ~ x + y)) |> lapply(class)
# parse_call(rlang::f_lhs((a + b) * c ~ x + y)) |> lapply(class)
