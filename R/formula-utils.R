
# allow for some operations in formulas without requiring the use of I()

#' @export
#'
reop_formula <- function(x) {
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
    if (
      x[[1]] == as.name("/") ||
      x[[1]] == as.name("*") ||
      x[[1]] == as.name("^")
    ){
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