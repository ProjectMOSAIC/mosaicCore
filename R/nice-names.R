#' Nice names
#'
#' Convert a character vector into a similar character vector that would
#' work better as names in a data frame by avoiding certain awkward characters
#'
#' @rdname nicenames
#' @param x a character vector
#' @param unique a logical indicating whether returned values should be uniquified.
#' @return a character vector
#' @examples
#' nice_names( c("bad name", "name (crazy)", "a:b", "two-way") )
#' @export

nice_names <- function(x, unique=TRUE) {
  x <- gsub('\\|>', '.result.', x)
  x <- gsub('%>%', '.result.', x)
  x <- gsub('\\(Intercept\\)','Intercept', x)
  x <- gsub('resample\\(([^\\)]*)\\)','\\1', x)
  x <- gsub('sample\\(([^\\)]*)\\)','\\1', x)
  x <- gsub('shuffle\\(([^\\)]*)\\)','\\1', x)
  x <- gsub('sample\\(','', x)
  x <- gsub('shuffle\\(','', x)
  x <- gsub('resample\\(','', x)
  #	x <- gsub('\\(','.', x)
  #	x <- gsub('-','.', x)
  #	x <- gsub(':','.', x)
  #	x <- gsub('\\)','', x)
  #	x <- gsub(' ','.', x)
  #	x <- gsub('^([0-9])','X\\1', x)
  return(make.names(x, unique = unique))
}

