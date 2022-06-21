#' Heuristically guess or verify type of a variable
#'
#' @name seems
#'
#' @description Given a vector `x` guess what kind of "type" it is, where "type"
#'   correspond to some common classes influencing useful ways to
#'   analyze/visualize. These functions are used internally to determine default
#'   behavior of some other functions.
#'
#' @param x a vector for which [is.atomic()] is `TRUE`
#'
#' @return All functions return `TRUE` or `FALSE` (a logical scalar).







#' @rdname seems
#' @description - [seems_integer()] -- Returns `TRUE` if `x` is [typeof()]
#'   integer or is a numeric with all values being in fact integers (i.e. equal
#'   to `round(x)`).
#'
#' @export
#' @examples
#' # seems_integer() ----------------------------------------------------------
#' seems_integer(1:5)       # TRUE
#' seems_integer(runif(5))  # FALSE
#'
seems_integer <- function(x) {
  seems_check_var(x)
  is.integer(x) || (is.numeric(x) && all(x == as.integer(round(x))))
}




#' @rdname seems
#' @description - [seems_continuous()] -- Returns `TRUE` if `x` is numeric and,
#'   if an integer, has more than 10 distinct values.
#'
#' @export
#' @examples
#' # seems_continuous() -------------------------------------------------------
#' seems_continuous(1:5)       # FALSE
#' seems_continuous(1:11)      # TRUE
#' seems_continuous(runif(5))  # TRUE
#'
#' # Summarize variables of `mtcars` in a type-dependent way:
#' iscont <- vapply(mtcars, seems_continuous, logical(1))
#' layout(matrix(1:12, 3, 4))
#' for(n in names(mtcars)) {
#'   if(iscont[n]) {
#'     hist(mtcars[[n]], main=n, xlab="")
#'   } else {
#'     barplot(table(mtcars[[n]]), main=n)
#'   }
#' }
#' layout(1)
seems_continuous <- function(x) {
  seems_check_var(x)
  tof <- typeof(x)
  if(seems_integer(x)) tof <- "integer"
  switch(
    tof,
    integer = nvalues(x) >= 10,
    double = TRUE,
    FALSE
  )
}

#' @rdname seems
#' @description - [seems_discrete()] -- Returns `TRUE` if `x` does not seem to
#'   be continuous.
#'
#' @export
seems_discrete <- function(x) {
  seems_check_var(x)
  !seems_continuous(x)
}


#' @rdname seems
#' @description - [seems_categorical()] -
#'
#' @export
seems_categorical <- function(x) {
  seems_check_var(x)
  .NotYetImplemented()
}




# Utilities ---------------------------------------------------------------

# Placeholder for common input assertions
seems_check_var <- function(x) {
  stopifnot(is.atomic(x))
}

# Number of distinct values of `x`
nvalues <- function(x) {
  seems_check_var(x)
  length(unique(x))
}
