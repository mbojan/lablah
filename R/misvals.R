#' Lists and tibbles with user missing values
#'
#' @param data Labelled tibble with missing values/ranges defined
#'
#' @return Function [misvals()] returns a list of vectors of missing values (or
#'   `NULL`s if none are defined). List elements are named with variable names.
#'
#' @export

misvals <- function(data) {
  lapply(data, labelled::na_values)
}

#' @rdname misvals
#'
#' @return Function [misranges()] returns a list of two-element vectors with
#'   ranges of missing values.
#'
#' @export
misranges <- function(data) {
  lapply(data, labelled::na_range)
}


#' @rdname misvals
#'
#' @return Function [misranges_df()] returns a tibble with columns
#' - `variable` - Variable name
#' - `from`, `to` - Bounds (inclusive) of the missing value range
#'
#' @export

misranges_df <- function(data) {
  l <- misranges(data)
  isnull <- vapply(l, is.null, logical(1))
  dplyr::bind_rows(
    lapply(l[!isnull], setNames, c("from", "to")),
    .id="variable"
  )
}

