#' Extract value labels and return as tibble(s)
#'
#' @param data data frame with variables having value labels
#'
#' @seealso [labelled::val_labels()]
#'
#' @export

vallabs <- function(object, ...) UseMethod("vallabs")



#' @describeIn vallabs Extract value labels and return a tibble.
#'
#' @return The default method returns a single tibble with columns:
#' - `label` - Value label
#' - `value` - Value
#'
#' @export
vallabs.default <- function(object, ...) {
  labelled::val_labels(object) |>
    tibble::enframe(name = "label", value = "value")
}



#' @describeIn vallabs Extract value labels from all the columns and return a list of tibbles.
#'
#' @return If `object` is a data frame `vallabs()` returns a list of tibbles (or
#'   `NULL`s of no value labels are present) named with variable names. Each
#'   tibble has columns as described in the default method.
#'
#' @export
vallabs.data.frame <- function(object, ...) {
  purrr::map(
    labelled::val_labels(object),
    tibble::enframe,
    name = "label",
    value = "value"
  )
}
