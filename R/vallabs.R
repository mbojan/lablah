#' List of tibbles with value labels
#'
#' @param data data frame with variables having value labels
#'
#' @return A list of tibbles (or `NULL`s of no value labels are present) named
#'   with variable names. Each tibble has columns:
#' - `label` - Value label
#' - `value` - Value
#'
#' @seealso [labelled::val_labels()]
#'
#' @export

vallabs <- function(data) {
  vl <- purrr::map(
    labelled::val_labels(data),
    ~ tibble::enframe,
    name = "label",
    value = "value"
  )
}
