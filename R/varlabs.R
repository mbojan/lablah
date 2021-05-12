#' Tibble of variable labels
#'
#' @param data data frame with labelled variables
#'
#' @return A tibble with columns
#' - `variable` - Variable name
#' - `label` - Variable label
#'
#' @seealso [labelled::var_label()]
#'
#' @export

varlabs <- function(data) {
  varlabs <- labelled::var_label(data)
  tibble::enframe(unlist(varlabs), name = "variable", value = "label")
}
