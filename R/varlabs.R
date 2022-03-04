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
  varl <- labelled::var_label(data)
  isnull <- sapply(varl, is.null)
  varl[isnull] <- list("")
  tibble::enframe(unlist(varl), name = "variable", value = "label")
}
