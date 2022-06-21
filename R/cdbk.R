#' Codebook
#'
#' Generate a codebook based on data documentation (variable names, variable
#' labels, and value labels).
#'
#' @param data labelled tibble
#'
#' @return Function [cdbk_df()] returns a tibble with columns:
#' - `variable` - variable name
#' - `label` - variable label
#' - `value_labels` - list of tibbles with value labels
#'
#' @export


cdbk_df <- function(data) {
  dplyr::mutate(varlabs(data), value_labels = vallabs(data))
}

#' @rdname cdbk_df
#'
#' @param ... other arguments passed to [DT::datatable()]
#'
#' @return Function [cdbk_dt()] uses [DT::datatable()] to generate a browsable
#'   and searchable codebook based on the documentation.
#'
#' @importFrom rlang .data
#' @export
cdbk_dt <- function(data, ...) {
  r <- dplyr::mutate(
    cdbk_df(data),
    value_labels = purrr::map_chr(
      .data$value_labels,
      ~ with(.x, paste(glue::glue("{value}: {label}"), collapse = "<br/>"))
      )
  )
  DT::datatable(r, escape = FALSE, ...)
}
