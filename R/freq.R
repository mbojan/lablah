#' Frequency tibble
#'
#' @param data data frame
#' @param var Variable name or index (negative counting from the right, positive
#'   counting from the left), see [dplyr::pull()]
#' @param name Column which values will be used as names. Specified as `var`. C.f. [dplyr::pull()]
#' @param ... other arguments
#'
#' @export

freq_df <- function(data, var = -1, ...) {
  varname <- tidyselect::vars_pull(names(data), !!rlang::enquo(var))
  x <- dplyr::pull(data, !!varname)
  vl <- lablah::vallabs(x)
  mv <- labelled::na_values(x)

  dplyr::count(data, value = !!as.name(varname)) |>
    dplyr::full_join(vl, by = "value") |>
    dplyr::mutate(
      value = value |>
        labelled::remove_labels() |>
        labelled::remove_user_na(),
      n = tidyr::replace_na(n, 0),
      user_missing = value %in% mv,
      system_missing = is.na(value),
      missing = user_missing | system_missing,
      pct = n / sum(n) * 100,
      pct_valid = {
        n <- n * !user_missing
        replace(n / sum(n) * 100, user_missing, NA)
      }
    ) |>
    arrange(missing, value) |>
    mutate(
      pct_cumulative = cumsum(pct_valid)
    )
}
