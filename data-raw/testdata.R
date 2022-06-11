# Test data for
#
# 1. Variable types
#   - Numeric
#   - Character
#   - Logical
#
# 2. Value labels
#   - All values labelled
#   - Some values labelled
#   - No values labelled
#
# 3. User missing values
#   - No
#   - Some
#   - All
#
# 4. System missing values
#   - No
#   - Yes
#
# 5. Variable label
#   - No
#   - Yes

library(magrittr)

e <- rlang::quo({
  input <- c(0,1,2,1,0)
  out <- switch(
    variable_type,
    numeric = x,
    character = LETTERS[x],
    logical = as.logical(x)
  )
  u <- sort(unique(out))
  labelled::val_labels(out) <- dplyr::case_when(
    value_labels == "all" ~ structure(u, names = letters[seq(along=u)]),
    value_labels == "some" ~ structure(u[1], names = "a"),
    value_labels == "no" ~ NULL
  )
  out
})

tidyr::crossing(
  variable_type = c("numeric", "character", "logical"),
  value_labels = c("all", "some", "no"),
  user_missign = c("all", "some", "no"),
  sysmis = c(FALSE, TRUE),
  variable_label = c(FALSE, TRUE)
) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    var = list(!!e)
  ) |>
  dplyr::pull(var) %>%
  setNames(paste0("v", seq(along = .))) |>
  tibble::as_tibble()

# usethis::use_data(testdata, overwrite = TRUE)
