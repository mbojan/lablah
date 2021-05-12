d <- haven::read_sav("f:/michal/Desktop/SOCIAL_DIAGNOSIS_H_2000_2015.SAV",
                     user_na = TRUE, n_max=0)

# User missing values
nas <- lapply(d, labelled::na_values)
nnull <- sapply(nas, is.null)
table(nnull)
nas[!nnull]

# User missing ranges
nar <- lapply(d, labelled::na_range)
nnull <- sapply(nar, is.null)
table(nnull)
nar[!nnull]

table(sapply(nar[!nnull], length))

dplyr::bind_rows(
  lapply(nar[!nnull], setNames, c("from", "to")),
  .id="variable"
)
