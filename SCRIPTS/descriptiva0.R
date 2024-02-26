# Metadata ----------------------------------------------------------------
# written by: Ferran Garcia, @pompolompo
# written on: 24-02-2024
# purpose: decribe dataset
# description: exploratory decription before imputation of NA's
# modified by: Clara Tarragó, @claratg15
# modified on: 26-02-2024

# Libraries ---------------------------------------------------------------

# Global Options ----------------------------------------------------------
wd <- "/home/ferran/Documents/Universitat/MULTI/dutch_houses"
file <- "/subset_houses.RData"
  
# Import ------------------------------------------------------------------
load(
  paste0(wd, "/DATA", file)
)

# Tipologia Variables -----------------------------------------------------

str(tbl_houses_subset)

num_var_ind <- sapply(
  X = tbl_houses_subset,
  FUN = is.numeric
  ) |>
  which()

char_var_ind <- sapply(
  X = tbl_houses_subset,
  FUN = is.character
) |>
  which()

fact_var_ind <- sapply(
  X = tbl_houses_subset,
  FUN = is.factor
) |>
  which()

# @pompolompo, 24-02-2024
# cal canviar la tipologia d'algunes variables
# en general la majoria de strings han de ser factors

# Descriptiva Univariant --------------------------------------------------

## Variables numèriques ----------------------------------------------------

## Variables categòriques --------------------------------------------------

# Descriptiva bivariant ---------------------------------------------------

## Variables numèriques ----------------------------------------------------

## Variables categòriques --------------------------------------------------




