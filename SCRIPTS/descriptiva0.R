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

### Numèriques contínues

library(psych)
psych::describe(tbl_houses_subset[, "parcel_size"])
psych::describe(tbl_houses_subset[, "floor_area"])
psych::describe(tbl_houses_subset[, "sale_price"])
psych::describe(tbl_houses_subset[, "price_metre"])

num_var_ind_cont <- c(5,6,12,18)

par(ask=TRUE)
for(k in num_var_ind_cont){hist(tbl_houses_subset[,k], main = paste("Histograma de", names(tbl_houses_subset)[k]))}
par(ask=FALSE)

par(ask=TRUE)
for(k in num_var_ind_cont){boxplot(tbl_houses_subset[,k], main = paste("Boxplot de", names(tbl_houses_subset)[k]))}
par(ask=FALSE)

## Variables categòriques --------------------------------------------------

# Descriptiva bivariant ---------------------------------------------------

## Variables numèriques ----------------------------------------------------

## Variables categòriques --------------------------------------------------




