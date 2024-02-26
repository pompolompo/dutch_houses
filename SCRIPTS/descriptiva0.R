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
library(kableExtra)

desc_parcel_size <- as.data.frame(psych::describe(tbl_houses_subset[, "parcel_size"]))
kable(desc_parcel_size, caption = "Anàlisi descriptiu: variable parcel_size") %>% kable_styling(full_width = FALSE)

desc_floor_area <- as.data.frame(psych::describe(tbl_houses_subset[, "floor_area"]))
kable(desc_floor_area, caption = "Anàlisi descriptiu: variable floor_area") %>% kable_styling(full_width = FALSE)

desc_sale_price <- as.data.frame(psych::describe(tbl_houses_subset[, "sale_price"]))
kable(desc_sale_price, caption = "Anàlisi descriptiu: variable sale_price") %>% kable_styling(full_width = FALSE)

desc_price_metre <- as.data.frame(psych::describe(tbl_houses_subset[, "price_metre"]))
kable(desc_price_metre, caption = "Anàlisi descriptiu: variable price_metre") %>% kable_styling(full_width = FALSE)


library(ggplot2)
num_var_ind_cont <- c(5,6,12,18)

for(k in num_var_ind_cont){
  print(ggplot(tbl_houses_subset, aes(x=!!sym(names(tbl_houses_subset)[k]))) + geom_histogram())
}

for(k in num_var_ind_cont){
  print(ggplot(tbl_houses_subset, aes(x="", y=!!sym(names(tbl_houses_subset)[k]))) + geom_boxplot())
}

## Variables categòriques --------------------------------------------------

# Descriptiva bivariant ---------------------------------------------------

## Variables numèriques ----------------------------------------------------

## Variables categòriques --------------------------------------------------




