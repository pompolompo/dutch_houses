# Metadata ----------------------------------------------------------------
# written by: Ferran Garcia, @pompolompo
# written on: 24-02-2024
# purpose: decribe dataset
# description: exploratory decription before imputation of NA's
# modified by: Clara Tarragó, @claratg15
# modified on: 26-02-2024
# modified by: Sílvia Rovira, @silrovira
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

### Numèriques disctretes

var_num_disc<-c(7,13)


for(k in var_num_disc){
  print(ggplot(tbl_houses_subset, aes(x=!!sym(names(tbl_houses_subset)[k]))) + geom_bar())
}

desc_city <- as.data.frame(table(tbl_houses_subset[, "rooms"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable rooms") %>% kable_styling(full_width = FALSE)

desc_city <- as.data.frame(table(tbl_houses_subset[, "time_on_market"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable time_on_market") %>% kable_styling(full_width = FALSE)



## Variables categòriques --------------------------------------------------

var_cat<-c(1,3,4,8,9,10,15,16,17)

for(k in var_cat){
  print(ggplot(tbl_houses_subset, aes(x=!!sym(names(tbl_houses_subset)[k]))) + geom_bar())
}


desc_city <- as.data.frame(table(tbl_houses_subset[, "city"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable city") %>% kable_styling(full_width = FALSE)

desc_city <- as.data.frame(table(tbl_houses_subset[, "type"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable type") %>% kable_styling(full_width = FALSE)

desc_city <- as.data.frame(table(tbl_houses_subset[, "construction_period"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable construction_period") %>% kable_styling(full_width = FALSE)

desc_city <- as.data.frame(table(tbl_houses_subset[, "interior_condition"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable interior_condition") %>% kable_styling(full_width = FALSE)

desc_city <- as.data.frame(table(tbl_houses_subset[, "exterior_condition"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable exterior_condition") %>% kable_styling(full_width = FALSE)

desc_city <- as.data.frame(table(tbl_houses_subset[, "year"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable year") %>% kable_styling(full_width = FALSE)

desc_city <- as.data.frame(table(tbl_houses_subset[, "house_id"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable house_id") %>% kable_styling(full_width = FALSE)

desc_city <- as.data.frame(table(tbl_houses_subset[, "postcode"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable postcode") %>% kable_styling(full_width = FALSE)

desc_city <- as.data.frame(table(tbl_houses_subset[, "energy_label"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable energy_label") %>% kable_styling(full_width = FALSE)


## Variables categòriques binàries

var_bin<-c(2,14)

for(k in var_bin){
  print(ggplot(tbl_houses_subset, aes(x=!!sym(names(tbl_houses_subset)[k]))) + geom_bar())
}


desc_city <- as.data.frame(table(tbl_houses_subset[, "apartment"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable apartment") %>% kable_styling(full_width = FALSE)

desc_city <- as.data.frame(table(tbl_houses_subset[, "busy_street"]))
kable(desc_city, caption = "Anàlisi descriptiu: variable busy_street") %>% kable_styling(full_width = FALSE)


# Descriptiva bivariant ---------------------------------------------------

## Variables numèriques ----------------------------------------------------

## Variables categòriques --------------------------------------------------




