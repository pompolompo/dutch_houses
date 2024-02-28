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


par(ask=TRUE)
for(k in var_num_disc){barplot(table(tbl_houses_subset[,k]), ylab = "Frequency", xlab = names(tbl_houses_subset)[k],main = paste("Barplot de", names(tbl_houses_subset)[k]))}
par(ask=FALSE)


## Variables categòriques --------------------------------------------------

var_cat<-c(1,3,4,8,9,10,15,16,17)

par(ask=TRUE)
for(k in var_cat){barplot(table(tbl_houses_subset[,k]), ylab = "Frequency", xlab = names(tbl_houses_subset)[k],main = paste("Barplot de", names(tbl_houses_subset)[k]))}
par(ask=FALSE)

table(tbl_houses_subset[,"city"])
table(tbl_houses_subset[,"type"])
table(tbl_houses_subset[,"construction_period"])
table(tbl_houses_subset[,"interior_condition"])
table(tbl_houses_subset[,"exterior_condition"])
table(tbl_houses_subset[,"year"])
table(tbl_houses_subset[,"house_id"])
table(tbl_houses_subset[,"postcode"])
table(tbl_houses_subset[,"energy_label"])

## Variables categòriques binàries

var_bin<-c(2,14)

par(ask=TRUE)
for(k in var_bin){barplot(table(tbl_houses_subset[,k]), ylab = "Frequency", xlab = names(tbl_houses_subset)[k],main = paste("Barplot de", names(tbl_houses_subset)[k]))}
par(ask=FALSE)

table(tbl_houses_subset[,"apartment"])
table(tbl_houses_subset[,"busy_street"])

# Descriptiva bivariant ---------------------------------------------------

## Variables numèriques ----------------------------------------------------

## Variables categòriques --------------------------------------------------

colors <- c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightcyan", 
                     "lightgray", "lightcoral", "lightseagreen", "lightsalmon")


library(kableExtra)


### Any i condició interior

a<-table(tbl_houses_subset$year,tbl_houses_subset$interior_condition)

kable(a, caption = "Taula de Contingencia: Year vs. Interior Condition")



ggplot(tbl_houses_subset, aes(x = year, fill = interior_condition)) +
         geom_bar(position = "dodge", width = 0.7) +
         labs(x = "year", y = "Conteig", fill = "interior condition") +
         ggtitle("Gràfic de barres bivariant entre year i interior condition") +
         theme_minimal()


mosaicplot(table(tbl_houses_subset$year,tbl_houses_subset$interior_condition), main = "Mosaic Plot  entre year i interior condition",sub="Year", col=colors, ylab="Interior Condition")



### Energy label i Condició interior

a<-table(tbl_houses_subset$energy_label,tbl_houses_subset$interior_condition)

kable(a, caption = "Taula de Contingencia: Energy Label vs. Interior Condition")

ggplot(tbl_houses_subset, aes(x = energy_label, fill = interior_condition)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "Energy_label", y = "Conteig", fill = "interior_condition") +
  ggtitle("Gràfic de barres bivariant entre Energy label i interior_condition") +
  theme_minimal()



mosaicplot(table(tbl_houses_subset$energy_label,tbl_houses_subset$interior_condition), main = "Mosaic Plot entre energy label i interior condition", sub="Energy_Label", col=colors, ylab="Interior condition")


### Condició exteriors i Energy label

a<-table(tbl_houses_subset$energy_label,tbl_houses_subset$exterior_condition)


kable(a, caption = "Taula de Contingencia: Energy Label vs. Exterior Condition")


ggplot(tbl_houses_subset, aes(x = energy_label, fill = exterior_condition)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "Energy_label", y = "Conteig", fill = "Exterior condition") +
  ggtitle("Gràfic de barras bivariant entre  Energy label i Exterior condition") +
  theme_minimal()



mosaicplot(table(tbl_houses_subset$energy_label,tbl_houses_subset$exterior_condition), main = "Mosaic Plot entre Energy label i Exterior condition", col=colors, ylab="Exterior condition")

### Any i Energy label



a<- table(tbl_houses_subset$year,tbl_houses_subset$energy_label)

kable(a, caption = "Taula de Contingencia: Year vs. Energy Label  ")


ggplot(tbl_houses_subset, aes(x = year, fill = energy_label)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "Year", y = "Conteig", fill = "Energy Label ") +
  ggtitle("Gràfic de barras bivariant entre  year i energy label") +
  theme_minimal()



mosaicplot(table(tbl_houses_subset$year,tbl_houses_subset$energy_label), main = "Mosaic Plot year i Energy label", col=colors)

###City i year
taula<-table(tbl_houses_subset$city,tbl_houses_subset$year)
kable(taula, caption = "Tabla de Contingencia: City vs. Year")

ggplot(tbl_houses_subset, aes(x = city, fill = year)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "city",fill = "year") +
  ggtitle("Gráfic de barres bivariant entre city i year") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$city,
                 tbl_houses_subset$year), 
           main = "Mosaic Plot", sub="city", col=colors,ylab = "year")


###City i energy_label
taula<-table(tbl_houses_subset$city,tbl_houses_subset$energy_label)
kable(taula, caption = "Tabla de Contingencia: City vs. Energy Label")

ggplot(tbl_houses_subset, aes(x = city, fill = energy_label)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "city",fill = "energy_label") +
  ggtitle("Gráfic de barres bivariant entre city i energy_label") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$city,
                 tbl_houses_subset$energy_label), 
           main = "Mosaic Plot", sub="city", col=colors,ylab = "energy_label")

###Type i year
taula<-table(tbl_houses_subset$type,tbl_houses_subset$year)
kable(taula, caption = "Tabla de Contingencia: Type vs. Year")

ggplot(tbl_houses_subset, aes(x = type, fill = year)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "type",fill = "year") +
  ggtitle("Gráfic de barres bivariant entre type i year") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$type,
                 tbl_houses_subset$year), 
           main = "Mosaic Plot", sub="type", col=colors,ylab = "year")

###Type i energy_label
taula<-table(tbl_houses_subset$type,tbl_houses_subset$energy_label)
kable(taula, caption = "Tabla de Contingencia: Type vs. Energy Label")

ggplot(tbl_houses_subset, aes(x = type, fill = energy_label)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "type", fill = "energy_label") +
  ggtitle("Gráfic de barres bivariant entre type i energy_label") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$type,
                 tbl_houses_subset$energy_label), 
           main = "Mosaic Plot", sub="type", col=colors,ylab = "energy_label")

###Construction_period i year
taula<-table(tbl_houses_subset$construction_period,tbl_houses_subset$year)
kable(taula, caption = "Tabla de Contingencia: Construction_period vs. Year")

ggplot(tbl_houses_subset, aes(x = construction_period, fill = year)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "construction_period", fill = "year") +
  ggtitle("Gráfic de barres bivariant entre construction Period i year") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$construction_period,
                 tbl_houses_subset$year), 
           main = "Mosaic Plot", sub="construction_period", col=colors,ylab = "year")

###Construction_period i energy_label
taula<-table(tbl_houses_subset$construction_period,tbl_houses_subset$energy_label)
kable(taula, caption = "Tabla de Contingencia: Construction Period vs. Energy Label")

ggplot(tbl_houses_subset, aes(x = construction_period, fill = energy_label)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "construction_period", fill = "energy_label") +
  ggtitle("Gráfic de barres bivariant entre construction_period i energy_label") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$construction_period,
                 tbl_houses_subset$energy_label), 
           main = "Mosaic Plot", sub="construction_period", col=colors,ylab = "energy_label")

### Year i exterior condition
taula<-table(tbl_houses_subset$year,tbl_houses_subset$exterior_condition)
kable(taula, caption = "Tabla de Contingencia: Construction Period vs. Energy Label")

ggplot(tbl_houses_subset, aes(x = year, fill = exterior_condition)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "year", fill = "exterior_condition") +
  ggtitle("Gráfic de barres bivariant entre year i exterior_condition") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$year,
                 tbl_houses_subset$exterior_condition), 
           main = "Mosaic Plot", sub="year", col=colors,ylab = "exterior_condition")

###Construction_period i interior_condition
taula<-table(tbl_houses_subset$construction_period,tbl_houses_subset$interior_condition)
kable(taula, caption = "Tabla de Contingencia: Construction Period vs. Interior Condition")

ggplot(tbl_houses_subset, aes(x = construction_period, fill = interior_condition)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "construction_period", fill = "interior_condition") +
  ggtitle("Gráfic de barres bivariant entre construction_period i interior_condition") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$construction_period,
                 tbl_houses_subset$interior_condition), 
           main = "Mosaic Plot", sub="construction_period", col=colors,ylab = "interior_condition")

###Construction_period i exterior_condition
taula<-table(tbl_houses_subset$construction_period,tbl_houses_subset$exterior_condition)
kable(taula, caption = "Tabla de Contingencia: Construction Period vs. Exterior Condition")

ggplot(tbl_houses_subset, aes(x = construction_period, fill = exterior_condition)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "construction_period", fill = "exterior_condition") +
  ggtitle("Gráfic de barres bivariant entre construction_period i exterior_condition") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$construction_period,
                 tbl_houses_subset$exterior_condition), 
           main = "Mosaic Plot", sub="construction_period", col=colors,ylab = "exterior_condition")

###Interior_condition i exterior_condition
taula<-table(tbl_houses_subset$interior_condition,tbl_houses_subset$exterior_condition)
kable(taula, caption = "Tabla de Contingencia: Interior Condition vs. Exterior Condition")

ggplot(tbl_houses_subset, aes(x = interior_condition, fill = exterior_condition)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "interior_condition", fill = "exterior_condition") +
  ggtitle("Gráfic de barres bivariant entre interior_condition i exterior_condition") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$interior_condition,
                 tbl_houses_subset$exterior_condition), 
           main = "Mosaic Plot", sub="interior_condition", col=colors,ylab = "exterior_condition")


###city - type

a<-table(tbl_houses_subset$city,tbl_houses_subset$type)
kable(a, caption = "Tabla de Contingencia: city vs. type")


ggplot(tbl_houses_subset, aes(x = city, fill = type)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "city", y = "Freqüència", fill = "type") +
  ggtitle("Gràfica de Barres Bivariant") +
  theme_minimal()



mosaicplot(table(tbl_houses_subset$city,
                 tbl_houses_subset$type), 
           main = "Mosaic Plot", sub="city", col=colors,ylab = "type")



###city-construction_period

a<-table(tbl_houses_subset$city,tbl_houses_subset$construction_period)
kable(a, caption = "Tabla de Contingencia: city vs. construction_period")

ggplot(tbl_houses_subset, aes(x = city, fill = construction_period)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "city", y = "Freqüència", fill = "construction_period") +
  ggtitle("Gràfica de Barres Bivariant") +
  theme_minimal()


mosaicplot(table(tbl_houses_subset$city,
                 tbl_houses_subset$construction_period), 
           main = "Mosaic Plot", sub="city", col=colors,
           ylab = "construction_period")

### city - condicions habitatge

a<-table(tbl_houses_subset$city,tbl_houses_subset$interior_condition)
kable(a, caption = "Tabla de Contingencia: city vs. interior_condition")

ggplot(tbl_houses_subset, aes(x = city, fill = interior_condition)) +
  geom_bar(position = "dodge",, width = 0.7) +
  labs(x = "city", y = "Freqüència", fill = "interior_condition") +
  ggtitle("Gràfica de Barres Bivariant") +
  theme_minimal()



mosaicplot(table(tbl_houses_subset$city,
                 tbl_houses_subset$interior_condition), 
           main = "Mosaic Plot", sub="city", col=colors,
           ylab = "interior_condition")


###city - condicions exterior


a<-table(tbl_houses_subset$city,tbl_houses_subset$exterior_condition)
kable(a, caption = "Tabla de Contingencia: city vs. exterior_condition")

ggplot(tbl_houses_subset, aes(x = city, fill = exterior_condition)) +
  geom_bar(position = "dodge",, width = 0.7) +
  labs(x = "city", y = "Freqüència", fill = "exterior_condition") +
  ggtitle("Gràfica de Barres Bivariant") +
  theme_minimal()



mosaicplot(table(tbl_houses_subset$city,
                 tbl_houses_subset$exterior_condition), 
           main = "Mosaic Plot", sub="city", col=colors,
           ylab = "exterior_condition")


###type - construction period


a<-table(tbl_houses_subset$type,tbl_houses_subset$construction_period)
kable(a, caption = "Tabla de Contingencia: type vs. construction_period")



ggplot(tbl_houses_subset, aes(x = type, fill = construction_period)) +
  geom_bar(position = "dodge",, width = 0.7) +
  labs(x = "type", y = "Freqüència", fill = "construction_period") +
  ggtitle("Gràfica de Barres Bivariant") +
  theme_minimal()


mosaicplot(table(tbl_houses_subset$type,
                 tbl_houses_subset$construction_period), 
           main = "Mosaic Plot", sub="type", col=colors,
           ylab = "construction_period")


###type - interior_conditon

a<-table(tbl_houses_subset$type,tbl_houses_subset$interior_condition)
kable(a, caption = "Tabla de Contingencia: type vs. interior_condition")

ggplot(tbl_houses_subset, aes(x = type, fill = interior_condition)) +
  geom_bar(position = "dodge",, width = 0.7) +
  labs(x = "type", y = "Freqüència", fill = "interior_condition") +
  ggtitle("Gràfica de Barres Bivariant") +
  theme_minimal()



mosaicplot(table(tbl_houses_subset$type,
                 tbl_houses_subset$interior_condition), 
           main = "Mosaic Plot", sub="type", col=colors,
           ylab = "interior_condition")

###type - exterior condition


a<-table(tbl_houses_subset$type,tbl_houses_subset$exterior_condition)
kable(a, caption = "Tabla de Contingencia: type vs. exterior_condition")

ggplot(tbl_houses_subset, aes(x = type, fill = exterior_condition)) +
  geom_bar(position = "dodge",, width = 0.7) +
  labs(x = "type", y = "Freqüència", fill = "exterior_condition") +
  ggtitle("Gràfica de Barres Bivariant") +
  theme_minimal()


mosaicplot(table(tbl_houses_subset$type,
                 tbl_houses_subset$exterior_condition), 
           main = "Mosaic Plot", sub="type", col=colors,
           ylab = "exterior_condition")
