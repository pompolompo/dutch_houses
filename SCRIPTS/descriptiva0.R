# Metadata ----------------------------------------------------------------
# written by: Ferran Garcia, @pompolompo
# written on: 24-02-2024
# purpose: decribe dataset
# description: exploratory decription before imputation of NA's

# Libraries ---------------------------------------------------------------
#library(kableExtra)
library(psych)
library(ggplot2)


# Global Options ----------------------------------------------------------
wd <- "/home/ferran/Documents/Universitat/MULTI/dutch_houses"
file <- "/subset_houses.RData"

  
# Import ------------------------------------------------------------------
load(
  paste0(wd, "/DATA", file)
)


# Descriptiva Univariant --------------------------------------------------

## Variables numèriques ----------------------------------------------------

### Numèriques contínues ----------------------------------------------------
# modified by: Clara Tarragó, @claratg15
# modified on: 26-02-2024
# modified by: Ferran Garcia, @pompolompo
# modified on: 28-02-2024
# modified by: Clara Tarragó, @claratg15
# modified on: 28-02-2024

library(psych)
library(kableExtra)

num_var_ind_cont <- c(5,6,12,18)

for(i in num_var_ind_cont){
  df <- as.data.frame(psych::describe(data.frame(tbl_houses_subset[, i])))
  print(kable(df, caption = paste0("Anàlisi descriptiu: variable", " ", names(tbl_houses_subset[i]))) %>% kable_styling(full_width = FALSE))
}

library(ggplot2)
for(k in num_var_ind_cont){
  print(ggplot(tbl_houses_subset, aes(x=!!sym(names(tbl_houses_subset)[k]))) + geom_histogram())
}

for(k in num_var_ind_cont){
  print(ggplot(tbl_houses_subset, aes(x="", y=!!sym(names(tbl_houses_subset)[k]))) + geom_boxplot())
}

#### --------------

nom_contingut <- "desc0_taules_cont.RData"

taules_cont <- list(
  parcel_size = psych::describe(tbl_dades[["parcel_size"]]),
  floor_area = psych::describe(tbl_dades[["floor_area"]]),
  sale_price = psych::describe(tbl_dades[["sale_price"]]),
  price_metre = psych::describe(tbl_dades[["price_metre"]])
)

caption_cont <- list(
  parcel_size = "Anàlisi descriptiu: variable parcel_size",
  floor_area = "Anàlisi descriptiu: variable floor_area",
  sale_price = "Anàlisi descriptiu: variable sale_price",
  price_metre = "Anàlisi descriptiu: variable price_metre"
)

save(
  taules_cont, caption_cont, 
  file = paste0(wd, "/FIGURES/DESC0/TAULES/", nom_contingut)
)

nb_num <- sapply(
  X = tbl_dades,
  FUN = is.numeric
) |> which() |>
  names()

for(nb_var in nb_num){
  ggplot2::ggplot(
    data = tbl_dades,
    mapping = aes(x = .data[[nb_var]])
  ) + ggplot2::geom_histogram(
    bins = 30
  ) + ggplot2::labs(
    title = paste0("Histograma de: ", nb_var),
    x = nb_var,
    y = NULL
  ) + theme_bw()
  
  ggsave(
    filename = paste0(wd, "/FIGURES/DESC0/GRAFS/",
                      "hist_", nb_var, ".png"),
    width = 2000, 
    height = 1500, 
    units = "px"
  )
  
  ggplot2::ggplot(
    data = tbl_dades,
    mapping = aes(x = "", 
                  y = .data[[nb_var]])
  ) + ggplot2::geom_violin() + 
    ggplot2::labs(
      title = paste0("Violinplot de: ", nb_var),
      x = nb_var,
      y = NULL
    ) + theme_bw()
  
  ggsave(
    filename = paste0(wd, "/FIGURES/DESC0/GRAFS/",
                      "violin_", nb_var, ".png"),
    width = 2000, 
    height = 1500, 
    units = "px"
  )  
}


### Numèriques disctretes ---------------------------------------------------
# modified by: Sílvia Rovira, @silrovira
# modified on: 26-02-2024

path<-("C:/Users/hp/Documents/dutch_houses")

var_num_disc<-c(7,13)
noms_var<-names(tbl_houses_subset)

for(i in var_num_disc){
  df <- as.data.frame(psych::describe(data.frame(tbl_houses_subset[, i])))
  print(kable(df, caption = paste0("Anàlisi descriptiu: variable", " ", names(tbl_houses_subset[i]))) %>% kable_styling(full_width = FALSE))
}



for(k in var_num_disc){
  png(filename=paste0(path,"/FIGURES/DESC0/GRAFS/","grafic_barres_", noms_var[k], ".png"),  width = 1000, height = 750, units = "px")
  print(ggplot(tbl_houses_subset, aes(x=!!sym(names(tbl_houses_subset)[k]))) + geom_bar())
  dev.off()
}




## Variables categòriques --------------------------------------------------

### Categòriques amb més de 2 nivells ---------------------------------------
# modified by: Sílvia Rovira, @silrovira
# modified on: 26-02-2024

var_cat<-c(1,3,4,8,9,10,15,16,17)


for(k in var_cat){
  png(filename=paste0(path,"/FIGURES/DESC0/GRAFS/","grafic_barres_", noms_var[k], ".png"),  width = 1000, height = 750, units = "px")
  print(ggplot(tbl_houses_subset, aes(x=!!sym(names(tbl_houses_subset)[k]))) + geom_bar())
  dev.off()
}


for(i in var_cat){
  df <- as.data.frame(table(tbl_houses_subset[, i]))
  print(kable(df, caption = paste0("Anàlisi descriptiu: variable", " ", names(tbl_houses_subset[i]))) %>% kable_styling(full_width = FALSE))
}



### Categòriques binàries ---------------------------------------
# modified by: Sílvia Rovira, @silrovira
# modified on: 26-02-2024

var_bin<-c(2,14)

for(k in var_bin){
  png(filename=paste0(path,"/FIGURES/DESC0/GRAFS/","grafic_barres_", noms_var[k], ".png"),  width = 1000, height = 750, units = "px")
  print(ggplot(tbl_houses_subset, aes(x=!!sym(names(tbl_houses_subset)[k]))) + geom_bar())
}

for(i in var_bin){
  df <- as.data.frame(table(tbl_houses_subset[, i]))
  print(kable(df, caption = paste0("Anàlisi descriptiu: variable", " ", names(tbl_houses_subset[i]))) %>% kable_styling(full_width = FALSE))
}


# Descriptiva bivariant ---------------------------------------------------

## Variables numèriques ----------------------------------------------------
# modified by: Judit Costa, @juditcosta
# modified on: 27-02-2024

path <- ("C:/Users/judit/Desktop/uni/3r/2nquatri/multi/treball")
numeriques<- c(5,6,7,12,13,18)
pdf(paste0(path,"Descriptiva_bivariant_numeriques.pdf"))
for(i in 1:length(numeriques)){
  for(j in i+1:length(numeriques)){
    if(j<= length(numeriques)){
    print(ggplot(tbl_houses_subset,aes(x=!!sym(names(tbl_houses_subset)[numeriques[i]]),y=!!sym(names(tbl_houses_subset)[numeriques[j]])),ylab=names(tbl_houses_subset[numeriques[j]])) + geom_point())
    
    }
  }
}
dev.off()

################################################ no va
## Variables numèriques pdf
path <- ("C:/Users/judit/Desktop/uni/3r/2nquatri/multi/treball/")
numeriques<- c(5,6,7,12,13,18)
pdf(paste0(path,"Descriptiva_bivariant_numeriques.pdf"))
for(i in 1:length(numeriques)){
  for(j in i+1:length(numeriques)){
    if(j<= length(numeriques)){
      print(ggplot(tbl_houses_subset,aes(x=!!sym(names(tbl_houses_subset)[numeriques[i]]),y=!!sym(names(tbl_houses_subset)[numeriques[j]])),ylab=names(tbl_houses_subset[numeriques[j]])) + geom_point())
    }
  }
}
dev.off()

#png
path <- ("C:/Users/judit/Desktop/uni/3r/2nquatri/multi/treball/")
numeriques<- c(5,6,7,12,13,18)
for(i in 1:length(numeriques)){
  for(j in i+1:length(numeriques)){
    if(j<= length(numeriques)){
      png(filename=paste0(path,"scatterplot_", noms_var[i], ".png"),  width = 1000, height = 750, units = "px")
      print(ggplot(tbl_houses_subset,aes(x=!!sym(names(tbl_houses_subset)[numeriques[i]]),y=!!sym(names(tbl_houses_subset)[numeriques[j]])),ylab=names(tbl_houses_subset[numeriques[j]])) + geom_point())
    }
  }
}

pdf(paste0(path,"Analisi_descriptiu_bivariant_numeriques.pdf"))
for(i in 1:length(numeriques)){
  for(j in i+1:length(numeriques)){
    if(j<= length(numeriques)){
      df <- as.data.frame(table(tbl_houses_subset[,i]))
      print(kable(df,caption=paste0("Anàlisi descriptiu: variable", " ", names(tbl_houses_subset[i]))) %>% kable_styling(full_width=FALSE))
    }
  }
}
#######################################

## Variables categòriques --------------------------------------------------

<<<<<<< HEAD
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

kable(a, caption = "Taula de Contingència: Energy Label vs. Interior Condition")

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

kable(a, caption = "Taula de Contingència: Year vs. Energy Label  ")


ggplot(tbl_houses_subset, aes(x = year, fill = energy_label)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "Year", y = "Conteig", fill = "Energy Label ") +
  ggtitle("Gràfic de barras bivariant entre  year i energy label") +
  theme_minimal()



mosaicplot(table(tbl_houses_subset$year,tbl_houses_subset$energy_label), main = "Mosaic Plot year i Energy label", col=colors)

###City i year
taula<-table(tbl_houses_subset$city,tbl_houses_subset$year)
kable(taula, caption = "Tabla de Contingència: City vs. Year")

ggplot(tbl_houses_subset, aes(x = city, fill = year)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "city",fill = "year") +
  ggtitle("Gràfic de barres bivariant entre city i year") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$city,
                 tbl_houses_subset$year), 
           main = "Mosaic Plot", sub="city", col=colors,ylab = "year")


###City i energy_label
taula<-table(tbl_houses_subset$city,tbl_houses_subset$energy_label)
kable(taula, caption = "Taula de Contingència: City vs. Energy Label")

ggplot(tbl_houses_subset, aes(x = city, fill = energy_label)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "city",fill = "energy_label") +
  ggtitle("Gràfic de barres bivariant entre city i energy_label") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$city,
                 tbl_houses_subset$energy_label), 
           main = "Mosaic Plot", sub="city", col=colors,ylab = "energy_label")

###Type i year
taula<-table(tbl_houses_subset$type,tbl_houses_subset$year)
kable(taula, caption = "Taula de Contingència: Type vs. Year")

ggplot(tbl_houses_subset, aes(x = type, fill = year)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "type",fill = "year") +
  ggtitle("Gràfic de barres bivariant entre type i year") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$type,
                 tbl_houses_subset$year), 
           main = "Mosaic Plot", sub="type", col=colors,ylab = "year")

###Type i energy_label
taula<-table(tbl_houses_subset$type,tbl_houses_subset$energy_label)
kable(taula, caption = "Taula de Contingència: Type vs. Energy Label")

ggplot(tbl_houses_subset, aes(x = type, fill = energy_label)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "type", fill = "energy_label") +
  ggtitle("Gràfic de barres bivariant entre type i energy_label") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$type,
                 tbl_houses_subset$energy_label), 
           main = "Mosaic Plot", sub="type", col=colors,ylab = "energy_label")

###Construction_period i year
taula<-table(tbl_houses_subset$construction_period,tbl_houses_subset$year)
kable(taula, caption = "Taula de Contingència: Construction_period vs. Year")

ggplot(tbl_houses_subset, aes(x = construction_period, fill = year)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "construction_period", fill = "year") +
  ggtitle("Gràfic de barres bivariant entre construction Period i year") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$construction_period,
                 tbl_houses_subset$year), 
           main = "Mosaic Plot", sub="construction_period", col=colors,ylab = "year")

###Construction_period i energy_label
taula<-table(tbl_houses_subset$construction_period,tbl_houses_subset$energy_label)
kable(taula, caption = "Taula de Contingència: Construction Period vs. Energy Label")

ggplot(tbl_houses_subset, aes(x = construction_period, fill = energy_label)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "construction_period", fill = "energy_label") +
  ggtitle("Gràfic de barres bivariant entre construction_period i energy_label") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$construction_period,
                 tbl_houses_subset$energy_label), 
           main = "Mosaic Plot", sub="construction_period", col=colors,ylab = "energy_label")

### Year i exterior condition
taula<-table(tbl_houses_subset$year,tbl_houses_subset$exterior_condition)
kable(taula, caption = "Taula de Contingència: Construction Period vs. Energy Label")

ggplot(tbl_houses_subset, aes(x = year, fill = exterior_condition)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "year", fill = "exterior_condition") +
  ggtitle("Gràfic de barres bivariant entre year i exterior_condition") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$year,
                 tbl_houses_subset$exterior_condition), 
           main = "Mosaic Plot", sub="year", col=colors,ylab = "exterior_condition")

###Construction_period i interior_condition
taula<-table(tbl_houses_subset$construction_period,tbl_houses_subset$interior_condition)
kable(taula, caption = "Taula de Contingència: Construction Period vs. Interior Condition")

ggplot(tbl_houses_subset, aes(x = construction_period, fill = interior_condition)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "construction_period", fill = "interior_condition") +
  ggtitle("Gràfic de barres bivariant entre construction_period i interior_condition") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$construction_period,
                 tbl_houses_subset$interior_condition), 
           main = "Mosaic Plot", sub="construction_period", col=colors,ylab = "interior_condition")

###Construction_period i exterior_condition
taula<-table(tbl_houses_subset$construction_period,tbl_houses_subset$exterior_condition)
kable(taula, caption = "Taula de Contingència: Construction Period vs. Exterior Condition")

ggplot(tbl_houses_subset, aes(x = construction_period, fill = exterior_condition)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "construction_period", fill = "exterior_condition") +
  ggtitle("Gràfic de barres bivariant entre construction_period i exterior_condition") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$construction_period,
                 tbl_houses_subset$exterior_condition), 
           main = "Mosaic Plot", sub="construction_period", col=colors,ylab = "exterior_condition")

###Interior_condition i exterior_condition
taula<-table(tbl_houses_subset$interior_condition,tbl_houses_subset$exterior_condition)
kable(taula, caption = "Taula de Contingència: Interior Condition vs. Exterior Condition")

ggplot(tbl_houses_subset, aes(x = interior_condition, fill = exterior_condition)) +
  geom_bar(position = "dodge", width = 0.7) +
  labs(x = "interior_condition", fill = "exterior_condition") +
  ggtitle("Gràfic de barres bivariant entre interior_condition i exterior_condition") +
  theme_minimal()

mosaicplot(table(tbl_houses_subset$interior_condition,
                 tbl_houses_subset$exterior_condition), 
           main = "Mosaic Plot", sub="interior_condition", col=colors,ylab = "exterior_condition")


###city - type

a<-table(tbl_houses_subset$city,tbl_houses_subset$type)
kable(a, caption = "Taula de Contingència: city vs. type")


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
kable(a, caption = "Taula de Contingència: city vs. construction_period")

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
kable(a, caption = "Taula de Contingència: city vs. interior_condition")

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
kable(a, caption = "Taula de Contingència: city vs. exterior_condition")

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
kable(a, caption = "Taula de Contingència: type vs. construction_period")



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
kable(a, caption = "Taula de Contingència: type vs. interior_condition")

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
kable(a, caption = "Taula de Contingència: type vs. exterior_condition")

ggplot(tbl_houses_subset, aes(x = type, fill = exterior_condition)) +
  geom_bar(position = "dodge",, width = 0.7) +
  labs(x = "type", y = "Freqüència", fill = "exterior_condition") +
  ggtitle("Gràfica de Barres Bivariant") +
  theme_minimal()


mosaicplot(table(tbl_houses_subset$type,
                 tbl_houses_subset$exterior_condition), 
           main = "Mosaic Plot", sub="type", col=colors,
           ylab = "exterior_condition")

## Variables categòriques i numèriques -------------------------------------
# modified by: Bernat Padrosa, @bernat16
# modified on: 27-02-2024

path<- ("D:/3rCURSESTADISTICA/multi")
categoriques<- c(1,2,3,4,8,9,10,14,17)
numeriques<- c(5,6,7,12,13,18)
pdf(paste0(path,"DescriptivaBivariant.pdf"))
for(i in 1:length(categoriques)){
  for(j in 1:length(numeriques)){
    print(ggplot(tbl_houses_subset,aes(x=!!sym(names(tbl_houses_subset)[categoriques[i]]),y=!!sym(names(tbl_houses_subset)[numeriques[j]])),ylab=names(tbl_houses_subset[numeriques[j]])) + geom_boxplot())
  }
}
dev.off()
