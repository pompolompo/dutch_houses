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
  df <- as.data.frame(psych::describe(tbl_houses_subset[, i]))
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
  df <- as.data.frame(psych::describe(tbl_houses_subset[, i]))
  print(kable(df, caption = paste0("Anàlisi descriptiu: variable", " ", names(tbl_houses_subset[i]))) %>% kable_styling(full_width = FALSE))
}



for(k in var_num_disc){
  print(ggplot(tbl_houses_subset, aes(x=!!sym(names(tbl_houses_subset)[k]))) + geom_bar())

}


print(ggplot(tbl_houses_subset, aes(x=!!sym(names(tbl_houses_subset)[7]))) + geom_bar())


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
