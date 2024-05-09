
# CREACIÓ VARIABLES DUMMY ------------------------------------------------------
# written by: Clara Tarragó, @claratg15
# written on: 09-05-2024
# purpose: create dummy variables
# description: creates dummy variables for the categorical variables


# Carreguem les dades ----------------------------------------------------------
load("tbl_houses_subset_PCA.RData")

attach(tbl_houses_subset)

# Creació variables dummy per a les categòriques -------------------------------

## City ------------------------------------------
freqs <- table(tbl_houses_subset$city)

resultat <- numeric(dim(tbl_houses_subset)[1])

vars <- 17


for(j in 1:length(freqs)){
  resultat <- numeric(dim(tbl_houses_subset)[1])
  for(i in 1:dim(tbl_houses_subset)[1]){
    if(city[i] == names(freqs)[j]){
      resultat[i] <- 1
    }else{
      resultat[i] <- 0
    }
  }
  vars <- vars + 1
  tbl_houses_subset[,vars] <- resultat
  names(tbl_houses_subset)[vars] <- names(freqs)[j]
}


## Type ------------------------------------------
freqs <- table(tbl_houses_subset$type)

resultat <- numeric(dim(tbl_houses_subset)[1])

for(j in 1:length(freqs)){
  resultat <- numeric(dim(tbl_houses_subset)[1])
  for(i in 1:dim(tbl_houses_subset)[1]){
    if(type[i] == names(freqs)[j]){
      resultat[i] <- 1
    }else{
      resultat[i] <- 0
    }
  }
  vars <- vars + 1
  tbl_houses_subset[,vars] <- resultat
  names(tbl_houses_subset)[vars] <- paste0("Type:", names(freqs)[j])
}


## Construction_period ------------------------------------------
freqs <- table(tbl_houses_subset$construction_period)

resultat <- numeric(dim(tbl_houses_subset)[1])

for(j in 1:length(freqs)){
  resultat <- numeric(dim(tbl_houses_subset)[1])
  for(i in 1:dim(tbl_houses_subset)[1]){
    if(construction_period[i] == names(freqs)[j]){
      resultat[i] <- 1
    }else{
      resultat[i] <- 0
    }
  }
  vars <- vars + 1
  tbl_houses_subset[,vars] <- resultat
  names(tbl_houses_subset)[vars] <- paste0("Construction period:", names(freqs)[j])
}


## Interior_condition  ------------------------------------------
freqs <- table(tbl_houses_subset$interior_condition)

resultat <- numeric(dim(tbl_houses_subset)[1])

for(j in 1:length(freqs)){
  resultat <- numeric(dim(tbl_houses_subset)[1])
  for(i in 1:dim(tbl_houses_subset)[1]){
    if(interior_condition[i] == names(freqs)[j]){
      resultat[i] <- 1
    }else{
      resultat[i] <- 0
    }
  }
  vars <- vars + 1
  tbl_houses_subset[,vars] <- resultat
  names(tbl_houses_subset)[vars] <- paste0("Interior condition:", names(freqs)[j])
}


## Exterior_condition  ------------------------------------------
freqs <- table(tbl_houses_subset$exterior_condition)

resultat <- numeric(dim(tbl_houses_subset)[1])

for(j in 1:length(freqs)){
  resultat <- numeric(dim(tbl_houses_subset)[1])
  for(i in 1:dim(tbl_houses_subset)[1]){
    if(exterior_condition[i] == names(freqs)[j]){
      resultat[i] <- 1
    }else{
      resultat[i] <- 0
    }
  }
  vars <- vars + 1
  tbl_houses_subset[,vars] <- resultat
  names(tbl_houses_subset)[vars] <- paste0("Exterior condition:", names(freqs)[j])
}


## Energy_label  ------------------------------------------
freqs <- table(tbl_houses_subset$energy_label)

resultat <- numeric(dim(tbl_houses_subset)[1])

for(j in 1:length(freqs)){
  resultat <- numeric(dim(tbl_houses_subset)[1])
  for(i in 1:dim(tbl_houses_subset)[1]){
    if(energy_label[i] == names(freqs)[j]){
      resultat[i] <- 1
    }else{
      resultat[i] <- 0
    }
  }
  vars <- vars + 1
  tbl_houses_subset[,vars] <- resultat
  names(tbl_houses_subset)[vars] <- paste0("Energy label:", names(freqs)[j])
}


## Cluster_group  ------------------------------------------
freqs <- table(tbl_houses_subset$cluster_group)

resultat <- numeric(dim(tbl_houses_subset)[1])

for(j in 1:length(freqs)){
  resultat <- numeric(dim(tbl_houses_subset)[1])
  for(i in 1:dim(tbl_houses_subset)[1]){
    if(cluster_group[i] == names(freqs)[j]){
      resultat[i] <- 1
    }else{
      resultat[i] <- 0
    }
  }
  vars <- vars + 1
  tbl_houses_subset[,vars] <- resultat
  names(tbl_houses_subset)[vars] <- paste0("Cluster group:", names(freqs)[j])
}


## Temps_mercat  ------------------------------------------
freqs <- table(tbl_houses_subset$temps_mercat)

resultat <- numeric(dim(tbl_houses_subset)[1])

for(j in 1:length(freqs)){
  resultat <- numeric(dim(tbl_houses_subset)[1])
  for(i in 1:dim(tbl_houses_subset)[1]){
    if(temps_mercat[i] == names(freqs)[j]){
      resultat[i] <- 1
    }else{
      resultat[i] <- 0
    }
  }
  vars <- vars + 1
  tbl_houses_subset[,vars] <- resultat
  names(tbl_houses_subset)[vars] <- paste0("Temps al mercat:", names(freqs)[j])
}

tbl_houses_subset <- tbl_houses_subset[,-c(1,3,4,8,9,10,11,14,16,17)]


# Guardem el .RData ------------------------------------------------------------

outputname <- "subset_houses_DISCR"
wd <- "/Volumes/USB DISK/3rCURS/2. anàlisi multivariant/treball/"

save(
  tbl_houses_subset,
  file = paste0(wd, outputname, ".RData")
)



