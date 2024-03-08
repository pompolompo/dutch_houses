# PREPROCESSING:
# modified by: Sílvia Rovira, @silrovira
# modified on: 06-03-2024
# modified by: Clara Tarragó, @claratg15
# modified on: 06-03-2024
# modified by: Clara Tarragó, @claratg15
# modified on: 07-03-2024

#### Carregar les dades -------------------------------------------------

load("/Volumes/USB DISK/3rCURS/2. anàlisi multivariant/treball/subset_houses.RData")

#### Afegir NA-----------------------------------------------------------

set.seed(12345)
num <- sample(5000,20)

### Afegim NAs a les variables time_on_market i interior_condition
for(i in 1:length(num)){
  tbl_houses_subset[num[i],"time_on_market"]<- NA
}

### Comprovem quines son les posicions on hi ha NA
which(is.na(tbl_houses_subset$floor_area))

### Agafem els 0 de la variable floor_area, que no tenen sentit, i els passem a NA

for (i in 1:dim(tbl_houses_subset)[1]){
  if(tbl_houses_subset[i,"floor_area"]==0){
    tbl_houses_subset[i,"floor_area"]<-NA
  }
}

### Passem l'outlier de parcel_size (9999999)a NA

outlier_parcel_size<-which(tbl_houses_subset[,"parcel_size"]==9999999)
tbl_houses_subset[outlier_parcel_size,"parcel_size"]<-NA

### Eliminem la variable price_metre. Una vegada imputem els valors NA de floor_area, la tornarem a crear
tbl_houses_subset <- tbl_houses_subset[,1:17]


### Save .Rdata -------------------------------------------------------------
wd <- "/Volumes/USB DISK/3rCURS/2. anàlisi multivariant/treball/"
outputname <- "subset_houses_NA"

save(
  tbl_houses_subset,
  file = paste0(wd, outputname, ".RData")
)

### Test de Little -------------------------------------------------------------
### Per verificar que els Nas esiguin distribuits aleatòriament.

# install.packages("naniar")
library(naniar)
mcar_test(tbl_houses_subset)
# no sé perquè em surt p-valor de 0. Si algú s'ho pot mirar porfi
# pot ser que sigui 0 pq els NA al ser generats de manera completament 
# aleatòria no segueixen cap mena de distribució.


### MICE  ----------------------------------------------------------------------

imp <- mice(tbl_houses_subset, m = 5, maxit = 50, meth = 'pmm', seed= 12345)
tbl_houses_subset <- complete(imp,1)


### KNN  ----------------------------------------------------------------------

install.packages("multiUS")
library(multiUS)
library(dplyr)
varnum <- c(5,6,7,12,13)
#dfnum <- tbl_houses_subset[,varnum]
dfcat <- tbl_houses_subset[,-varnum]
df_KNN <- cbind(KNNimp(dfnum),dfcat)
df_KNN <- df_KNN[,order(colnames(df_KNN))]


### Canvis necessàris -----------------------------------------------------------------

### Tornem a crear la variable price_metre
tbl_houses_subset[["price_metre"]] <- tbl_houses_subset[["sale_price"]] / tbl_houses_subset[["floor_area"]]

### Recodifiquem la variable busy_street com a factor
tbl_houses_subset$busy_street <- as.factor(tbl_houses_subset$busy_street)


### Save .Rdata -------------------------------------------------------------
wd <- "/Volumes/USB DISK/3rCURS/2. anàlisi multivariant/treball/"
outputname <- "subset_houses_IMP"

save(
  tbl_houses_subset,
  file = paste0(wd, outputname, ".RData")
)
