#PREPROCESSING:
# modified by: Sílvia Rovira, @silrovira
# modified on: 06-03-2024
# modified by: Clara Tarragó, @claratg15
# modified on: 06-03-2024

#### Carregar les dades -------------------------------------------------

load("D:/3rCURS/2. anàlisi multivariant/treball/subset_houses.RData")

#### Afegir NA-----------------------------------------------------------

set.seed(12345)
num<- sample(5000,20)
set.seed(54321)
cat<- sample(5000,20)

### Afegim NAs a les variables time_on_market i interior_condition
for(i in 1:length(num)){
  tbl_houses_subset[num[i],"time_on_market"]<- NA
  tbl_houses_subset[cat[i],"interior_condition"]<- NA
}

###comprovem quines son les posicions on hi ha NA
which(is.na(tbl_houses_subset$time_on_market))
which(is.na(tbl_houses_subset$interior_condition))

### Agafem els 0 de la variable floor_area, que no tenen sentit, i els passem a NA

for (i in 1:dim(tbl_houses_subset)[1]){
  if(tbl_houses_subset[i,"floor_area"]==0){
    tbl_houses_subset[i,"floor_area"]<-NA
  }
}

### posem el outlier de parcel_size (9999999)a NA

outlier_parcel_size<-which(tbl_houses_subset[,"parcel_size"]==9999999)
tbl_houses_subset[outlier_parcel_size,"parcel_size"]<-NA


### Eliminem la variable price_metre. Una vegada imputem els valors NA de floor_area, la tornarem a crear
tbl_houses_subset <- tbl_houses_subset[,1:17]

### Save .Rdata -------------------------------------------------------------
wd <- "D:/3rCURS/2. anàlisi multivariant/treball/"
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

### MICE ----------------------------------------------------------------------

# install.packages("mice")
library(mice)

### Variables numèriques: time_on_market i floor_area
imp <- mice(tbl_houses_subset, m = 5, maxit = 50, meth = 'pmm', seed= 12345)
tbl_houses_subset <- complete(imp,1)

### Variable categòrica: interior_condition
tbl_houses_subset$interior_condition <- as.numeric(tbl_houses_subset$interior_condition)

imp <- mice(tbl_houses_subset, m = 5, maxit = 50, meth = 'pmm', seed= 12345)
tbl_houses_subset <- complete(imp,1)




### KNN  ----------------------------------------------------------------------

library(multiUS)
library(dplyr)
tbl_houses_subset$interior_condition <- as.numeric(tbl_houses_subset$interior_condition)
dfNA <- data.frame(floor_area = tbl_houses_subset$floor_area, time_on_market = tbl_houses_subset$time_on_market,interior_condition = tbl_houses_subset$interior_condition)
names(dfNA)
dfNoNA <- tbl_houses_subset[, !names(tbl_houses_subset) %in% names(dfNA)]
df_KNN <- cbind(KNNimp(dfNA),dfNoNA)
df_KNN <- df_KNN[,order(colnames(df_KNN))]
