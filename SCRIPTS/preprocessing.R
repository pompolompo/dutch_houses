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
set.seed(12345)
cat<- sample(5000,20)

### Afegim NAs a les variables time_on_market i interior_condition
for(i in 1:length(num)){
  tbl_houses_subset[num[i],"time_on_market"]<- NA
  tbl_houses_subset[cat[i],"interior_condition"]<- NA
}

### Agafem els 0 de la variable floor_area, que no tenen sentit, i els passem a NA

for (i in 1:dim(tbl_houses_subset)[1]){
  if(tbl_houses_subset[i,"floor_area"]==0){
    tbl_houses_subset[i,"floor_area"]<-NA
  }
}

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



