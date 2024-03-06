#PREPROCESSING:
# modified by: Sílvia Rovira, @silrovira
# modified on: 06-03-2024
# modified by: Clara Tarragó, @claratg15
# modified on: 06-03-2024

#### Afegir NA-----------------------------------------------------------

set.seed(arrel)
num<- sample(5000,20)
set.seed(arrel)
cat<- sample(5000,20)

for(i in 1:length(num)){
  tbl_houses_subset[num[i],"time_on_market"]<- NA
  tbl_houses_subset[cat,"interior_condition"]<- NA
}

###Agafar els 0 de la variable floor_area i passar-los a NA

for (i in 1:dim(tbl_houses_subset)[1]){
  if(tbl_houses_subset[i,"floor_area"]==0){
    tbl_houses_subset[i,"floor_area"]<-NA
  }
}

# Save .Rdata -------------------------------------------------------------
wd<-"C:/Users/hp/Documents/dutch_houses"
save(
  tbl_houses_subset,
  file = paste0(wd, "/DATA/", outputname, ".RData")
)
