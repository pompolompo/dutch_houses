#PREPROCESSING:
# modified by: Sílvia Rovira, @silrovira
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

wd<-"C:/Users/hp/Documents/dutch_houses"

# Save .Rdata -------------------------------------------------------------
save(
  tbl_houses_subset,
  file = paste0(wd, "/DATA/", outputname, ".RData")
)
