# Metadata ----------------------------------------------------------------
# written by: Ferran Garcia
# written on: 03-05-2024
# modified on: 05-05-2024
# purpose: clustering & profiling post pca
# desc: jerarquic clustering & profiling 
# to do:
#  - fer profiling
#      + interpretar estadístics resum
#      + acabar gràfics --> mosaicplots i scatterplots
#      + interpretar gràfics
#      + posar nom als clústers
#  - repetir-ho amb les dimensions significatives enlloc de les variables originals


# Libraries ---------------------------------------------------------------
library(cluster)
library(dendextend)
library(dplyr)

# Global options ----------------------------------------------------------
wd <- "/home/ferran/Documents/Universitat/MULTI/dutch_houses/"
dat <- "subset_houses_IMP.RData"
dim_significa <- 3 # utilitzo les dimensions del script PCA.R 

# Imports -----------------------------------------------------------------
load(file = paste0(wd, "DATA/", dat))


# Sources -----------------------------------------------------------------
source(paste0(wd, "FUNCS/", "Scatter_clust_function.R"))


# Sense afegir variables --------------------------------------------------


## PCA0 -------------------------------------------------------------------
lis_pca0 <- tbl_houses_subset[, sapply(tbl_houses_subset, is.numeric)] |> 
  prcomp(scale. = TRUE)

eigenvalues <- lis_pca0[["sdev"]]**2; round(eigenvalues, 3) # 3
var_explica <- cumsum(eigenvalues/sum(eigenvalues)); round(var_explica, 3) # 3
dim_significa0 <- 3 # dimensions finals

x_pca0 <- lis_pca0[["x"]][, 1:dim_significa0]


### Interpretació dimensions PCA0 -----------------------------------------
i <- 1
j <- 2

dimX <- lis_pca0[["rotation"]][, paste0("PC", i)]
dimY <- lis_pca0[["rotation"]][, paste0("PC", j)]

rangX <- range(dimX) + c(-.5, .5)
rangY <- range(dimY) + c(-.5, .5)

plot(NULL, xlim = rangX, ylim = rangY, xlab = i, ylab = j,
     main = paste("Dimensions factorials", i, "i", j), las = 1)
abline(v = 0, h = 0)
arrows(x0 = 0, y0 = 0, x1 = dimX, y1 = dimY, lwd = 2)
text(dimX, dimY, names(dimX), offset = 3)

# Dimensió 1: Petites, barates, dolentes
# Les més correlacionaes (+) --> NULL
# Les no correlacionades (=) --> time_on_market
# Les menys correlacionades (-) --> floor_area, sale_price, rooms; parcel_size, price_metre
#
# Vivendes petites no molt cares, algunes costen de vendre i altres no


# Totes les hi variables estan negativament correlacionades o no correlacionades
#   Les que més floor_area, sale_price_rooms
#   Una mica menys parcel_size i price_metre
#   No correlacionades time_on_market

# Dimensió 2: Localització cèntrica
# La més correlacionada (+) és price_metre, també ho està sale_price
# La menys correlacionada (-) és time_on market, també ho estàn rooms, floor_area i parcel_size
# 
# Les correlacionades (+) indiquen un preu alt (amenities en zonesd d'alta demanda)
# Les correlacionades (-) indiquen dificultat de venda (molt temps al mercat i molt espai)

# Dimensió 3: 
# Les més correlacionades (+) són rooms i floor_are (no massa)
# Les menys correlacionades (-) són time on market (de lluny), price_metre i parcel_size (no massa)
#
# Vivendes petites amb moltes habitacions i preu baix que estan poc temps al mercat


## Clustering 0 -----------------------------------------------------------
distances0 <- dist(x_pca0)


### Agrupació jeràrquica --------------------------------------------------
jerarq0 <- hclust(
  d = distances0, 
  method = "ward.D2"
)  

color_branches(as.dendrogram(jerarq0), k = 6) |> 
  plot(main = "Sense afegir variables, k = 6", leaflab = "none")

grups_jerarq0 <- cutree(
  tree = jerarq0,
  k = 6
)

tbl_houses_subset[["grup"]] <- as.factor(grups_jerarq0)

## Profiling 0  -----------------------------------------------------------

### Estadístics resum 0 ----------------------------------------------------
resum0 <- group_by(tbl_houses_subset, grup) |> 
  summarise(
    across(
      where(is.numeric),
      mean,
      .names = "{.col}"
      )
    )

resum0 <-t(resum0)[-1,]
colnames(resum0) <- paste("Grup", 1:6)
resum0 <- matrix(as.numeric(resum0), ncol = 6, byrow = FALSE,
                 dimnames = dimnames(resum0))

# ordre i xifres que faciliten l'interpretació
resum0[c("price_metre", "sale_price"), ] <- resum0["sale_price", ]/1000
resum0 <- cbind(resum0, "Mitjana" = rowMeans(resum0)) |> round(3) 
resum0 <- resum0[ 
  c(
    "sale_price", "price_metre", "rooms", "floor_area", "parcel_size",
    "time_on_market"
  ),
]


### Gràfics 0 -------------------------------------------------------------
# boxplots --> numèriques
with(tbl_houses_subset, boxplot(sale_price ~ grup))
with(tbl_houses_subset, boxplot(price_metre ~ grup))
tbl_houses_subset[tbl_houses_subset[["floor_area"]] < 4000, ] |> 
  with(boxplot(floor_area ~ grup))
tbl_houses_subset[tbl_houses_subset[["parcel_size"]] < 2000, ] |> 
  with(boxplot(parcel_size ~ grup))
with(tbl_houses_subset, boxplot(rooms ~ grup))
with(tbl_houses_subset, boxplot(time_on_market ~ grup))

# scatterplots --> numèriques
pal <- palette.colors(n = 6, palette = "Tableau")
plot(x_pca0[, c("PC1", "PC2")], col = pal[grups_jerarq0])
plot(x_pca0[, c("PC1", "PC3")], col = pal[grups_jerarq0])
plot(x_pca0[, c("PC2", "PC3")], col = pal[grups_jerarq0])

# mosaicplots --> prova chi2 --> H_0: clust indep. energy_lab vs H_1: dependents --> problema
## eficiència energètica
auxN <- with(tbl_houses_subset, table(grup, energy_label))
colnames(auxN) <- LETTERS[7:1]
chisq.test(auxN) # dependents
# diferència difícil de caracteritzar
mosaicplot(auxN, col = RColorBrewer::brewer.pal(7, "BrBG"), xlab = "", ylab = "",
           main = "Clúster contra energy_label", cex.axis = 1.25, las = 1)

## construction_period
auxN <- with(tbl_houses_subset, table(grup, construction_period))[, -1]
chisq.test(auxN) # dependents
# diferència difícil de caracteritzar
mosaicplot(auxN, col = RColorBrewer::brewer.pal(10, "BrBG"), xlab = "", ylab = "",
           main = "Clúster contra construction_period", cex.axis = 1.25, las = 1,
           sub = "1 és el més vell i 9 és el més nou")


