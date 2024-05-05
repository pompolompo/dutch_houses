# Metadata ----------------------------------------------------------------
# written by: Ferran Garcia
# written on: 03-05-2024
# modified on: 05-05-2024
# purpose: clustering & profiling post pca
# desc: jerarquic clustering & profiling 
#          - amb les variables numèriques 'pures'
#          - convertint categòriques ordinals en numèriques
# to do:
#  - interpretar dimensions pca amb "noves" variables
#  - fer clústering: quins mètodes? --> jeràrquic, kmeans
#  - fer profiling: procediment exactament? -->

# Libraries ---------------------------------------------------------------
library(cluster)
library(dendextend)

# Global options ----------------------------------------------------------
wd <- "/home/ferran/Documents/Universitat/MULTI/dutch_houses/"
dat <- "subset_houses_IMP.RData"
dim_significa <- 3 # utilitzo les dimensions del script PCA.R 

# Imports -----------------------------------------------------------------
load(file = paste0(wd, "DATA/", dat))


# Afegint variables -------------------------------------------------------
# convertim construction period, interior/exterior condition i energy label a integer
# així les podem interpretar al pca i al profiling
# construction_period: contra més gran és el codi (números del 0 al 9) més nova és la casa
# interior_condition: contra més gran és el codi (números del 0 al 9) millor és la condició
# exterior_condition: contra més gran és el codi (números del 0 al 9) millor és la condició
# energy_label: contra més gran és el codi (números de l'1 al 7) pitjor és l'eficiència energètica
tbl_houses_subset[["interior_condition"]] <- as.integer(tbl_houses_subset[["interior_condition"]])
tbl_houses_subset[["exterior_condition"]] <- as.integer(tbl_houses_subset[["exterior_condition"]])
tbl_houses_subset[["energy_label"]] <- as.integer(tbl_houses_subset[["energy_label"]])
tbl_houses_subset[["construction_period"]] <- as.integer(tbl_houses_subset[["construction_period"]])

## PCA1 -------------------------------------------------------------------
lis_pca1 <- tbl_houses_subset[, sapply(tbl_houses_subset, is.numeric)] |> 
  prcomp(scale. = TRUE)

eigenvalues <- lis_pca1[["sdev"]]**2; round(eigenvalues, 3) # 4
var_explica <- cumsum(eigenvalues/sum(eigenvalues)); round(var_explica, 3) # 4 o 5
dim_significa1 <- 4 # dimensions finals

x_pca1 <- lis_pca1[["x"]][, 1:dim_significa1]


## Interpretació dimensions PCA1 ------------------------------------------
# falta interpretació
i <- 3
j <- 4

par(mfrow = c(1, 2))

# variables "qualitat"
vars <- c("construction_period", "interior_condition", "exterior_condition",
          "energy_label", "time_on_market")
dimX <- lis_pca1[["rotation"]][, paste0("PC", i)][vars]
dimY <- lis_pca1[["rotation"]][, paste0("PC", j)][vars]

rangX <- range(dimX) + c(-.5, .5)
rangY <- range(dimY) + c(-.5, .5)

plot(NULL, xlim = rangX, ylim = rangY, xlab = i, ylab = j,
     main = paste("Dimensions factorials", i, "i", j), las = 1)
abline(v = 0, h = 0)
arrows(x0 = 0, y0 = 0, x1 = dimX, y1 = dimY, lwd = 2)
text(dimX, dimY, names(dimX), offset = 3)

# variables "preu i espai"
vars <- c("parcel_size", "floor_area", "rooms", "sale_price", "price_metre")
dimX <- lis_pca1[["rotation"]][, paste0("PC", i)][vars]
dimY <- lis_pca1[["rotation"]][, paste0("PC", j)][vars]

rangX <- range(dimX) + c(-.5, .5)
rangY <- range(dimY) + c(-.5, .5)

plot(NULL, xlim = rangX, ylim = rangY, xlab = i, ylab = j,
     main = paste("Dimensions factorials", i, "i", j), las = 1)
abline(v = 0, h = 0)
arrows(x0 = 0, y0 = 0, x1 = dimX, y1 = dimY, lwd = 2)
text(dimX, dimY, names(dimX), offset = 3)


## Clustering 1 -----------------------------------------------------------
distances1 <- dist(x_pca1)

