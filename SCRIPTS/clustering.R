# Metadata ----------------------------------------------------------------
# written by: Ferran Garcia, @pompolompo
# written on: 07-02-2024
# purpose: cluster observations
# description: agrupacions en clústers utilitzant diferents mètodes:
#                1. dendogrames variables mixtes (diferents nombres de clústers)
#                2. dendogrames numèrics (diferents nombres de clústers)
#                3. k-means
#              validació dels mètodes:
#                a. índex KH (numèriques) 
#                b. colze ()
#                c. scatterplot (numeriques)


# Libraries ---------------------------------------------------------------
library(cluster)

# Global options ----------------------------------------------------------
wd <- "/home/ferran/Documents/Universitat/MULTI/dutch_houses/"
dat <- "subset_houses.RData"
k <- 2:10 # nombre de clústers

# Source ------------------------------------------------------------------


# Imports -----------------------------------------------------------------
load(file = paste0(wd, "DATA/", dat))


# Clustering --------------------------------------------------------------


## Dendograma mixt ---------------------------------------------------------

### Distància ---------------------------------------------------------------
# per variables mixtes només podem utilitzar la distància 'gower'
# l'argument stand = TRUE estandaritza les variables abans de calcular les distàncies: 
# així la magnitud de les variables no influeix en la seva importància en la distància
# gower no és distància (per això l'elevem al quadrat)
# gower és una mesura de dissimilitud (per això fem 1 - gower^2) --> no feu cas
distances <- list(
  gower = daisy(tbl_houses_subset, metric = "gower", stand = TRUE)**2
)

### Agrupacions i dendograma ------------------------------------------------
# utilitzem ward com a definició de 'vecindad', amb altres mètodes podemo obtenir diferents resultats
# 'vecindad' és la semlança d'una observació amb la resta del seu grup
h <- hclust(
  d = distances[["gower"]],
  method = "ward.D2"
)

plot(h, labels = FALSE) # dendograma

# agrupacions per diferent quantitat de grups (k)
# també es poden determinar els grups a partir d'una distància per on tallar
grups <- cutree(
  tree = h,
  k = k
)





