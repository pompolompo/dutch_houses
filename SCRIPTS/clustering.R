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

categoriques <- c(1:4,8:11,14,16,17)
tbl_houses_subset[categoriques] <- lapply(tbl_houses_subset[categoriques], as.factor)
distances <- list(
  gower = daisy(tbl_houses_subset, metric = "gower", stand = TRUE)**2
)

### Agrupacions i dendograma ------------------------------------------------
# utilitzem ward com a definició de 'vecindad', amb altres mètodes podem
#obtenir diferents resultats
# 'vecindad' és la semblança d'una observació amb la resta del seu grup
h <- hclust(
  d = distances[["gower"]],
  method = "ward.D2"
)

plot(h,hang=-1,cex=0.6,labels=FALSE) # dendograma
#hang permet disminuir la granddària del text

# agrupacions per diferent quantitat de grups (k)
# també es poden determinar els grups a partir d'una distància per on tallar
grups <- cutree(
  tree = h,
  k = k
)





#PREPROFILING
#tbl_houses_subset$grups <- as.character(cutree(
#  tree = h,
#  k = 4
#))

#library(FactoMineR)
#plot(FactoMineR::catdes(tbl_houses_subset, num.var = 19))


### Dendograma numèric -------------------------------------------------------

# es fa servir la distància euclidiana ja que estem treballant amb només 
# variables numèriques. 

numeriques <- c(5:7,12:13,18)
names(tbl_houses_subset[numeriques]) #comprovem que estiguin bé les numèriques

data_numeriques<-data.frame(tbl_houses_subset[,numeriques])
dist_euclidiana  <- dist(data_numeriques[,1:6])

h2 <- hclust(dist_euclidiana,method="ward.D2")  

str(h2)

plot(h2,hang=-1,cex=0.6,labels=FALSE)

grups <- cutree(
   tree = h2,
   k = 3 #posem aquest com un altre k, ja ho decidirem
)





# PREPROFILING

#tbl_houses_subset$grups <- as.character(cutree(
#  tree = h2,
#  k = 6
#))

# library(FactoMineR)
# sense_id <- tbl_houses_subset[,c(1:14,17:19)]
# num <- tbl_houses_subset[,c(numeriques,19)]
# plot(FactoMineR::catdes(num, num.var = 7))
