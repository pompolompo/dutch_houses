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
dat <- "subset_houses_IMP.RData"
c <- 2:10 # nombre de clústers

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
  k = c
)

## Dendograma numèric -------------------------------------------------------

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
   k = 3 # posem aquest com un altre k, ja ho decidirem
)


## K-means -------------------------------------------------------------------
# modified by: Clara Tarragó, @claratg15
# modified on: 14-03-2024
# modified by: Judit Costa, @juditcosta
# modified on: 14-03-2024

numeriques <- tbl_houses_subset[ , c(5,6,7,12,13,18)]

k <- 6      # nombre òptim de clústers
clust_kmeans <- kmeans(numeriques, centers = k)

names(numeriques)
print(clust_kmeans)
attributes(clust_kmeans)

# Nombre d'observacions a cada grup
clust_kmeans$size

# Variabilitat de cada grup
clust_kmeans$withinss

# Centroides de cada grup de totes les variables
clust_kmeans$centers

# Descomposició de l'inèrcia
Bss <- sum(rowSums(clust_kmeans$centers^2)*clust_kmeans$size)
Bss
Wss <- sum(clust_kmeans$withinss)
Wss
Tss <- clust_kmeans$totss
Tss

Bss+Wss

Ib1 <- 100*Bss/(Bss+Wss)
Ib1

CalinskiHarabaz<-Bss/Wss

# Afegim una variable que indiqui a quin clúster pertany cada observació
# tbl_houses_subset[,19] <- clust_kmeans$cluster

# Mètodes de Validació ----------------------------------------------------

## Elbow Method (numèriques) -----------------------------------------------
# modified by: Ferran garcia, @pompolompo
# modified on: 15-03-2024
# purpose: trobara el nombre de clústers òptim pel clústering de K-means
# desc: 1. es fa el clústering per difernts nombresx de grups, 
#       2. es calcula una mètrica de validació per cada ajust i.e:
#           - % variància explicada
#           - inèrcia = SSE_entreclust/SSE_dinsclust
#       3. s'escull el nombre de clústers on canvii bruscament la mètrica

fit_kmeans <- sapply(
  X = c,
  FUN = stats::kmeans,
  x = numeriques
)

inertia <- unlist(fit_kmeans["betweenss", ])/unlist(fit_kmeans["tot.withinss", ])

plot(x = c, y = inertia, type = "b", 
     main = "Elbow method", xlab = "Nombre de Clústers",
     sub = "Sembla que el 6 és el nombre òptim de clústers")
