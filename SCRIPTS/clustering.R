# Metadata ----------------------------------------------------------------
# written by: Ferran Garcia, @pompolompo
# written on: 07-02-2024
# purpose: cluster observations
# description: agrupacions en clústers utilitzant diferents mètodes:
#                1. dendogrames variables mixtes (diferents nombres de clústers)
#                2. dendogrames numèrics (diferents nombres de clústers)
#                3. k-means
#              validació dels mètodes:
#                a. índex CH (numèriques) 
#                b. colze (numèriques)
#                c. scatterplot (numeriques)


# Libraries ---------------------------------------------------------------
library(cluster)

# Global options ----------------------------------------------------------
wd <- "/home/ferran/Documents/Universitat/MULTI/dutch_houses/"
dat <- "subset_houses_IMP.RData"
c <- 2:10 # nombre de clústers

# Source ------------------------------------------------------------------
source(paste0(wd, "FUNCS/", "Scatter_clust_function.R"))

# Imports -----------------------------------------------------------------
load(file = paste0(wd, "DATA/", dat))


# Clustering --------------------------------------------------------------

## Distància ---------------------------------------------------------------
# per variables mixtes només podem utilitzar la distància 'gower'
# l'argument stand = TRUE estandaritza les variables abans de calcular les distàncies: 
# així la magnitud de les variables no influeix en la seva importància en la distància
# gower no és distància (per això l'elevem al quadrat)
# gower és una mesura de dissimilitud (per això fem 1 - gower^2) --> no feu cas

numeriques <- sapply(
  X = tbl_houses_subset,
  FUN = is.numeric
) |> which()

distances <- list(
  gower = daisy(tbl_houses_subset, metric = "gower", stand = TRUE)**2,
  euclid = dist(tbl_houses_subset[, numeriques])
)

## Dendograma mixt ---------------------------------------------------------
# utilitzem ward com a definició de 'vecindad', amb altres mètodes podem
# obtenir diferents resultats
# 'vecindad' és la semblança d'una observació amb la resta del seu grup
jerarq_mixt <- hclust(
  d = distances[["gower"]],
  method = "ward.D2"
)

plot(jerarq_mixt, hang = -1, cex = 0.6, labels = FALSE) # dendograma
# hang permet disminuir la grandària del text

# agrupacions per diferent quantitat de grups (k)
# també es poden determinar els grups a partir d'una distància per on tallar
grups_jerarq_mixt <- cutree(
  tree = jerarq_mixt,
  k = c
)

## Dendograma numèric -------------------------------------------------------
# es fa servir la distància euclidiana ja que estem treballant amb només 
# variables numèriques

jerarq_num <- hclust(
  d = distances[["euclid"]], 
  method = "ward.D2"
  )  

plot(jerarq_num, hang = -1, cex = 0.6, labels = FALSE)

grups_jerarq_num <- cutree(
   tree = jerarq_num,
   k = c
)

## K-means -------------------------------------------------------------------
# modified by: Ferran Garcia, @pompolompo
# modified on: 28-03-2024

# aplico kmeans per tots els valors de k --> c
clust_kmeans <- lapply(
  X = c,
  FUN = kmeans,
  x = tbl_houses_subset[, numeriques]
) |> setNames(paste0("nclust", c))

# creo una taula amb les magnituds importants
# divideixo entre 10e10 els que no són ratio per millorar visibliitat
resum_kmeans <- sapply(
  X = clust_kmeans,
  FUN = function(fit){
    r <- c(
      'wSS/bSS' = fit[["tot.withinss"]]/fit[["betweenss"]],
      'calinski' = 
        (fit[["betweenss"]]/(length(max(fit[["cluster"]]) - 1)))/
        (fit[["tot.withinss"]]/(length(fit[["cluster"]]) - max(fit[["cluster"]]))),
      'inertia*' = fit[["tot.withinss"]]/10e10,
      'betweenSS*' = fit[["betweenss"]]/10e10
    )
    return(r)
  }
) |> t()
round(resum_kmeans, digits = 3)

# creo una taula amb la mida de cada clúster (columna)
# per cada valor de l'hiperparàmetre k (files)
size_kmeans <- sapply(
  X = clust_kmeans,
  FUN = function(fit){
    mida <- fit[["size"]]
    r <- c(min(mida), max(mida), mida, 
           rep(0, length(clust_kmeans) + 1 - length(mida)))
    return(r)
  }
) |> t()

colnames(size_kmeans) <- c("min", "max", paste0("clust", 1:max(c)))
size_kmeans

# creo una llista amb els centroides de cada clústering
# cada element de la llista és una matriu que correspona aun valor de k on:
# les files de cada matriu corresponen als centroides dels grups 
centre_kmeans <- lapply(
  X = clust_kmeans,
  FUN = function(fit){
    centre <- fit[["centers"]]
    return(centre)
  }
)

# Mètodes de Validació ----------------------------------------------------

## Elbow Method (numèriques) -----------------------------------------------
# modified by: Ferran garcia, @pompolompo
# modified on: 15-03-2024
# purpose: trobara el nombre de clústers òptim pel clústering de K-means
# desc: 1. es fa el clústering per difernts nombresx de grups, 
#       2. es calcula una mètrica de validació per cada ajust i.e:
#           - % variància explicada
#           - calinski = SSE_entreclust/SSE_dinsclust
#       3. s'escull el nombre de clústers on canvii bruscament la mètrica

plot(x = c, y = resum_kmeans[, "inertia*"], type = "b", lwd = 3,
     main = "Elbow method: Inèrcia", xlab = "Nombre de Clústers", ylab = "",
     sub = "Sembla que el 5 o 6 és el nombre òptim de clústers")

plot(x = c, y = resum_kmeans[, "calinski"], type = "b", lwd = 3,
     main = "Elbow method: Calinski", xlab = "Nombre de Clústers", ylab = "",
     sub = "Sembla que el 5 o 6 és el nombre òptim de clústers")

plot(x = c, y = resum_kmeans[, "calinski"]/sum(resum_kmeans[, "calinski"]), 
     type = "l", col = "lightblue", xlab = "Nombre de Clústers", ylab = "", lwd = 3,
     main = "Elbow method: Inèrcia (salmó) i Calinski (blau); normalitzats entre 0 i 1")
lines(x = c, y = resum_kmeans[, "inertia*"]/sum(resum_kmeans[, "inertia*"]),
      type = "l", col = "salmon", lwd = 3)


## Calinski ----------------------------------------------------------------
# el mètode de calinski prèn el nombre de clústers qua maximitza l'índex
# problema: l'índex creix conforme k augmenta, aparentment de forma indefinida
# he provat de fer-ho amb c <- 2:1000 i no està clar si seguirà creixent o no
which.max(resum_kmeans[, "calinski"]) |> names()

## Scatterplot -------------------------------------------------------------
# comprovem intuitivament si els clústers tenen sentit
# graficant dues variables numèriques i els seus corresponents clústers

v <- length(numeriques)
cat("Hi ha", factorial(v)/(factorial(v-2)*factorial(2)), 
    "potencials combinacions de variables numèriques:")

# quines variables utilitzem?
v1 <- names(numeriques)[1]
v2 <- names(numeriques)[5]

# quin clústering utilitzem?
k <- 6

# utilitzant funció Scatter_clust definida al directori FUNCS
# gràfic --> kmeans(6)
Scatter_clust(
  var1 = v1,
  var2 = v2,
  clust = clust_kmeans[[k-1]][["cluster"]],
  centre = centre_kmeans[[k-1]][, c(v1, v2)],
  pal = palette.colors(n = k, palette = "Tableau")
)

# gràfic --> jeràrquic_numèric(6)
Scatter_clust(
  var1 = v1,
  var2 = v2,
  clust = grups_jerarq_num[, k-1],
  pal = palette.colors(n = k, palette = "Tableau")
)

# falta provar amb altres variables i altre nombre de grups
# algunes variables van bé fer agrupar (les escollides) i altres no

# AQUESTS LOOFS FAN TOTS ELS SCATTER (de forma cutrilla)
# for(i in 1:6){
#   for(j in (i+1):6){
#     v1 <- names(numeriques)[i]
#     v2 <- names(numeriques)[j]
#     
#     Scatter_clust(
#       var1 = v1,
#       var2 = v2,
#       clust = clust_kmeans[[k-1]][["cluster"]],
#       centre = centre_kmeans[[k-1]][, c(v1, v2)],
#       pal = palette.colors(n = k, palette = "Tableau")
#     )
#     
#     cat("Variables:", v1, "i", v2, "\n")
#     Sys.sleep(3)
#   }
# }