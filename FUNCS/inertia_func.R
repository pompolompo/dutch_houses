# Metadata ----------------------------------------------------------------
# written by: Ferran Garcia, @pompolompo
# written on: 28-03-2024
# purpose: function to calculate inertia in kmeans clustering


# Global Options ----------------------------------------------------------
wd <- "/home/ferran/Documents/Universitat/MULTI/dutch_houses/"
dat <- "subset_houses_IMP.RData"
k <- 5

# Libraries ---------------------------------------------------------------


# Imports -----------------------------------------------------------------
load(file = paste0(wd, "DATA/", dat))


# Sources -----------------------------------------------------------------


# Inertia Function --------------------------------------------------------

inertia <- function(fit, dataset){
  centroides <- fit[["centers"]] 
  clusters <- fit[["cluster"]]
  inertia <- numeric(1)
  
  for(n in sort(unique(clusters), decreasing = FALSE)){
    ind <- which(clusters == n)
    cluster_n <- t(dataset[ind, ])
    centroide_n <- centroides[n, ]
    inertia_n <- (cluster_n - centroide_n)**2 |> sum()
    inertia <- inertia + inertia_n
  }
  
  return(inertia)
  }


# Example -----------------------------------------------------------------
# seleccionem les variables numèriques
numeriques <- sapply(
  X = tbl_houses_subset,
  FUN = is.numeric
) |> which()

dataset <- tbl_houses_subset[, numeriques]

# apliquem la funció
fit <- kmeans(
  x = dataset,
  centers = k
)

# comprovem que és el mateix que tot.within.ss
inertia(fit, dataset)
fit[["tot.withinss"]]
