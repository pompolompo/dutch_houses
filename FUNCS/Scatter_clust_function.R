# Metadata ----------------------------------------------------------------
# written by: Ferran Garcia, @pompolompo
# written on: 28-03-2024
# purpose: function to scatterplot clusters 
#         based on clustering group and variables

# Global Options ----------------------------------------------------------
wd <- "/home/ferran/Documents/Universitat/MULTI/dutch_houses/"
dat <- "subset_houses_IMP.RData"

# Libraries ---------------------------------------------------------------


# Imports -----------------------------------------------------------------
load(file = paste0(wd, "DATA/", dat))

# Sources -----------------------------------------------------------------


# Scatter_clust function --------------------------------------------------

Scatter_clust <- function(
    dades,        # dataframe amb les dades
    var1,         # variable eix X
    var2,         # variable eix Y
    pal,          # paleta de colors
    clust,        # clústers de les observacions
    centre = NULL # centroides de cada clúster (si null no es dibuixen)
                          ){
  plot(dades[[var1]], dades[[var2]], 
       col = pal[clust], xlab = var1, ylab = var2, cex = .75,
       main = paste("Scatterplot de", var1, "i", var2))
  
  if(!is.null(centre)){
    points(centre[, var1], centre[, var2], pch = 20)
  } 
  
  return(invisible())
}

# Example -----------------------------------------------------------------

# # clústering
# numeriques <- sapply(
#   X = tbl_houses_subset,
#   FUN = is.numeric
# ) |> which()
# 
# k <- 6
# 
# clust_kmeans <- kmeans(
#   x = tbl_houses_subset[, numeriques],
#   centers = k
# )
# 
# # funció:
# Scatter_clust(
#   var1 = "time_on_market",
#   var2 = "sale_price",
#   pal = palette.colors(n = k, palette = "Tableau"),
#   clust = clust_kmeans[["cluster"]],
#   centre = clust_kmeans[["centers"]][, c("time_on_market", "sale_price")]
# )
# 
# v1 <- names(numeriques)[4]
# v2 <- names(numeriques)[6]
# 
# Scatter_clust(
#   var1 = v1,
#   var2 = v2,
#   pal = palette.colors(n = k, palette = "Tableau"),
#   clust = clust_kmeans[["cluster"]],
#   centre = clust_kmeans[["centers"]][, c(v1, v2)]
# )