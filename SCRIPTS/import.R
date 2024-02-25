# Metadata ----------------------------------------------------------------
# written by: Ferran Garcia, @pompolompo
# written on: 23-02-2024
# purpose: import dataset
# description: reads data of dutch housing and selects random subset of observation
# modified by: Ferran Garcia, @pompolompo
# modified on: 25-02-2024
# modified by: Silvia Rovira
# modified on: 25-02-2024
# modified by: Elies Roman
# modified on: 25-02-2024


# Libraries ---------------------------------------------------------------
library(readxl)


# Global Options --------------------------------------------------------------
wd <- "/home/ferran/Documents/Universitat/MULTI/dutch_houses"
wd<-"C:/Users/hp/Documents/dutch_houses"
wd<-"C:/Users/maria/OneDrive/Desktop/multi"

file <- "/dutch_houses.xlsx"
outputname <- "subset_houses"

arrel <- 12345
subset_obs <- 5000

# Import ------------------------------------------------------------------
tbl_houses <- readxl::read_excel(
  paste0(wd, "/DATA", file)
  )



# Noves variables ---------------------------------------------------------

tbl_houses[,"price_metre"]<-tbl_houses[,"sale_price"]/tbl_houses[,"floor_area"]



# Subset ------------------------------------------------------------------
tbl_houses[["busy_street"]] <- gsub(
  x = tbl_houses[["busy_street"]],
  pattern = "2",
  replacement = "1")


tbl_houses<-subset(tbl_houses, floor_area!=0)

set.seed(seed = arrel)
selected_obs <- sample(x = 1:nrow(tbl_houses), 
                       size = subset_obs,
                       replace = FALSE)
tbl_houses_subset <- tbl_houses[selected_obs, ]


# Save .Rdata -------------------------------------------------------------

save(
  tbl_houses_subset,
  file = paste0(wd, "/DATA/", outputname, ".RData")
    )
