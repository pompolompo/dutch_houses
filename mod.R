# Metadata ----------------------------------------------------------------
# written by: Elies Roman, 
# written on: 24-02-2024
# purpose: import dataset modified 
# description: reads data of dutch housing and selects random subset of observation converting busy street to binary


library(readxl)
dataa<-read_xlsx('C:/Users/maria/OneDrive/Desktop/multi/dutch_houses.xlsx')


dataa <- subset(dataa, busy_street != 1)
dataa$busy_street[dataa$busy_street== 2] <- 1

subset<-5000

t1 <- sample(x = 1:nrow(dataa), 
                       size = subset,
                       replace = FALSE)


Taula<- dataa[t1, ]
 
save(Taula, file="subset_dutch_houses.RData")

