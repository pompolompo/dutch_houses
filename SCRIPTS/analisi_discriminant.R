# ==============================================================================
# Carreguem les llibreries necessaries
list.of.packages <-c("caret", "MASS", "klaR", "ggplot2", "ggpubr") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)


# ------------------------------------------------------------------------------
# Dividim les dades: 80% entrenament i 20% test

variable_resposta<-"cluster_group"

##### eliminem les variablea amb variancia 0:

tbl_houses_subset<-tbl_houses_subset[,-28]
num<-c(2,3,4,5,7)
cat<-c(1,6,9:64)

## Declarem la semilla
set.seed(6789)

muestra <- caret::createDataPartition(y = tbl_houses_subset[,variable_resposta], p = 0.8, list = FALSE)
train <- tbl_houses_subset[muestra, ]
test <- tbl_houses_subset[-muestra, ]

# Estimació dels paràmetres de preprocessament
preproc_param <- caret::preProcess(x = train, method = c("center", "scale"))

# Transformem les dades segons el que s'ha establert en els paràmetres anteriors
train <- preproc_param |> predict(train)
test <- preproc_param |> predict(test)

# ==============================================================================
# Podem visualitzar les dades per poder detectar variables classificadores que 
# puguin contribuir a la discriminació dels grups


#Variables numèriques

p1 <- ggplot(data = train, aes(x = parcel_size, fill = !!sym(variable_resposta), colour =!!sym(variable_resposta))) +
  geom_density(alpha = 0.3) +
  theme_bw()


p2 <- ggplot(data = train, aes(x = floor_area, fill = !!sym(variable_resposta), colour =!!sym(variable_resposta))) +
  geom_density(alpha = 0.3) +
  theme_bw()

p3 <- ggplot(data = train, aes(x = rooms, fill = !!sym(variable_resposta), colour =!!sym(variable_resposta))) +
  geom_density(alpha = 0.3) +
  theme_bw()

p4 <- ggplot(data = train, aes(x = sale_price, fill = !!sym(variable_resposta), colour =!!sym(variable_resposta))) +
  geom_density(alpha = 0.3) +
  theme_bw()

p5 <- ggplot(data = train, aes(x = price_metre, fill = !!sym(variable_resposta), colour =!!sym(variable_resposta))) +
  geom_density(alpha = 0.3) +
  theme_bw()

ggarrange(p1, p2, p3, p4,p5, ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom")


#variables categoriques

p1 <- ggplot(data = train, aes(x = Amsterdam, fill = !!sym(variable_resposta), colour =!!sym(variable_resposta))) +
  geom_density(alpha = 0.3) +
  theme_bw()

# ------------------------------------------------------------------------------
# També ho podem veure visualitzant les gràfiques de punts per veure distàncies als
# centroides


png("plot.png")
pairs(x = train[, num], col = c("firebrick", "green3", "darkblue", "pink", "orange", "yellow")[train[, variable_resposta]], pch = 20)
#com que tenim moltes variables aquest gràfic no s'acaba de veure bé
dev.off()

city<-c(9:12)
cons_per<-c(28:36)
#fem aquest grafic per cada grup de categoriques
pairs(x = train[, city], col = c("firebrick", "green3", "darkblue", "pink", "orange", "yellow")[train[, variable_resposta]], pch = 20)


options(digits = 4)
modelo_lda <- lda(cluster_group ~ ., data = train)
modelo_lda


## fem la projecció dels individus en el pla format per dos funcions discriminants (fem un gràfic per a cada parella)
datos_lda <- cbind(train, predict(modelo_lda)$x)

#GRÀFIC LD1 I LD2

ggplot(datos_lda, aes(LD1, LD2)) +
  geom_point(aes(color = !!sym(variable_resposta))) +
  ggtitle("Gráfico LDA")

#GRÀFIC LD1 I LD3

ggplot(datos_lda, aes(LD1, LD3)) +
  geom_point(aes(color = !!sym(variable_resposta))) +
  ggtitle("Gráfico LDA")

#GRÀFIC LD1 I LD4

ggplot(datos_lda, aes(LD1, LD4)) +
  geom_point(aes(color = !!sym(variable_resposta))) +
  ggtitle("Gráfico LDA")

#GRÀFIC LD1 I LD5

ggplot(datos_lda, aes(LD1, LD5)) +
  geom_point(aes(color = !!sym(variable_resposta))) +
  ggtitle("Gráfico LDA")

#GRÀFIC LD2 I LD3

ggplot(datos_lda, aes(LD2, LD3)) +
  geom_point(aes(color = !!sym(variable_resposta))) +
  ggtitle("Gráfico LDA")

#GRÀFIC LD2 I LD4

ggplot(datos_lda, aes(LD2, LD4)) +
  geom_point(aes(color = !!sym(variable_resposta))) +
  ggtitle("Gráfico LDA")

#GRÀFIC LD2 I LD5

ggplot(datos_lda, aes(LD2, LD5)) +
  geom_point(aes(color = !!sym(variable_resposta))) +
  ggtitle("Gráfico LDA")

#GRÀFIC LD3 I LD4

ggplot(datos_lda, aes(LD3, LD4)) +
  geom_point(aes(color = !!sym(variable_resposta))) +
  ggtitle("Gráfico LDA")

#GRÀFIC LD3 I LD5

ggplot(datos_lda, aes(LD3, LD5)) +
  geom_point(aes(color = !!sym(variable_resposta))) +
  ggtitle("Gráfico LDA")

#GRÀFIC LD4 I LD5

ggplot(datos_lda, aes(LD4, LD5)) +
  geom_point(aes(color = !!sym(variable_resposta))) +
  ggtitle("Gráfico LDA")


## Por último, mediante la función partimat() del paquete klaR, se puede visualizar 
## cómo quedan las regiones bivariantes que clasifican los individuos en cada clase 


klaR::partimat(train[,"cluster_group"] ~ ., data = train[,num], method = "lda", 
               image.colors = c("skyblue", "lightgrey", "yellow","pink","orange","green"), col.mean = "red")



klaR::partimat(train[,"cluster_group"] ~ ., data = train[,10:12], method = "lda", 
               image.colors = c("skyblue", "lightgrey", "yellow","pink","orange","green"), col.mean = "red")



## Por último, aplicando las funciones discriminantes a los datos reservados para 
## estudiar la capacidad predictiva del modelo, se obtiene la tabla conocida como 
## matriz de confusión, donde se compara el grupo real con el pronosticado por el 
## modelo:

predicciones_lda <- modelo_lda |> predict(test)
table(test$cluster_group, predicciones_lda$class, dnn = c("Grupo real", "Grupo pronosticado"))
mean(predicciones_lda$class == test$cluster_group)


