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


# ------------------------------------------------------------------------------
# També ho podem veure visualitzant les gràfiques de punts per veure distàncies als
# centroides
pairs(x = train[, -5], col = c("firebrick", "green3", "darkblue")[train[, variable_resposta]], pch = 20)

## Como se observa en dichos gráficos, las variables clasificadoras pueden contribuir 
## a la discriminación entre las tres especies de flores iris.
## Para aplicar la función lda() se debe especificar la variable de clasificación 
## (Species) y el conjunto de datos (entrenamiento_t); de forma opcional, se 
## pueden especificar las probabilidades a priori (prior, por defecto se usa 
## proportions), el método de estimación de las medias y varianzas (method, 
## por defecto moment) o el argumento CV para obtener los grupos pronosticados 
## y las probabilidades a posteriori (por defecto, CV=FALSE).

options(digits = 4)
modelo_lda <- lda(cluster_group ~ ., data = train)
modelo_lda

## La salida muestra las probabilidades previas (Prior probabilities of groups) 
## y los centroides de cada grupo (Group means). A continuación muestra las 
## funciones discriminantes de Fisher mediante los respectivos coeficientes w_jt
## En este caso, las dos funciones discriminantes son:
## D_1 = 0.6497·SL + 0.7416·SW - 3.9531·PL - 2.0670·PW
## D_2 = -0.1239·SL - 0.8131·SW + 2.0349·PL - 2.4184·PW
## con una proporción de discriminación de 0,9927 y 0,0073, respectivamente.

## La proyección de los individuos (en este caso flores) en el plano formado por 
## las dos funciones discriminantes:

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




## Como se aprecia, la primera función discriminante es la que mayor contribución 
## tiene a la separación entre los grupos, separando muy claramente a la especie 
## setosa y, en menor medida, a las especies virginica y versicolor, grupos entre 
## los que hay un pequeño grado de solapamiento. Por otro lado, la segunda función 
## discriminante, con una proporción de discriminación de 0,0073, apenas contribuye a 
## la separación entre grupos.

## Por último, mediante la función partimat() del paquete klaR, se puede visualizar 
## cómo quedan las regiones bivariantes que clasifican los individuos en cada clase 

klaR::partimat(cluster_group ~ ., data = train, method = "lda", 
               image.colors = c("skyblue", "lightgrey", "yellow","pink","orange","green"), col.mean = "red")

## Por último, aplicando las funciones discriminantes a los datos reservados para 
## estudiar la capacidad predictiva del modelo, se obtiene la tabla conocida como 
## matriz de confusión, donde se compara el grupo real con el pronosticado por el 
## modelo:

predicciones_lda <- modelo_lda |> predict(test)
table(test$Species, predicciones_lda$class, dnn = c("Grupo real", "Grupo pronosticado"))
mean(predicciones_lda$class == test$Species)


# ==============================================================================
# ANALISIS DISCRIMINANT CUADRÀTIC
## Para ilustrar la realización de un análisis discriminante cuadrático en R, se 
## aplica la función qda() a los datos iris utilizados en el caso lineal. 
## La elección de la misma base de datos responde a un planteamiento didáctico, 
## para poder comparar los resultados de ambos métodos y las diferencias que produce 
## asumir la igualdad de matrices de varianzas-covarianzas (método lineal) o 
## no asumirlas (método cuadrático)

options(digits = 4)
modelo_qda <- qda(Species ~ ., data = train)
modelo_qda

partimat(Species ~ ., data = train, method = "qda", image.colors = c("skyblue", "lightgrey", "yellow"), col.mean = "red")

## Como se aprecia, ahora los contornos de las áreas no son siempre lineales, sino 
## que incluyen fronteras cuadráticas. Por último, aplicando el discriminante 
## cuadrático a los datos reservados para estudiar la capacidad predictiva del 
## modelo, se obtiene la matriz de confusión, donde se observa que no se mejoran 
## los resultados respecto al discriminante lineal.

predicciones_qda <- modelo_qda |> predict(test)
matriz_confusion <- table(test$Species, predicciones_qda$class, dnn = c("Grupo real", "Grupo pronosticado"))
matriz_confusion <- reshape2::melt(matriz_confusion)
matriz_confusion <- caret::confusionMatrix(test$Species, predicciones_qda$class)




