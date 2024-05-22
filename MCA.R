##############################################
###   ANÃ€LISIS DE COMPONENTS MÃšLTIPLES     ###
###                 (ACM)                  ###
##############################################

# Cargamos las librerias necesarias
library(FactoMineR)
library(Matrix)
library(factoextra)
library(corrplot)

# Base de datos
## Vamos a utilizar los datos â€œpoisonâ€ del paquete FactoMineR, que es una encuesta 
## a niÃ±os de primaria que tuvieron intoxicaciÃ³n por alimentos, informaciÃ³n de 
## los sintomas y comida. Estos datos poseen informaciÃ³n de 55 niÃ±os, asÃ­ como 
## tambiÃ©n algunas 15 variables.
data(poison)
head(poison)
str(poison)

# Nos quedamos con las variables categoricas
tipo <- sapply(poison, class)
varCat <- names(tipo)[which(tipo %in% c("factor", "character"))]

## [1] "Enfermo"       "Sexo"          "NÃ¡useas"       "VÃ³mitos"       "Abdominales"
## [6] "Fiebre"        "Diarrea"       "Patata"        "Pescado"       "Mayonesa"
## [11] "CalabacÃ­n"    "Queso"         "Helado"

for (ca in varCat) {
  plot(poison[, ca], main = ca, ylab = "Count", col = "steelblue", 
       las = 2)
}

# ==============================================================================
# AnÃ¡lisi de Correspondencies Multiples (ACM)
res.mca <- MCA(poison, quanti.sup = c(1, 2), quali.sup = c(3, 10:15), graph = FALSE)
## Visualizamos los objetos que encontramos en el ACM
res.mca

# ------------------------------------------------------------------------------
# Visualizar los resultados
## Extrae los valores propios y varianzas
eig.val <- get_eigenvalue(res.mca)
head(eig.val)

## Visualizar porcentajes de inercia por cada dimension
### Para determinar el numero de componentes principales se puede mirar un Scree Plot, 
### que es un plot de los eigenvalues ordenados de mayor a menor. 
### El nÃºmero de componentes es determinado en el punto mas allÃ¡ del cual los 
### valores propios (egeinvalues) restantes son todos relativamente pequeÃ±os y 
### de tamaÃ±o comparable.
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))


# ------------------------------------------------------------------------------
## Biplot de individuos y categorias (similitud)
### Luego, hacemos un diagrama simetrico:
### individuos: puntos azules
### categorÃ­as: triÃ¡ngulos rojos. 
fviz_mca_biplot(res.mca,
                repel = TRUE, # evita la superposicion de texto
                ggtheme = theme_minimal())

# ==============================================================================
# AnÃ lisi del ACM obtenido
## Variables
var <- get_mca_var(res.mca)
var
### Coordenadas en las dimensiones
head(var$coord)
### Calidad de representacion (Cos2)
### CuÃ¡nto de la variabilidad de los datos originales se captura en el espacio 
### dimensional creado por el MCA. Una alta calidad de representaciÃ³n significa 
### que los puntos en este espacio representan adecuadamente las relaciones entre 
### las categorÃ­as de las variables originales. El valor va de 0 a 1
head(var$cos2)
### Contribuciones a las dimensiones
### Cuanto contribuye cada variable a la generaciÃ³n de los ejes
head(var$contrib)

### Se puede representar visualmente de la siguiente manera: 
fviz_mca_var(res.mca, choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_minimal())
#### Esta grÃ¡fica ayuda a identificar las variables que estÃ¡n mÃ¡s correlacionadas con cada dimensiÃ³n. 
#### Las correlaciones cuadradas entre variables y las dimensiones son usadas como coordenadas. 

### VisualizaciÃ³n sÃ³lo de modalidades de las variables
fviz_mca_var(res.mca,
             repel = TRUE, 
             ggtheme = theme_minimal())

# Calidad de la representaciÃ³n de las categorias de las variables
## La calidad de la representaciÃ³n se llama el coseno cuadrado (Cos2), el cual 
## mide el grado de asociaciÃ³n entre las categorÃ­as de las variables y un eje en 
## particular. Si la categorÃ­a de una variable estÃ¡ bien representada por dos 
## dimensiones, la suma del cos2 es cercana a uno. Para algunos Ã­tems de las filas, 
## mÃ¡s de dos dimensiones son requeridas para represetar perfectamente los datos. 
head(var$cos2)

### Grado de asociacion entre las categorias de las variables
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal())

### Podemos realizar la misma informaciÃ³n pero poniendo transparente aquellas etiquetas
### que se representan de mala forma
fviz_mca_var(res.mca, alpha.var = "cos2",
             repel = TRUE,
             ggtheme = theme_minimal())

### Podemos visualizar la medida del coseno2 a travÃ©s de un correlograma
corrplot(var$cos2, is.corr = FALSE)

### TambÃ­en podemos visualizar el coseno en los ejes de las dos variables
fviz_cos2(res.mca, choice = "var", axes = 1:2)

### Podemos visualizar la contribuciÃ³n en el 1r eje
#### La linea roja indica el valor promedio esperado si las contribuciones fueran uniformes.
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)

### Incluso podemos visualizar la dispersiÃ³n del cos2 segun las modalidades
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal())

# ==============================================================================
# Individuos
ind <- get_mca_ind(res.mca)
ind

## Coordenadas
head(ind$coord)

## calidad de representacion
head(ind$cos2)

## Contribuciones
head(ind$contrib)

## contribucion y calidad de representacion
fviz_mca_ind(res.mca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal())

## Cos2 de individuos
fviz_cos2(res.mca, choice = "ind", axes = 1:2, top = 20)
## contribucion de individuos a las variables
fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 20)

## Podemos agrupar los individuos
### El siguiente cÃ³digo agrupa los individuos por colores utilizando los niveles 
### de la variable de elecciÃ³n
### El argumento habillage se usa para especificar el factor de la variable para agrupar 
### los individuos por color. Se agrega tambiÃ©n un elipse de concentraciÃ³n alrededor 
### de cada grupo usando el argumento addEllipses = TRUE.
fviz_mca_ind(res.mca,
             label = "none", 
             habillage = "Vomiting", # color por grupos
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence", #elipse de concentracion  y punto medio
             ggtheme = theme_minimal())

fviz_ellipses(res.mca, c("Vomiting", "Fever"),
              geom = "point")

fviz_ellipses(res.mca, 1:4, geom = "point")

# ==============================================================================
# Descripcion de la dimensiÃ³n
## Correlacion de varaibles con las dimensiones
res.desc <- dimdesc(res.mca, axes = c(1,2)) #es com fer el profiling pero del acm

### Descripcion de dimension 1
res.desc[[1]]

### Descripcion de dimension 2
res.desc[[2]]