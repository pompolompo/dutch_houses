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
## Modifiquem la variable city, degut a que les etiquetes eren massa llargues 
##i al representar-les es sobreposaven entre elles. Així doncs, per tal de poder 
##visualitzar millor els gràfics, hem numerat les ciutats de l'1 al 4, mantenint-la com a factor.

tbl_houses_subsetACM<-tbl_houses_subset[-15]
categories=c("Amsterdam","Rotterdam","The Hague","Utrecht")
valors_numerics=c("1","2","3","4")
tbl_houses_subsetACM$city<-factor(tbl_houses_subset$city, levels=categories, labels=valors_numerics)
#canviar noms variables més curts!!!!!

# Ens quedem amb les variables categóriques:
tipo <- sapply(tbl_houses_subsetACM, class)
varCat <- names(tipo)[which(tipo %in% c("factor", "character"))]

#Realitzem un gráfic de barres per a cada una de les variables categóriques.
for (ca in varCat) {
  plot(tbl_houses_subsetACM[, ca], main = ca, ylab = "Count", col = "steelblue", 
       las = 2)
}

# ==============================================================================
# Análisi de Correspondencies Multiples (ACM)
res.mca <- MCA(tbl_houses_subsetACM, quanti.sup = c(5:7,12,13,16), quali.sup = c(10,11), graph = FALSE)
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
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 5))

#Obsevem el percentatge de variabilitat explicada per a cadascuna de les dimensions que més l'expliquen.
#La primera dimensió explica un 5% de la variabilitat, la segona un 4.3%, la tercera un 3.9% i les 
#següents dimensions presenten poca diferència entre elles pel que fa la variabilitat 
#explicada, decreixent entre cada dimensió com a molt un 0.1%.

# ------------------------------------------------------------------------------
## Gráfic de les variables, representades en la dimensió 1 i 2.
#Els triangles vermells corresponen a les diferents categories de les variables.
fviz_mca_var(res.mca,
                repel = TRUE, # evita la superposicion de texto
                ggtheme = theme_minimal())

#A continuació realitzem un zoom per a poder veure representades les categories 
#de les variables suplementàries,que corresponen a les variables month i year,  
#representades com a triangles verds.

fviz_mca_var(res.mca,
             repel = TRUE, # evita la superposicion de texto
             ggtheme = theme_minimal(), ylim=c(-0.25,0.25), xlim=c(-0.25,0.25))

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
#### Amb la realització d'aquest gràfic podem representar la correlació entre 
#les variables i les dimensions principals, en aquest cas la 1 i la 2, que 
#expliquen un 4.8% i un 4.5% de la variabilitat respectivament. 
#Aquestes correlacions són utilitzades com a coordenades. 


# Calidad de la representaciÃ³n de las categorias de las variables:
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
#A partir d'aqeust gràfic, en el que es pot veure el grau d'associació entre les cateogries
# de les variables, podem observar que les que es troben millor representades en les dimensions 1 i 2 són 
# apartment_0 i aprtment_1, trobant-se la gran majoria de les variables restants en valors de cos2 inferiors a 0.2.


### Posem la mateixa informació però en la representació actual les poc representades tenen el triangle de color clar, les conclusions són les mateixes que anteriorment.
fviz_mca_var(res.mca, alpha.var = "cos2",
             repel = TRUE,
             ggtheme = theme_minimal())

### Podemos visualizar la medida del coseno2 a travÃ©s de un correlograma
corrplot(var$cos2, is.corr = FALSE)
#Per poder observar més precisament les correlacions de les variables en cada dimensió, procedim a la realització d'un grafic de contribucions, 
#en que la linea vermella representaria la contribució mitjana si la distrbució fos uniforme

#DIMENSIÓ 1:
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
#En la dimensió 1 les variables més correlacionades són apartment_0, apartment_1, type_5, construction_period_9 i energy_label_A . 

#DIMENSIÓ 2:
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)
#En la dimensió 2 les variables més correlacionades són apartment_0 i type_2, i amb una contribució d'aproximadament el 6% tenim les variables type_5 i apartment_1.

#DIMENSIÓ 3:
fviz_contrib(res.mca, choice = "var", axes = 3, top = 15)
#En la dimsenió 3 la variable més correlacionada és interior_condition_9 contribuïnt en un 12%, seguida de exterior_condition_9, exterior_condition_8, interior_condition_8 i city_3.

#DIMENSIÓ 4:
fviz_contrib(res.mca, choice = "var", axes = 4, top = 15)
#En la dimensió 4 les variables més correlacionades són interior_condition_1 i exterior_condition_1, contrïbuint cada una d'elles en un 30%. La resta de variables en canvi contribueixen de forma poc significativa, trobant-se la diferència entre la 3r variable que més contribueix i exterior_condition_1 més d'un 25% de contribució.

#DIMENSIÓ 5:
fviz_contrib(res.mca, choice = "var", axes = 5, top = 15)
###########CANVIAR !!!!!!!!
#En la dimensió 5 no hi ha variables amb alta correlació en la dimensió, només estarien considerablament correlacionades les variables interior_condition_8, exterior_condition_8, interior_condition_9 i exterior_condition_9.

### Tambe podemos visualizar el coseno en los ejes de las dos variables
fviz_cos2(res.mca, choice = "var", axes = 1:2) #qualitat de la representació de les variables en les dimensions 1 i 2.
fviz_cos2(res.mca, choice = "var", axes = 2:3) #qualitat de la representació de les variables en les dimensions 2 i 3.
fviz_cos2(res.mca, choice = "var", axes = 3:4) #qualitat de la representació de les variables en les dimensions 3 i 4.
fviz_cos2(res.mca, choice = "var", axes = 4:5) #qualitat de la representació de les variables en les dimensions 4 i 5.
fviz_cos2(res.mca, choice = "var", axes = 1:3) #qualitat de la representació de les variables en les dimensions 1 i 3.
fviz_cos2(res.mca, choice = "var", axes = 1:4) #qualitat de la representació de les variables en les dimensions 1 i 4.
fviz_cos2(res.mca, choice = "var", axes = 1:5) #qualitat de la representació de les variables en les dimensions 1 i 5.
fviz_cos2(res.mca, choice = "var", axes = 2:4) #qualitat de la representació de les variables en les dimensions 2 i 4.
fviz_cos2(res.mca, choice = "var", axes = 2:5) #qualitat de la representació de les variables en les dimensions 2 i 5.
fviz_cos2(res.mca, choice = "var", axes = 3:5) #qualitat de la representació de les variables en les dimensions 3 i 5.


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
# DescripciÃ³n de la dimensiÃ³n
## Correlacion de varaibles con las dimensiones
res.desc <- dimdesc(res.mca, axes = c(1,2))

### Descripcion de dimension 1
res.desc[[1]]

### Descripcion de dimension 2
res.desc[[2]]