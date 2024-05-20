
# PCA ----------------------------------------------------------------
# written by: Bernat Padrosa, @bernat16
# written on: 25-04-2024
# purpose: do PCA

tbl_houses_subset$temps_mercat<- as.factor(tbl_houses_subset$temps_mercat)

objects()
attributes(tbl_houses_subset)

attach(tbl_houses_subset)
names(tbl_houses_subset)

sapply(tbl_houses_subset,class)

#set a list of numerical variables (with no missing values)
numeriques<-which(sapply(tbl_houses_subset,is.numeric))
numeriques

dcon<-tbl_houses_subset[,numeriques]
sapply(dcon,class)

### PRINCIPAL COMPONENT ANALYSIS
pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)

print(pc1)
str(pc1)

# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
pc1$sdev
inerProj<- pc1$sdev^2 
inerProj
totalIner<- sum(inerProj)
totalIner
pinerEix<- 100*inerProj/totalIner
pinerEix
barplot(pinerEix)
#les 2 primeres dimensions son les que mes ens aporten, les dos ultimes no aporten 
#casi res de informacio

### CALCULEM LA ACUMULACIÓ D'INÈRCIA:
#Cummulated Inertia in subspaces, from first principal component to the 11th dimension subspace
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2], col="lightgreen",
        main = "Percentatge acumulat d'inèrcia",names.arg = c("dim1", "dim2", "dim3","dim4","dim5","dim6"))
abline(h=80,col="red")
percInerAccum<-100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
percInerAccum
# Ens quedem amb 3 dimensions significatives, perquè acumulen el 80% de la inèrcia


### CRITERI PER SABER QUAN ENS QUEDEM:
# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)

nd = 3 #numero de dimensions que volem seleccionar

print(pc1) 
attributes(pc1)
pc1$rotation

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
View(pc1$x)
dim(pc1$x) #variables son 11 (numeriques)
dim(dcon)
dcon[2000,] #varaibles numeriques del registre 2000
pc1$x[2000,] #valor que obte en cada una de les dimensions, valr obtingut de les combinacions lineals de les variables originals

#psi guardem les projeccions dels individuus en el nou subespai
Psi = pc1$x[,1:nd]
dim(Psi)
Psi[2000,]

### STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(dcon) #nom de les variables (etiqueta identificativa --> si no hi ha agafem directament el rownmaes)
etiq = names(dcon)
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS
#ze fem vector de 0 i guardarem els vectors propis 

### PLOT OF INDIVIDUALS

#select your axis
eje1<-1 #primera dimensio
eje2<-2 #segona dimensio
eje3<-3 #tercera dimensió

#haurem de mirar quantes dimensions son significatives i fer totes les combinacions significatives 2 a 2.

plot(Psi[,eje1],Psi[,eje2],xlab="dimensió 1",ylab="dimensió 2") #com es distrbueix els individus en primera i segona dimensio
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
# Representació dels individus en les dimensions 1 i 2. Veiem alguns outliers. 


plot(Psi[,eje1],Psi[,eje2], xlab="dimensió 1",ylab="dimensió 2",type="n")
text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="red")
axis(side=3, pos= 0, labels = F, col="red")
axis(side=2, pos= 0, labels = F, col="red")
axis(side=4, pos= 0, labels = F, col="red")

#Projection of variables
Phi = cor(dcon,Psi)
View(Phi)

## COMPARACIÓ DOS A DOS DIMENSIÓ 1 AMB DIMENSIÓ 2:
X<-Phi[,eje1]
Y<-Phi[,eje2]
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)

## COMPARACIÓ DOS A DOS DIMENSIÓ 1 AMB DIMENSIÓ 3:
X<-Phi[,eje1]
Y<-Phi[,eje3]
plot(Psi[,eje1],Psi[,eje3],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)

## COMPARACIÓ DOS A DOS DIMENSIÓ 2 AMB DIMENSIÓ 3:
X<-Phi[,eje2]
Y<-Phi[,eje3]
plot(Psi[,eje2],Psi[,eje3],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)


### PROJECCIÓ DELS CLÚSTERS
#varcat=factor(tbl_houses_subset[,16],labels = c("1","2","3","4","5","6","7")) 
#plot(Psi[,1],Psi[,2],col=varcat)
#axis(side=1, pos= 0, labels = F, col="darkgray")
#axis(side=3, pos= 0, labels = F, col="darkgray")
#axis(side=2, pos= 0, labels = F, col="darkgray")
#axis(side=4, pos= 0, labels = F, col="darkgray")
#legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2), cex=0.6)


### PROJECCIÓ VARIABLES QUALITATIVES: Agafarem el nuvol de punts i pintarem 
#la variable time_on_market
# PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
# PROJECCI? OF INDIVIDUALS DIFFERENTIATING THE time_on_market

varcat=factor(tbl_houses_subset[,17]) 
plot(Psi[,1],Psi[,2],col=varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2), cex=0.6)
# eix a la dreta ja no estan. pipipi. price_metre. llavors, com més adalat de l'eix hortitzontal estiguis, el preu per metre és més alt
# va així <-- és una z. 


#select your qualitative variable
k<-18

varcat<-factor(tbl_houses_subset[,k])
fdic1 = tapply(Psi[,eje1],varcat,mean)
fdic2 = tapply(Psi[,eje2],varcat,mean) 
text(fdic1,fdic2,labels=levels(varcat),col="yellow", cex=0.7)

plot(Psi[,eje1],Psi[,eje2],type="n",xlab="dimensió 1",ylab="dimensió 2",main = "projeccions dels centroides dels clústers")
axis(side=1, pos= 0, labels = F, col="blue")
axis(side=3, pos= 0, labels = F, col="blue")
axis(side=2, pos= 0, labels = F, col="blue")
axis(side=4, pos= 0, labels = F, col="blue")

text(fdic1,fdic2,labels=levels(varcat),col="red", cex=0.7)

#all qualitative together
X<-Phi[,eje1]
Y<-Phi[,eje2]
plot(Psi[,eje1],Psi[,eje2],type="n")
axis(side=1, pos= 0, labels = F, col="black")
axis(side=3, pos= 0, labels = F, col="black")
axis(side=2, pos= 0, labels = F, col="black")
axis(side=4, pos= 0, labels = F, col="black")

#nominal qualitative variables

dcat<-c(1,2,3,4,10,14,16) #variables qualitatives interessants
colors<-rainbow(length(dcat))

c<-1
for(k in dcat){
  seguentColor<-colors[c]
  fdic1 = tapply(Psi[,eje1],tbl_houses_subset[,k],mean)
  fdic2 = tapply(Psi[,eje2],tbl_houses_subset[,k],mean)
  text(fdic1,fdic2,labels=levels(factor(tbl_houses_subset[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(tbl_houses_subset)[dcat],pch=1,col=colors, cex=0.6)
#he pintat el centroide de totes les variables que hem seleccionat

#Mateix gràfic però amb zoom:
fm<- 20
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-4,2), ylim=c(-2,3),ylab="Dimensió 2",xlab = "Dimensió 1")
axis(side=1, pos= 0, labels = F, col="black")
axis(side=3, pos= 0, labels = F, col="black")
axis(side=2, pos= 0, labels = F, col="black")
axis(side=4, pos= 0, labels = F, col="black")

#add projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="black")
text(X,Y,labels=etiq,col="black", cex=0.7)

#add centroids
c<-1
for(k in dcat){
  seguentColor<-colors[c]
  fdic1 = tapply(Psi[,eje1],tbl_houses_subset[,k],mean)
  fdic2 = tapply(Psi[,eje2],tbl_houses_subset[,k],mean) 
  text(fdic1,fdic2,labels=levels(factor(tbl_houses_subset[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("topleft",names(tbl_houses_subset)[dcat],pch=1,col=colors, cex=0.6)

### AFEGIR VARIABLE ORDINAL
dordi<- c(8)
levels(factor(tbl_houses_subset[,dordi]))
tbl_houses_subset[,dordi[1]] <- factor(tbl_houses_subset[,dordi[1]], ordered=TRUE,  levels= c("Mala ","mala/mediocre","mediocre","mediocre/estàndard","estàndard","estàndard/bé","bé","bé/excel·lent","excel·lent"))
levels(tbl_houses_subset[,dordi])

c<-1
col<-1
for(k in dordi){
  seguentColor<-colors[col]
  fdic1 = tapply(Psi[,eje1],tbl_houses_subset[,k],mean)
  fdic2 = tapply(Psi[,eje2],tbl_houses_subset[,k],mean) 
  lines(fdic1,fdic2,pch=16,col=seguentColor)
  text(fdic1,fdic2,labels=levels(tbl_houses_subset[,k]),col=seguentColor, cex=0.6)
  c<-c+1
  col<-col+1
}
legend("topleft",names(tbl_houses_subset)[dordi],pch=1,col=colors[1:length(dordi)], cex=0.6)

#using our own colors palette
# search palettes in internet. One might be https://r-charts.com/es/colores/

colors<-c("red", "blue", "darkgreen", "orange", "violet", "magenta","green","pink","yellow")

#represent numerical variables in background
plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1,1.5), ylim=c(-2,1))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#add projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
text(X,Y,labels=etiq,col="gray", cex=0.7)

#add centroids
c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eje1],tbl_houses_subset[,k],mean)
  fdic2 = tapply(Psi[,eje2],tbl_houses_subset[,k],mean) 
  text(fdic1,fdic2,labels=levels(factor(tbl_houses_subset[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomright",names(tbl_houses_subset)[dcat],pch=19,col=colors, cex=0.45)

