# Metadata ----------------------------------------------------------------
# written by: Joan Ruiz & Mar Bellet
# written on: 31-03-2024
# purpose: profiling
# description: anàlisi de les variables de la base de dades pels diferents 
#              grups obtinguts en el clustering


# Anem a utilitzar un clustering de k-means amb 6 grups per fer profiling


tbl_houses_subset<-tbl_houses_subset[,c(1:14,16:17)]
numeriques <- tbl_houses_subset[ , c(5,6,7,12,13,16)]
categoriques<-tbl_houses_subset[,c(1,2,3,4,8,9,10,11,14,15)]

k <- 6  # nombre òptim de clústers
set.seed(12345)
clust_kmeans <- kmeans(numeriques, centers = k)
tbl_houses_subset$cluster_group <- clust_kmeans$cluster #afegim la variable grup a la base de dades
tbl_houses_subset$cluster_group <- as.factor(tbl_houses_subset$cluster_group)



## PREPROFILING


library(cluster)
distances <- list(
  gower = daisy(tbl_houses_subset, metric = "gower", stand = TRUE)**2
)

h <- hclust(
  d = distances[["gower"]],
  method = "ward.D2"
)

tbl_houses_subset$cluster_group <- as.character(cutree(
  tree = h,
  
  k = 6
  
))

library(FactoMineR)
plot(FactoMineR::catdes(tbl_houses_subset[,c(1,2,8,9,17)], num.var = 5))
#s'han de retocar les variables del plot i el num.var....


### PROFILING
#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum,P){
  #freq dis of fac
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  #mitjanes x grups
  xk <- tapply(Xnum,P,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); 
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}


ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=TRUE);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  #i hi ha divisions iguals a 0 dona NA i no funciona
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0]; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}

#P must contain the class variable
P<-tbl_houses_subset[,17] #variable grups de clustering
nameP<-"classe"
nc<-length(levels(factor(P)))
nc

K<-dim(tbl_houses_subset)[2]
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(tbl_houses_subset)))
#hem creat una matriu de 0s que omplirem a continuació, on principalment 
#hi hauran els p-valors 

n<-dim(tbl_houses_subset)[1]

dd <- tbl_houses_subset

for(k in 1:(K-1)){
  if (is.numeric(tbl_houses_subset[,k])){ 
    print(paste("Anàlisi per classes de la Variable:", names(tbl_houses_subset)[k]))
    
    boxplot(tbl_houses_subset[,k]~P, main=paste("Boxplot of", names(tbl_houses_subset)[k], "vs", nameP ), horizontal=TRUE)
    
    barplot(tapply(tbl_houses_subset[[k]], P, mean),main=paste("Means of", names(tbl_houses_subset)[k], "by", nameP ))
    abline(h=mean(tbl_houses_subset[[k]]))
    legend(0,mean(tbl_houses_subset[[k]]),"global mean",bty="n")
    print("Estadístics per groups:")
    for(s in levels(as.factor(P))) {print(summary(tbl_houses_subset[P==s,k]))}
    o<-oneway.test(tbl_houses_subset[,k]~P)
    print(paste("p-valueANOVA:", o$p.value))
    kw<-kruskal.test(tbl_houses_subset[,k]~P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
    pvalk[,k]<-ValorTestXnum(tbl_houses_subset[,k], P)
    print("p-values ValorsTest: ")
    print(pvalk[,k])      
  }else{
    if(class(dd[,k])=="Date"){
      print(summary(dd[,k]))
      print(sd(dd[,k]))
      #decide breaks: weeks, months, quarters...
      hist(dd[,k],breaks="weeks")
    }else{
      #qualitatives
      print(paste("Variable", names(tbl_houses_subset)[k]))
      table<-table(P,tbl_houses_subset[,k])
      print("Cross-table")
      print(table)
      rowperc<-prop.table(table,1)
      
      colperc<-prop.table(table,2)
      
      tbl_houses_subset[,k]<-as.factor(tbl_houses_subset[,k])
      
      
      marg <- table(as.factor(P))/n
      print(append("Categories=",levels(as.factor(tbl_houses_subset[,k]))))
      
      #from next plots, select one of them according to your practical case
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(tbl_houses_subset)[k]))
      paleta<-rainbow(length(levels(tbl_houses_subset[,k])))
      for(c in 1:length(levels(tbl_houses_subset[,k]))){lines(colperc[,c],col=paleta[c]) }
      
      #with legend
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(tbl_houses_subset)[k]))
      paleta<-rainbow(length(levels(tbl_houses_subset[,k])))
      for(c in 1:length(levels(tbl_houses_subset[,k]))){lines(colperc[,c],col=paleta[c]) }
      legend("topright", levels(tbl_houses_subset[,k]), col=paleta, lty=2, cex=0.6)
      
      #condicionades a classes
      print(append("Categories=",levels(tbl_houses_subset[,k])))
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(tbl_houses_subset)[k]))
      paleta<-rainbow(length(levels(tbl_houses_subset[,k])))
      for(c in 1:length(levels(tbl_houses_subset[,k]))){lines(rowperc[,c],col=paleta[c]) }
      
      #with legend
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(tbl_houses_subset)[k]))
      paleta<-rainbow(length(levels(tbl_houses_subset[,k])))
      for(c in 1:length(levels(tbl_houses_subset[,k]))){lines(rowperc[,c],col=paleta[c]) }
      legend("topright", levels(tbl_houses_subset[,k]), col=paleta, lty=2, cex=0.6)
      
      #amb variable en eix d'abcisses
      marg <-table(tbl_houses_subset[,k])/n
      print(append("Categories=",levels(tbl_houses_subset[,k])))
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(tbl_houses_subset)[k]), las=3)
      #x<-plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(tbl_houses_subset)[k]), xaxt="n")
      #text(x=x+.25, y=-1, adj=1, levels(CountryName), xpd=TRUE, srt=25, cex=0.7)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c]) }
      
      #with legend
      plot(marg,type="l",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(tbl_houses_subset)[k]), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(rowperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
      
      #condicionades a columna 
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(tbl_houses_subset)[k]), las=3)
      paleta<-rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c]) }
      
      #with legend
      plot(marg,type="n",ylim=c(0,1),main=paste("Prop. of pos & neg by",names(tbl_houses_subset)[k]), las=3)
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c,],col=paleta[c])}
      legend("topright", levels(as.factor(P)), col=paleta, lty=2, cex=0.6)
      
      table<-table(tbl_houses_subset[,k],P)
      print("Cross Table:")
      print(table)
      print("Distribucions condicionades a columnes:")
      print(colperc)
      
      #diagrames de barres apilades                                         
      
      paleta<-rainbow(length(levels(tbl_houses_subset[,k])))
      
      barplot(table(tbl_houses_subset[,k], as.factor(P)), beside=FALSE,col=paleta , main =paste("Diagrama de barres apliades de ",names(tbl_houses_subset)[k]) )
      legend("topright",levels(as.factor(tbl_houses_subset[,k])),pch=1,cex=0.5, col=paleta)
      
      
      #diagrames de barres adosades
      barplot(table(tbl_houses_subset[,k], as.factor(P)), beside=TRUE,col=paleta, main =paste("Diagrama de barres adosades de ",names(tbl_houses_subset)[k] ))
      legend("topright",levels(as.factor(tbl_houses_subset[,k])),pch=1,cex=0.5, col=paleta)
      
      print("Test Chi quadrat: ")
      print(chisq.test(tbl_houses_subset[,k], as.factor(P)))
      
      print("valorsTest:")
      print( ValorTestXquali(P,tbl_houses_subset[,k]))
      #calcular els pvalues de les quali
    }
  }
}


#descriptors de les classes més significatius. Afegir info qualits
for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:",levels(as.factor(P))[c]));
    print(sort(pvalk[c,]), digits=3) 
  }
}





#Gràfics algo mes senzills però entendors

for (j in c(5,6,7,12,13,16)){
  boxplot(tbl_houses_subset[,j] ~ tbl_houses_subset[,17],col = rainbow(6),xlab = "Grups",ylab = paste("Valors de",names(tbl_houses_subset)[j]))
  title(main=paste("boxplot de la variable",names(tbl_houses_subset)[j],"per grups"))
  abline(h=mean(tbl_houses_subset[[j]]))
}

df<-tbl_houses_subset


