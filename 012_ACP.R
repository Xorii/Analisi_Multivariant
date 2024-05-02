#ANÀLISI DE COMPONENTS PRINCIPALS

# 1. Definir nombre de dimensions per incloure 80% inèrcia
# 2. Generar combinacions 2 a 2 de les gràfiques
# 3. Representació d'individus (detectar outliers)
# 4. Gràfics variables numèriques 2 a 2
# 5. Gràfics amb numèriques i categòriques
# 6. Representació per grups "temàtics"

#setwd("C:/Users/Auba/OneDrive - Universitat de Barcelona/AM/00. Treball")

#importar dades i canivar el nom del dataframe
load("009_Preprocessing2.RData")
dd <- df_mice
str(dd)
objects()
#attributes(dd)


#comprovar que les dades son correctes i estan ben llegides
attach(dd)
names(dd)
sapply(dd,class)

#variables numèriques
numeriques <- which(sapply(dd,is.numeric))
numeriques

dcon <- dd[,numeriques]   #dataframe només amb les numèriques
sapply(dcon,class)

#comprovar que no hi ha missings a les variables numèriques
sum(is.na(dcon))

#variables categoriques
categoriques <- names(dd)[-numeriques]


# PRINCIPAL COMPONENT ANALYSIS OF dcon

pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)

print(pc1)

str(pc1)

# [1] Definir nombre de dimensions per incloure 80% inèrcia
# WHICH PERCENTAGE OF THE TOTAL INERTIA IS REPRESENTED IN SUBSPACES?
pc1$sdev
inerProj <- pc1$sdev^2 
inerProj
totalIner <- sum(inerProj)
totalIner
pinerEix <- 100*inerProj/totalIner
pinerEix
barplot(pinerEix, ylim = c(0,50))

# Inèrcia acumulada
barplot(100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2])
percInerAccum <- 100*cumsum(pc1$sdev[1:dim(dcon)[2]]^2)/dim(dcon)[2]
percInerAccum
#per incloure un 80% de la inèrcia -> 7 dimensions

# SELECTION OF THE SINGIFICNT DIMENSIONS (keep 80% of total inertia)
nd <- 7

print(pc1)
attributes(pc1)
pc1$rotation

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
#View(pc1$x)
dim(pc1$x)
dim(dcon)
dcon[2000,]
pc1$x[2000,]

Psi <- pc1$x[,1:nd]
dim(Psi)
Psi[2000,]

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden <- row.names(dcon)
etiq <- names(dcon)
ze <- rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS



# GRÀFICS

# [2] Generar combinacions 2 a 2 dels gràfics
plot.psi <- function(Psi, eje1, eje2, dir) {
  png(filename= paste0(dir,"plot_psi",eje1,"-",eje2, ".png"))

  plot(Psi[,eje1],Psi[,eje2], type="n")
  text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  #library(rgl)
  #plot3d(Psi[,1],Psi[,2],Psi[,3])
  
  title(paste(eje1, "-", eje2, sep = " "))
  
  graphics.off()
}

directori <- "C:/Users/Auba/OneDrive - Universitat de Barcelona/AM/00. Treball/grafics/"

for (i in 1:ncol(Psi)) {
  for (j in i:ncol(Psi)) {
    if (i!=j)
      plot.psi(Psi, i, j, directori)
  }
}



# [3¿?] Representació d'individus (detectar outliers)
#Projecció variables
Phi <- cor(dcon,Psi)
View(Phi)

# difícil veure outliers en aquests gràfics



# [4] Gràfics variables numèriques 2 a 2
# GRÀFICS NUMÈRIQUES

plot.num <- function(Psi, eje1, eje2, dir) {
  png(filename= paste0(dir,"plot_num",eje1,"-",eje2,".png"))
  
  X <- Phi[,eje1]
  Y <- Phi[,eje2]

  plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
  axis(side=1, pos= 0, labels = F)
  axis(side=3, pos= 0, labels = F)
  axis(side=2, pos= 0, labels = F)
  axis(side=4, pos= 0, labels = F)
  arrows(ze, ze, X, Y, length = 0.07,col="blue")
  text(X,Y,labels=etiq,col="darkblue", cex=0.7)
  
  title(paste(eje1, "-", eje2, sep = " "))
  
  graphics.off()
}


for (i in 1:ncol(Psi)) {
  for (j in i:ncol(Psi)) {
    if (i!=j)
      plot.num(Psi, i, j, directori)
  }
}


# [5] Gràfics amb numèriques i categòriques

#variables categòriques
dcat <- which(sapply(dd,is.factor))




colors <- rainbow(length(dcat))

plot.tot <- function(Psi, eje1, eje2, dir) {
  jpeg(filename = paste0(dir,"plot_tot",eje1,"-",eje2,".png"), 
       width = 7, height = 7, units = 'in', res = 300)
  
  X <- Phi[,eje1]
  Y <- Phi[,eje2]
  
  #represent numerical variables in background
  plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1.5,2), ylim=c(-1,1))
  #plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  
  #add projections of numerical variables in background
  arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
  text(X,Y,labels=etiq,col="gray", cex=0.7)
  
  #add centroids
  c <- 1
  for(k in dcat){
    seguentColor<-colors[c]
    
    fdic1 = tapply(Psi[,eje1],dd[,k],mean)
    fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
    
    #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
    text(fdic1,fdic2,labels=levels(factor(dd[,k])),col=seguentColor, cex=0.6)
    c<-c+1
  }
  
  legend("bottomleft", names(dd)[dcat], pch=1, col=colors, cex=0.6)
  title(paste(eje1, "-", eje2, sep = " "))
  
  graphics.off()
}


for (i in 1:ncol(Psi)) {
  for (j in i:ncol(Psi)) {
    if (i!=j)
      plot.tot(Psi, i, j, directori)
  }
}



#-------------------
# veure gràfics a R (no guardar-los)
plot.tot2 <- function(Psi, eje1, eje2) {
  #png(filename = paste0(dir,"plot_tot",eje1,"-",eje2,".png"))
  
  X <- Phi[,eje1]
  Y <- Phi[,eje2]
  
  #represent numerical variables in background
  plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(-1.5,2), ylim=c(-1,1))
  #plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  
  #add projections of numerical variables in background
  arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
  text(X,Y,labels=etiq,col="gray", cex=0.7)
  
  #add centroids
  c <- 1
  for(k in dcat){
    seguentColor<-colors[c]
    
    fdic1 = tapply(Psi[,eje1],dd[,k],mean)
    fdic2 = tapply(Psi[,eje2],dd[,k],mean) 
    
    #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(dd[,k]))
    text(fdic1,fdic2,labels=levels(factor(dd[,k])),col=seguentColor, cex=0.6)
    c<-c+1
  }
  
  legend("bottomleft", names(dd)[dcat], pch=1, col=colors, cex=0.6)
  title(paste(eje1, "-", eje2, sep = " "))
  #graphics.off()
}


for (i in 1:ncol(Psi)) {
  for (j in i:ncol(Psi)) {
    if (i!=j)
      plot.tot2(Psi, i, j)
  }
}
