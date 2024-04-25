#ANÀLISI DE COMPONENTS PRINCIPALS

# 1. Definir nombre de dimensions per incloure 80% inèrcia
# 2. Generar combinacions 2 a 2 de les gràfiques
# 3. Representació d'individus (detectar outliers)
# 4. Gràfics variables numèriques 2 a 2
# 5. Gràfics amb numèriques i categòriques
# 6. Representació per grups "temàtics"


#importar dades i canivar el nom del dataframe
load("009_Preprocessing2.RData")
dd <- df_mice
str(dd)
objects()
#attributes(dd)

# Visualització de les dades - ACP

# Crear dataframe variables contínues

# comprovar que les dades son correctes i estan ben llegides
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


# PRINCIPAL COMPONENT ANALYSIS OF dcon

pc1 <- prcomp(dcon, scale=TRUE)
class(pc1)
attributes(pc1)

print(pc1)

str(pc1)


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
#per incloure un 80% de la inèrcia -> 6 dimensions

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

save(Psi, file='012_PCA_7dim.RData')

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden <- row.names(dcon)
etiq <- names(dcon)
ze <- rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS



# GRÀFICS

plot.psi <- function(Psi, eje1, eje2, dir) {
  jpeg(filename= paste0(dir,"plot_psi",eje1,"-",eje2, ".jpeg"))
  plot(Psi[,eje1],Psi[,eje2], main = paste0("Psi[",i,",",j,"]"))
  text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  
  plot(Psi[,eje1],Psi[,eje2], type="n")
  text(Psi[,eje1],Psi[,eje2],labels=iden, cex=0.5)
  axis(side=1, pos= 0, labels = F, col="cyan")
  axis(side=3, pos= 0, labels = F, col="cyan")
  axis(side=2, pos= 0, labels = F, col="cyan")
  axis(side=4, pos= 0, labels = F, col="cyan")
  #library(rgl)
  #plot3d(Psi[,1],Psi[,2],Psi[,3])
  graphics.off()
}

directori <- "C:/Users/Auba/OneDrive - Universitat de Barcelona/AM/00. Treball/individus/"

for (i in 1:ncol(Psi)) {
  for (j in i:ncol(Psi)) {
    if (i!=j)
      plot.psi(Psi, i, j, directori)
  }
}

#Projecció variables
Phi <- cor(dcon,Psi)
View(Phi)

# difícil veure outliers en aquests gràfics

# GRÀFICS NUMÈRIQUES

plot.num <- function(Psi, eje1, eje2, dir) {
  jpeg(filename= paste0(dir,"plot_num",eje1,"-",eje2,".jpeg"))
  
  X <- Phi[,eje1]
  Y <- Phi[,eje2]

  plot(Psi[,eje1],Psi[,eje2],type="n",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
  axis(side=1, pos= 0, labels = F)
  axis(side=3, pos= 0, labels = F)
  axis(side=2, pos= 0, labels = F)
  axis(side=4, pos= 0, labels = F)
  arrows(ze, ze, X, Y, length = 0.07,col="blue")
  text(X,Y,labels=etiq,col="darkblue", cex=0.7)
  
  graphics.off()
}

directori <- "C:/Users/Auba/OneDrive - Universitat de Barcelona/AM/00. Treball/numeriques/"


for (i in 1:ncol(Psi)) {
  for (j in i:ncol(Psi)) {
    if (i!=j)
      plot.num(Psi, i, j, directori)
  }
}




