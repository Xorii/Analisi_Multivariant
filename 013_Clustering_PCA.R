###############################################################################
######## 013: CLUSTERING JERÀRQUIC AMB EL PCA #################################
###############################################################################

setwd("D:/UNI/Multivariant/TREBALL/DADES")

## Pàquets
library(cluster)

## Càrrega de dades
load('012_PCA_7dim.RData')
load("009_Preprocessing2.RData")
df <- Psi


## Distància euclidiana
distMatrix <- daisy(df, metric = "euclidean", stand=TRUE) 

h1 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST
class(h1)
str(h1)


plot(h1, main = "Dendograma amb la distància euclidiana",xlab = "Dades", ylab = "")

## assignar nombre de clusters pel clustering jerarquic
k<-7 

c1 <- cutree(h1,k)
c1

## Guardar la base de dades amb els clústers
df_clust <- cbind(df_mice,c1)
names(df_clust)[21] <- 'PCA'

save(df_clust, file= '013_clustering_PCA.RData')


### EXTRES

## Distància manhattan
distMatrix <- daisy(df, metric = "manhattan", stand=TRUE) 
# fem servir la distància manhattan

h2 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST
class(h1)
str(h1)


plot(h2, main = "Dendograma amb la distància de Manhattan",xlab = "Dades", ylab = "")



## Distància gower
dissimMatrix <- daisy(df, metric = "gower", stand=TRUE) 
distMatrix <- dissimMatrix**2

h3 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST
class(h1)
str(h1)


plot(h3, main = "Dendograma amb al distància de Gower^2",xlab = "Dades", ylab = "")




