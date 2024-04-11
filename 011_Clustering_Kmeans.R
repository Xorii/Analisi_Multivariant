
#Retrieve the data saved AFTER the profiling practice...... this means data already cleaned

## directori de treball
#setwd("D:/UNI/Multivariant/TREBALL/DADES")
load("009_Preprocessing2.RData") ## es treballarà amb les dades mice. 


### paquets a fer servir
library(cluster)
library(clusterSim)
library(fpc)
library(pracma)




## es comprova la lectura correcta de les dades
names(df_mice)
dim(df_mice)
summary(df_mice)


attach(df_mice)

#set a list of numerical variables
names(df_mice)

dcon <- data.frame (birth, renda, recency, wine, fruit, meat, fish, 
                    sweet, gold, NComOf, web, store, WebVis)
dim(dcon)

#
# CLUSTERING
#

## s'afegeix una arrel
set.seed(123)

### mètode del colze
library(ggplot2)

## calcular tots els kmeans del 1 al 10. 
sse <- c()
for (k in 1:10) {
  kmeans_model <- kmeans(dcon, k) #dcon son totes les var numeriques i k el nombre de classes a fer. 
  sse[k] <- kmeans_model$tot.withinss
}

plot <- ggplot(data.frame(x=1:10, y=sse), aes(x, y)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  labs(x="Number of clusters", y="SSE")
plot

## s'escull k=6, és on es s'estabilitza el colze. 

kmeans_model <- kmeans(dcon, 6)
print(kmeans_model) 
attributes(kmeans_model) #atributs de cada cluster
kmeans_model$size ## tamanys de cada cluster

## Afegir la classificació dels clusters en la base de dades
df_mice[,21]<-kmeans_model$cluster
names(df_mice)[21]<-"Kmeans"
names(df_mice)[17]<-"Ward"

## gràfics mostrant la classificació dels clústers

plot(df_mice$renda, df_mice$fruit, col = as.numeric(df_mice$Kmeans), 
     xlab="Renda", ylab="Despesa en fruita en u.m.", main="Clustering K-Means")
legend("topright", legend=unique(df_mice$Kmeans), col = 1:length(df_mice$Kmeans), pch = 1)

## per veure millor els clústers:

plot(df_mice$renda, df_mice$fruit, col = as.numeric(df_mice$Kmeans), xlim=c(0,17e+04), 
     xlab="Renda", ylab="Despesa en fruita en u.m.", main="Clustering K-Means")
legend("topright", legend=unique(df_mice$Kmeans), col = 1:length(df_mice$Kmeans), pch = 1)


## gràfics mostrant la classificació dels clústers (per una altra variable)

plot(df_mice$recency, df_mice$fruit, col = as.numeric(df_mice$Kmeans), 
     xlab="Dies transcorreguts des de l'última compra", ylab="Despesa en fruita en u.m.", main="Clustering K-Means")
legend("topright", legend=unique(df_mice$Kmeans), col = 1:length(df_mice$Kmeans), pch = 1)


# LETS COMPUTE THE DECOMPOSITION OF INERTIA

Bss <- sum(rowSums(kmeans_model$centers^2)*kmeans_model$size)
Wss <- sum(kmeans_model$withinss)
Tss <- kmeans_model$totss # buscar que es aixo

CalinskiHarabaz<-Bss/Wss ## vol valors el més grans possibles. 


#
# VALIDACIÓ DE CLUSTERING
#


# Método de gap estadístico (Gap statistic method)
### Este método compara la SSE para el clustering observado con la SSE esperada 
### para un conjunto de datos aleatorios de referencia. El nÃºmero Ã³ptimo de clusters 
### se elige como aquel que maximiza la diferencia entre las SSE observadas y las SSE esperadas. 

library(cluster)
gap_stat <- clusGap(dcon, FUN=kmeans, nstart=10, K.max=10, B=50)
plot <- plot(gap_stat, main="Gap statistic plot")


# Coeficiente de Silhouette
### mide la similitud de cada objeto en su propio grupo en comparaciÃ³n con otros 
### grupos. Este coeficiente tiene un rango de -1 a 1, donde los valores mÃ¡s cercanos 
### a 1 indican que los grupos son mÃ¡s distintos entre sÃ­. 
### R: cluster::silhouette()

## Calcular coeficiente de Silhouette

sil <- c()

for (k in 2:10) {
  kmeans_result <- kmeans(dcon, k)
  sil[k] <- mean(silhouette(kmeans_result$cluster, dist(dcon))[, 3])
}
sil <- sil[-1]
plot <- ggplot(data.frame(x=2:10, y=sil), aes(x, y)) +
  geom_line() +
  geom_point() +
  labs(x="Number of clusters", y="Silhouette coefficient")
plot

### El número óptimo de clusters es aquel que maximiza el promedio del índice de silueta



### En el grÃ¡fico, puedes buscar el punto de codo, donde la disminuciÃ³n en el Ã­ndice de 
### Davies-Bouldin disminuye significativamente y seleccionar el valor de k que se encuentra 
### en esa posiciÃ³n como el nÃºmero Ã³ptimo de clusters.

# -----------------------------------------------------------------------------
# Ãndice de Calinski-Harabasz
### mide la relaciÃ³n entre la dispersiÃ³n dentro de cada grupo y la dispersiÃ³n entre los grupos. 
### Un valor mÃ¡s alto indica una mejor separaciÃ³n entre los grupos. 
### R: fpc::calinski.test()

## Calcular Ã­ndice de Calinski-Harabasz

CH_ind <- c()
for(k in 2:10){
  kmeans_result <- kmeans(dcon, centers = k)
  Bss <- sum(rowSums(kmeans_result$centers^2)*kmeans_result$size)
  Wss <- sum(kmeans_result$withinss)
  CH_ind[k] <- Bss/Wss 
}

CH_dfr <- data.frame(k = 2:10, CH = CH_ind[-1])
ggplot(CH_dfr, aes(x = k, y = CH)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = c(2, 4, 5,6, 8, 10))





