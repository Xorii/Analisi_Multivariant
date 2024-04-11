load("009_Preprocessing2.RData")
df <- df_mice

library(cluster)

##Dades mixtes

#dissimilarity matrix
#do not include in actives the identifier variables nor the potential response variable


dissimMatrix <- daisy(df, metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h1 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot
class(h1)
str(h1)


plot(h1, main = "Dendograma amb totes les variables",xlab = "Dades", ylab = "")


#Gràfic del cluster amb la despesa en fruita com a variable respsota

## assignar nombre de clusters pel clustering jerarquic
k<-3 #Número de clústers obtingut mitjançant el dendograma

c1 <- cutree(h1,k)
c1

colors <- c("#6baed6","#b3de69","#ee8262")

plot(df$renda, df$fruit, col = colors[c1], 
     xlab="Renda", ylab="Despesa en fruita en u.m.",
     main="Clustering jeràrquic totes les variables",
     xli=c(0,200000))
legend("topright", legend=unique(c1), col = colors, pch = 16)



##Dades numèriques

filtre2 <- sapply(df,class)=="numeric"

dissimMatrix <- daisy(df[,filtre2], metric = "gower", stand=TRUE)

distMatrix<-dissimMatrix^2

h2 <- hclust(distMatrix,method="ward.D2")  # NOTICE THE COST
#versions noves "ward.D" i abans de plot: par(mar=rep(2,4)) si se quejara de los margenes del plot
class(h2)
str(h2)


plot(h2, main = "Dendograma amb variables numèriques", xlab = "Dades", ylab = "")



#Gràfic del cluster amb la despesa en fruita com a variable respsota

## assignar nombre de clusters pel clustering jerarquic
k<-3 #Número de clústers obtingut mitjançant el dendograma

c2 <- cutree(h2,k)
c2

colors <- c("#b3de69","#6baed6","#ee8262")

plot(df$renda, df$fruit, col = colors[c2], 
     xlab="Renda", ylab="Despesa en fruita en u.m.",
     main="Clustering jeràrquic variables numèriques",
     xli=c(0,200000))
legend("topright", legend=unique(c2), col = colors, pch = 16)
