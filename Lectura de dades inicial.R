
# LECTURA INICIAL DE LES DADES#

library(readr)
M <- read_delim("marketing_campaign.csv", 
                delim = "\t", escape_double = FALSE, 
                trim_ws = TRUE)

#Creem les variables
M$AlOf <- apply(M[,21:25],1,max)
M$NOf <- apply(M[,21:25],1,sum)

#Eliminem i reordenem les variables que no fan falta
D <- M[,-c(1,8,18,21,22,23,24,25,27,28,29)]
D <- D[,c(1:7,18,8:14,19:20,15:17)]

#Convertim les categÃ²riques en factors
D$Education <- as.factor(D$Education)
D$Marital_Status <- as.factor(D$Marital_Status)
D$Kidhome <- as.factor(D$Kidhome)
D$Teenhome <- as.factor(D$Teenhome)
D$Complain <- as.factor(D$Complain)
D$AlOf <- as.factor(D$AlOf)
D$NOf <- as.factor(D$NOf)

#Posem els noms curts  les variables
names(D) <- c("birth","educ","status","renda","kids","teens",
              "recency","comp","wine","fruit","meat","fish",
              "sweet","gold","NComOf","AlOf","NOf","web","store","WebVis")

#AleatÃ²riament borrem el 5% de les dades
set.seed(123)
ind <- sample(1:(dim(D)[1]*(dim(D)[2]-2)),112 ,replace=FALSE)
j <- ind%%18+1
i <- ind%/%18

for(k in 1:112){
  D[i[k], j[k]] <- NA
}

#### Exportar base de dades

library(xlsx)
# Coma como separador y punto como separador decimal
write.xlsx(D,"Dades.xlsx", # Nombre de la hoja de Excel
           col.names = TRUE)     # Incluir los nombres de las columnas (TRUE) o no (FALSE)
                  # Si TRUE, los NA serÃ¡n celdas vacÃ­as

