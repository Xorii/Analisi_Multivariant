
#LECTURA DE DADES#

## Lectura de dades
library(readxl)
df <- read_excel("Dades.xlsx") ## comentar que no s'han llegit bé les dades

df <- as.data.frame(df[2:ncol(df)]) ## treure la columna id (de files)

# df[,c("kids","teens","nOf")] <- sapply(df[,c("kids","teens","nOf")],as.numeric)
  ## aquesta líena no es pot executar o no les passa desprès a factor

df[,names(df)[sapply(df,class)=="character"]] <- 
  lapply(df[,names(df)[sapply(df,class)=="character"]],as.factor) # definir categòriques

## comprovar que s'hagin definit bé les variables
sapply(df, class) # de forma general.
sum(sapply(df, class) == "factor") # nombre total de variables categòriques
sum(sapply(df, class) == "numeric") # nombre total de variables numèriques

### detall important, les categòriques amb valors fora de rang també són missings ??

## modificació 






