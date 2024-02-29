library(readxl)
df <- read_excel("Dades.xlsx")

df <- as.data.frame(df[2:ncol(df)])

df[,c("kids","teens","NOf")] <- sapply(df[,c("kids","teens","nOf")],as.numeric)

df[,names(df)[sapply(df,class)=="character"]] <- 
  lapply(df[,names(df)[sapply(df,class)=="character"]],as.factor)

attach(df)

