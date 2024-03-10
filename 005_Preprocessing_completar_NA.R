library(openxlsx)
library(modi)
library(misty)
library(mice)
library(multiUS)
library(dplyr)
library(class)
library(reshape2)
library(ggplot2)

df <- D
df2 <- select_if(df,is.numeric)
df2$mahal <- MDmiss(df2, colMeans(df2, na.rm = TRUE), cov(df2, use="complete.obs"))
df2$p <- pchisq(df2$mahal, df=(ncol(df2)-1), lower.tail=FALSE)
outliers <- which(df2$p < 0.01)
length(outliers)
# 209 outliers

na.test(df)

# No refusem que els NA estan aleatoriament distribuits

# SR (08.09.2024): Completamos los Nas que se encuentran en las variables
# categoricas
tipos <- sapply(df, class)
colsCat <- names(tipos)[which(tipos %in% c("factor", "character"))]
colsNum <- colnames(df)[which(!colnames(df) %in% colsCat)]

df <- data.frame(df)

## Detectamos que variables tienen NAs en las categoricas
colSums(is.na(df[, colsCat]))

for (cC in colsCat) {
  # Convertimos a character
  df[, cC] <- as.character(df[, cC])
  # Convertimos los NAs
  df[which(is.na(df[, cC])), cC] <- "NotResponse"
  # Torna a ser factor
  df[, cC] <- as.factor(df[, cC])
}

colSums(is.na(df[, colsCat]))

#MICE vs. KNNI

df1 <- mice(df)
df_mice <- complete(df1)

sum(is.na(df_mice))


#KNN

summary(df)

#better if you sort them by increasing number of missing values

df_imputedKNN <- multiUS::KNNimp(df, k = 2)
sum(is.na(df_imputedKNN))

for (cN in colsNum) {
  varOriginal <- df[, cN]
  varMICE <- df_mice[, cN]
  varKNN <- trunc(df_imputedKNN[, cN])
  
  dataImput <- data.frame(Original = varOriginal, 
                          MICE = varMICE, 
                          KNN = varKNN)
  
  dataImput <- melt(dataImput)
  
  ggplot(dataImput, aes(x = value, color = variable, fill = variable)) +
    geom_density(alpha = 0.5) +
    labs(title = "",
         x = "",
         y = "") +
    theme_minimal() + 
    facet_wrap(~variable)
  ## ggsave(filename = paste0())
}
  
save(df_imputedKNN, df_mice, file = "Dades_preprocessades.RData")
