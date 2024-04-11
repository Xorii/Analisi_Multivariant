#Preprocessing

```{r}
library(openxlsx)
library(modi)
library(misty)
library(mice)
library(multiUS)
library(dplyr)
library(class)
library(reshape2)
library(ggplot2)
```

```{r}
df <- D
df2 <- select_if(df,is.numeric)
df2$mahal <- MDmiss(df2, colMeans(df2, na.rm = TRUE), cov(df2, use="complete.obs"))
df2$p <- pchisq(df2$mahal, df=(ncol(df2)-1), lower.tail=FALSE)
outliers <- which(df2$p < 0.01)
length(outliers)
# 209 outliers
```

```{r}
View(df[outliers,])
```

```{r}
summary(df)
```

```{r}
summary(df[outliers,])
```

Després d'obervar i comparar estadístics hem vist que molts outliers no són aleatoris i seran rellevants en el clustering, la gran majoria dels outliers tenen una renda més alta i això afecta a les variables de consum, que es disparen. Per tant, després d'observar eliminarem els tres valors de birth que són massa petits, el valor de la renda tant alt, i els status civils tant minoritaris.

```{r}
df[which(df[,"status"] == "Absurd"),"status"] <- NA
df[which(df[,"status"] == "Alone"), "status"] <- NA
df[which(df[,"status"] == "YOLO"), "status"] <- NA
df[which(df[,"renda"] > 500000), "renda"] <- NA
df[which(df[,"birth"] < 1910), "birth"] <- NA
```


```{r}
na.test(df)
```


# No refusem que els NA estan aleatoriament distribuits

# SR (08.09.2024): Completamos los Nas que se encuentran en las variables
# categoricas

```{r}
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
```

```{r}
colSums(is.na(df[, colsCat]))
```

#MICE vs. KNNI

```{r}
df1 <- mice(df)
df_mice <- complete(df1)
```

```{r}
sum(is.na(df_mice))
```


#KNN
```{r}
summary(df)
```

```{r}
#better if you sort them by increasing number of missing values

df_imputedKNN <- multiUS::KNNimp(df, k = 2)
sum(is.na(df_imputedKNN))
```

```{r}
library(ggplot2)
library(reshape2)  # for melt function

for (cN in colsNum) {
  varOriginal <- df[, cN]
  varMICE <- df_mice[, cN]
  varKNN <- trunc(df_imputedKNN[, cN])
  
  dataImput <- data.frame(Original = varOriginal, 
                           MICE = varMICE, 
                           KNN = varKNN)
  
  dataImput <- melt(dataImput)
  
  # Create the plot
  p <- ggplot(dataImput, aes(x = value, color = variable, fill = variable)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Density Plot of", cN),
         x = cN,
         y = "Density") +
    theme_minimal() + 
    facet_wrap(~variable)
  
  # Save the plot
  ggsave(filename = paste0("density_plot_", cN, ".png"), plot = p)
}

```
