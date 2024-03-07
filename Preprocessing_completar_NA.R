library(openxlsx)
library(modi)
library(misty)
library(mice)
library(multiUS)
library(dplyr)

df <- dd
df2 <- select_if(df,is.numeric)
df2$mahal <- MDmiss(df2, colMeans(df2, na.rm = TRUE), cov(df2, use="complete.obs"))
df2$p <- pchisq(df2$mahal, df=(ncol(df2)-1), lower.tail=FALSE)
outliers <- which(df2$p < 0.01)
length(outliers)
# 209 outliers

na.test(df)

# No refusem que els NA estan aleatoriament distribuits

#MICE vs. KNNI

df1 <- mice(df)
df_mice <- complete(df1)

sum(is.na(df_mice))
sum(is.na((select_if(df_mice,is.numeric))))

#Veiem que no ha completat les variables categòriques

#No podem utlitzar el knn ja que tenim masses missings