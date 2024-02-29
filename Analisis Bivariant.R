
#ANALISIS BIVARIANT#

## es defineix el directori on guardar els gràfics

dirIA <- "D:/UNI/Multivariant/TREBALL/Analisis Bivariant"
#la ultima barra es important perque ho guardi dins de la carpeta i no ho afegeixi al nom

# GRAFICS

## Definim una funció per fer tots els gràfics

AnaBivar <- function(df, NomVar, dir){
  for(i in 1:(ncol(df)-1)){
    for(j in (i+1):ncol(df)){
      jpeg(filename= paste(dir,NomVar[i],NomVar[j], ".jpeg", sep=""))
      if(class(df[,i]) == "numeric" && class(df[,j]) == "numeric"){ # 2 var. numeriques
        plot(df[,i],df[,j], ylab = names(df)[i], xlab = names(df)[j], main=paste0("Scatter plot de ",NomVar[i]," y ", NomVar[j]))
      }
      if(class(df[,i]) == "numeric" && class(df[,j]) == "factor"){ #Numerica + categorica
        if(shapiro.test(df[,i])[2] < 0.05){ #comprovar normalitat. Si NO compleix: 
          plot(tapply(df[,i],df[,j],mean,na.rm=TRUE), 
               ylab = names(df)[i], xlab = names(df)[j], main=paste0("Scatter plot de ", NomVar[i]," y ", NomVar[j], sep="")) 
        }else{
          boxplot(df[,i] ~ df[,j], ylab = names(df)[i], xlab = names(df)[j], main=paste0("Boxplot de ",NomVar[i]," y ", NomVar[j])) #si compleix:
        }
      }
      if(class(df[,i]) == "factor" && class(df[,j]) == "numeric"){
        if(shapiro.test(df[,j])[2] < 0.05){
          plot(tapply(df[,j],df[,i],mean,na.rm=TRUE), 
               ylab = names(df)[j], xlab = names(df)[i], main=paste0("Scatter plot de ",NomVar[i]," y ", NomVar[j]))
        }else{
          boxplot(df[,j] ~ df[,i], ylab = names(df)[j], xlab = names(df)[i], main=paste0("Boxplot de ",NomVar[i]," y ", NomVar[j]))
        }
      }
      if(class(df[,i]) == "factor" && class(df[,j]) == "factor"){ #categorica + categorica:
        barplot(table(df[,i],df[,j]), ylab = names(df)[i], xlab = names(df)[j], 
                beside = TRUE, legend = TRUE, main=paste0("Gràfic de barres per ",NomVar[i]," y ", NomVar[j]))
        barplot(table(df[,j],df[,i]), ylab = names(df)[i], xlab = names(df)[j],
                beside = TRUE, legend = TRUE, main=paste0("Gràfic de barres per ",NomVar[i]," y ", NomVar[j]))
      }
      if(class(df[,i]) == "integer" && class(df[,j]) == "numeric"){ # binaria + numerica
        
      }
    }
  }
  graphics.off()
}

NomVar <- colnames(df) ## variables per les que es vol fer l'analisis

## Execució de la funció
AnaBivar(data, NomVar, dirIA)


# TAULES. (SCRIPT PASSAT PER UN ALTRE GRUP)

require(openxlsx)
require(dplyr)
require(psych)


df_list <- list()
a <- 0
#Na <- c()

for(i in 1:ncol(df)){
  if(class(df[,i]) == "factor"){
    for(j in 1:ncol(df)){
      if(class(df[,j]) == "numeric"){
        partial_df <- data.frame(t(sapply(psych::describeBy(df[,j], df[,i]), c)))
        a <- a + 1
        #Na[a] <- sum(c(is.na(df[,i]), is.na(df[,j])))
        #paste("data",a) <- data.frame(t(sapply(psych::describeBy(df[,j], df[,i]), c)))
        df_list[[a]] <- partial_df
      }
      
    }
  }
  
}
a <- 0

# Combinar todos los DataFrames en uno solo
datatbl <- do.call(rbind, df_list)
#data <- cbind(data, Na)
View(datatbl)
datat <- t(datatbl)
View(datat)

datat[is.na(datat)]<-''
openxlsx::write.xlsx(as.data.frame(datat), paste0(path_grafics,"/analisibivariant.xlsx"), colNames = TRUE, rowNames = TRUE, append = FALSE, dec=".")
#write.csv2(as.data.frame(datat), paste0(path_grafics,"/analisibivariant.csv"), row.names = TRUE,
#col.names = TRUE, fileEncoding = "windows-1252")
#write.table(datat, "/analisibivariant.txt", row.names = TRUE,
#col.names = TRUE, dec=".", sep="\t")

