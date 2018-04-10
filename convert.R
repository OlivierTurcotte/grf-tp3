## Met ton directory à l'emplacement des fichiers ci-dessous.
rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("import.r")
setwd("./Données historiques")
ADJ <- import()

rendement <- list()

for(i in 1:30){
    temp <- numeric(0)
    for(j in rev(seq(length(ADJ[[i]]))[-1])){
        temp[j-1] <- ADJ[[i]][j-1]/ADJ[[i]][j]
    }
    rendement[[i]] <- temp
}



gm_mean <- function(x) exp(sum(log(x[x > 0])) / length(x))-1
meanADJ <- lapply(rendement,gm_mean) ## Moyenne
varADJ  <- lapply(rendement,function(x) sqrt(var(x))) ## Ecart-Type


## Init
covADJ <- matrix(numeric(30^2),ncol = 30, nrow = 30)
corADJ <- matrix(numeric(30^2),ncol =30)
for(i in 1:30){
    for(j in 1:30){
        ## Covariance et correlation avec les donnes disponibles.
        m <- min(c(length(rendement[[i]]),length(rendement[[j]])))
        covADJ[i,j] <- cov(rendement[[i]][seq(m)],rendement[[j]][seq(m)])
        corADJ[i,j] <- cor(rendement[[i]][seq(m)],rendement[[j]][seq(m)])
    }
    
}

fileOut <- file("ADJ.csv")

## Remplissage des valeurs vides pour des strings nulle afin d'avoir des données
## de longueurs identiques
for(i in seq_len(30)){
    if(length(ADJ[[i]]) != 21){
        ADJ[[i]] <- c(ADJ[[i]],rep("",21 - length(ADJ[[i]])))
    }
}
rendement <- lapply(rendement,function(x) x-1)
for(i in seq_len(30)){
    if(length(rendement[[i]]) != 20){
        rendement[[i]] <- c(rendement[[i]],rep("",20 - length(rendement[[i]])))
    }
}


dir.create(file.path(getwd(), "Computation"), showWarnings = FALSE)
setwd(file.path(getwd(), "Computation"))

## Write out
write.table(ADJ,fileOut,sep=";", col.names=FALSE, dec = ",")
write.table(rendement,"Rendement.csv",sep = ";", col.names = F,dec = ",")
write.csv2(covADJ,"Cov.csv")
write.csv2(corADJ,"Cor.csv")
write.csv2(meanADJ,"Mean.csv")
write.csv2(varADJ,"Ecart.csv")
setwd('../..')

