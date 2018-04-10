## Met ton directory à l'emplacement des fichiers ci-dessous.
rm(list = ls())




files <- c("NA.TO",	"BCE.TO",	"CNR.TO",	"RY.TO",	"ATD-B.TO",	"GIBA.TO",	"DOL.TO",
  "BMO.TO",	"POW.TO",	"MRU.TO",	"QBRB.TO",	"PWF.TO",   "SAP.TO"	,"IAG.TO",
  "AC.TO",	"CAE.TO",	"SNC.TO",	"PJC-A.TO",	"BBDB.TO",	"TAP",	"LB.TO",
  "DOO.TO",	"CCA.TO",	"TCL-A.TO",	"GIL.TO",	"RCH.TO",	"TFII.TO",	"BLX.TO",
  "WSP.TO",	"INE.TO")


ADJ <- list()
for (i in files){
    ## Read
    input <- read.csv(paste(i,".csv",sep = ""), header = TRUE)
    
    ## RegExp
    pattern <-  "\\d+-03-01"
    match <- regmatches(input[[1]],gregexpr(pattern, input[[1]]))
    
    ## Prises des données correspondantes aux dates trouvés
    values <- input$Adj.Close[input$Date == match]
    values <- rev(values) ## Inversion
    match <- rev(unlist(Filter(length,match))) ## Mise en forme des noms
    names(values) <- match ## Attribution des noms

    ADJ[[i]] <- values ## Ajout des valeurs à la liste
}

## Guillaume Michel ---
for(i in 6:21)
{
    ADJ[["DOO.TO"]][i] <- ADJ[["DOO.TO"]][i-1] * ADJ[["BBDB.TO"]][[i]]/ADJ[["BBDB.TO"]][[i-1]]
}

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

