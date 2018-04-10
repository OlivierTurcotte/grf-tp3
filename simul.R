rm(list = ls())
n <- 10000

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("./Données Denis")


## Poids 15-30 ----
temp <- read.csv2("Poids15-30.csv",dec = ",",header = F)
poids15_30 <- matrix(0,nrow = n,ncol = 30)

for (j in seq(n)){
    c <- numeric(0)
    for (i in names(temp)){
        c <- c(c,temp[[i]][j])
    }
    poids15_30[j,] <- c
}

## Poids 30-30 ----

temp <- read.csv2("Poids30-30.csv",dec = ",",header = F)
poids30_30 <- matrix(0,nrow = n,ncol = 30)

for (j in seq(n)){
    c <- numeric(0)
    for (i in names(temp)){
        c <- c(c,temp[[i]][j])
    }
    poids30_30[j,] <- c
}
## Ecart et moyenne ----
ecart <- read.csv2("Ecart.csv",dec = ",", header =F)
corr <- read.csv2("Corr.csv",dec = ",",header = F)

## Function Risque

risque <- function(poids){
    values <- matrix(0,ncol = 1,nrow=n)
    for(ctr in seq(n)){
        a <- numeric(0)
        for(i in seq(30)){
            for( j in seq(30)){
                a <- c(a,poids[ctr,i]*poids[ctr,j]*ecart[[i]]*ecart[[j]]*corr[max(i,j),min(i,j)])
            }
        }
        values[ctr,1] <- sum(a)
    }
    values
}

risque15_30 <- risque(poids15_30)
risque30_30 <- risque(poids30_30)

write.table(risque15_30,"Risque15-30.csv",sep=";", col.names=FALSE, dec = ",")
write.table(risque30_30,"Risque30-30.csv",sep = ";",col.names = FALSE, dec = ",")
