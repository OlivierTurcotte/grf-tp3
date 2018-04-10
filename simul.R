rm(list = ls())
n <- 10000

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("./Données Denis")

temp <- read.csv2("Poids15-30_2.csv",dec = ",",header = F)
poids <- matrix(0,nrow = n,ncol = 30)

for (j in seq(n)){
    c <- numeric(0)
    for (i in names(temp)){
        c <- c(c,temp[[i]][j])
    }
    poids[j,] <- c
}

ecart <- read.csv2("Ecart.csv",dec = ",", header =F)
corr <- read.csv2("Corr.csv",dec = ",",header = F)

a <- numeric(0)
for(i in seq(30)){
    for( j in seq(30)){
        a <- c(a,poids[1,i]*poids[1,j]*ecart[i]*ecart[j]*corr[max(i,j),min(i,j)])
    }
}
sum(unlist(a))
