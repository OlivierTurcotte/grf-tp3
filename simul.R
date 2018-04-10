rm(list = ls())
n <- 10000

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("./Données Denis")

temp <- read.csv2("Poids15-30_2.csv",dec = ",",header = F)
V <- matrix(0,nrow = n,ncol = 30)

for (j in seq(2)){
    c <- numeric(0)
    for (i in names(temp)){
        c <- c(c,temp[[i]][j])
    }
    V[j,] <- c
}
V[1,]

