import <- function() {
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
    
    ADJ
}