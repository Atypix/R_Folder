idStart <- 20
idEnd <- 1

dbRemoveTable(DB, "devis")

importDevis <- function () {
  
  out <- tryCatch(
    {
      startDate <-  Sys.Date()- idStart
      endDate <-  Sys.Date()- (idEnd -1)
      url <- 'http://personnel.ecocup.fr/bo2api/controllers/quotes/index.php?start='
      
      df <- jsonlite::fromJSON(txt = paste0(url, startDate, "&end=",endDate))
      message(paste("Chargement des données Devis Vente du :", startDate, "au", endDate))
      
      df$id_entite <- sapply(df$id_entite, function(x) gsub("1", "EcocupFR", x))
      df$id_entite <- sapply(df$id_entite, function(x) gsub("2", "EcocupBE", x))
      df$id_entite <- sapply(df$id_entite, function(x) gsub("3", "EcocupUK", x))
      df$id_entite <- sapply(df$id_entite, function(x) gsub("4", "Greencup", x))
      
      #on nettoie les dates
      df$cdate <- sapply(df$cdate, function(x) gsub('([0-9]+) .*', '\\1', x))
      df$cdate <- sapply(df$cdate, function(x) gsub(' ', '', x))
      df$cdate <- sapply(df$cdate, function(x) format(as.Date(x, "%Y-%m-%d"), "%Y-%m-%d"))
      
      #on insere en BDD
      dbWriteTable(DB, "devis", df,append=TRUE)
      
      
      idStart <<- idStart + 20
      idEnd <<- idEnd + 20
      
      
      
      if(startDate <= limit) {
        message(paste("Terminé Devis"))
        idStart <<- 20
        idEnd <<- 1
        
      } else {
        importDevis()
      }
      
    },
    error=function(cond) {
      
      importDevis()
      
      return("ERROR")
    },
    finally={
      
      
      
      
    })
}

importDevis()

