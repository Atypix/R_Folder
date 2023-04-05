deltaJour <- 5

idStart <- deltaJour
idEnd <- 1

dbRemoveTable(DB, "factures")
op <- options(stringsAsFactors=F) 
importFacturation <- function () {
  
  out <- tryCatch(
    {
      startDate <-  Sys.Date()- idStart
      endDate <-  Sys.Date()- (idEnd -1)
      url <- 'http://personnel.ecocup.fr/bo2api/controllers/receipts/index.php?&start='
      message(paste("Chargement des données Facturation du :", startDate, "au", endDate))
      df <- jsonlite::fromJSON(txt = paste0(url, startDate, "&end=",endDate))
      cleanFacturation (df)
      
      idStart <<- idStart + deltaJour
      idEnd <<- idEnd + deltaJour
      
      if(startDate <= limit) {
        message(paste("Terminé Factures"))
        idStart <<- deltaJour
        idEnd <<- 1
        
      } else {
        importFacturation()
      }
      
      
    },
    error=function(cond) {

      message(cond)
      importFacturation()
      return("ERROR")
    },
    
    finally={
      
      
      
      
    })
  
}

cleanFacturation <- function (datas) {

  if (datas$ref != 'ECOCUPFESTIVAL52974') {
    cleaned <- data.frame(date = datas$date,
                          id_entite = datas$id_entite,
                          acompte = datas[['acompte']],
                          restant_du = datas[['restant_du']],
                          
                          nom = datas$contenu$client$nom,
                          prenom = datas$contenu$client$prenom,
                          email = datas$contenu$client$email,
                          telephone = datas$contenu$client$telephone,
                          montant = datas$montant,
                          tva = paste0(as.character(datas$contenu$brtva), as.character(datas$contenu$panier$tvafinal) ),
                          reglee = datas$reglee,
                          pays = datas$contenu$client$pays,
                          cp = datas$contenu$client$codepostal,
                          clientype = datas$clientype)
    
    
    cleaned$tva <- sapply(cleaned$tva, function(x) gsub("[A-Z]", "\\1", x))
    cleaned$tva <- sapply(cleaned$tva, function(x) if (x=='') {x <- 0} else {x <- x})
    
    cleaned <- cleaned %>% mutate(departement = str_sub(cp, 0, 2))
    cleaned$montantHT <- round(as.numeric(as.character(cleaned$montant)) - as.numeric(as.character(cleaned$tva)), 2)
    # On nomme les entités
    cleaned$id_entite <- sapply(cleaned$id_entite, function(x) gsub("1", "EcocupFR", x))
    cleaned$id_entite <- sapply(cleaned$id_entite, function(x) gsub("2", "EcocupBE", x))
    cleaned$id_entite <- sapply(cleaned$id_entite, function(x) gsub("3", "EcocupUK", x))
    cleaned$id_entite <- sapply(cleaned$id_entite, function(x) gsub("4", "Greencup", x))
    
    cleaned$date <- sapply(cleaned$date, function(x) format(as.Date(x, "%d/%m/%Y"), "%Y-%m-%d"))
    cleaned$acompte <- sapply(cleaned$acompte, function(x) round(as.numeric(as.character(x)), 2))
    cleaned$montant <- sapply(cleaned$montant, function(x) round(as.numeric(as.character(x)), 2))
    
    #on insere en BDD
    dbWriteTable(DB, "factures", cleaned, append=TRUE)
  }

  
}

importFacturation()