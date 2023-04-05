#!/usr/local/bin/Rscript

library('dash')
library('dashCoreComponents')
library('dashHtmlComponents')
library('dashTable')
library('jsonlite')
library('ggplot2')
library('plotly')
library('dplyr')
library('reshape2')
library('writexl')
library('htmlwidgets')
library('tidyverse')
library('lubridate')
library('ISOweek')
library('maps')
library ('geojsonR')
library('rjson')
library('RJSONIO')
library('odbc')
library('RMySQL')

idStart <- 10
idEnd <- 1
driver = dbDriver("MySQL");
DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)
dfSQL <- data.frame()
dfSQLDevis <- data.frame()

importFacturation <- function () {
  
  out <- tryCatch(
    {
      startDate <-  Sys.Date()- idStart
      endDate <-  Sys.Date()- idEnd
      url <- 'http://personnel.ecocup.fr/bo2api/controllers/receipts/index.php?&start='
      message(paste("Chargement des données Facturation du :", startDate, "au", endDate))
      df <- jsonlite::fromJSON(txt = paste0(url, startDate, "&end=",endDate))
      cleanFacturation (df)
      
      idStart <<- idStart + 10
      idEnd <<- idEnd + 10
      importFacturation()
      
    },
    error=function(cond) {
      dbRemoveTable(DB, "factures")
      dbWriteTable(DB, "factures", dfSQL)

      message(paste("Terminé"))
      idStart <<- 10
      idEnd <<- 1
      importDevis()
      return("ERROR")
    },
    warning=function(cond) {
      message(paste("Warning sur l'URL :", url))
      return("WARNING")
    },
    finally={
      
      
      
      
    })
  
}

cleanFacturation <- function (datas) {
  
  cleaned <- data.frame(date = datas$date,
                        id_entite = datas$id_entite,
                        acompte = datas[['acompte']],
                        restant_du = datas[['restant_du']],
                        
                        nom = datas$contenu$client$nom,
                        prenom = datas$contenu$client$prenom,
                        email = datas$contenu$client$email,
                        telephone = datas$contenu$client$telephone,
                        montant = datas$montant,
                        #tva = if ( !is.na(datas$contenu$brtva) ) { round(as.numeric(datas$contenu$brtva), 2)} else {datas$contenu$panier$tvafinal },
                        tva = paste0(as.character(datas$contenu$brtva), as.character(datas$contenu$panier$tvafinal) ),
                        #montantHT = paste0(as.character(datas$contenu$brht), as.character(datas$contenu$panier$totalfinalht) ),
                        reglee = datas$reglee,
                        pays = datas$contenu$client$pays,
                        cp = datas$contenu$client$codepostal,
                        clientype = datas$clientype)
  
  
  cleaned$tva <- sapply(cleaned$tva, function(x) gsub("[A-Z]", "\\1", x))
  cleaned$tva <- sapply(cleaned$tva, function(x) if (x=='') {x <- 0} else {x <- x})
  #cleaned$montantHT <- sapply(cleaned$montantHT, function(x) gsub("[A-Z]", "\\1", x))
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
  dfSQL <<- rbind(dfSQL, cleaned)
  
}

importDevis <- function () {
  out <- tryCatch(
    {
      startDate <-  Sys.Date()- idStart
      endDate <-  Sys.Date()- idEnd
      url <- 'http://personnel.ecocup.fr/bo2api/controllers/quotes/index.php?start='
      
      df <- jsonlite::fromJSON(txt = paste0(url, startDate, "&end=",endDate))
      
      df$id_entite <- sapply(df$id_entite, function(x) gsub("1", "EcocupFR", x))
      df$id_entite <- sapply(df$id_entite, function(x) gsub("2", "EcocupBE", x))
      df$id_entite <- sapply(df$id_entite, function(x) gsub("3", "EcocupUK", x))
      df$id_entite <- sapply(df$id_entite, function(x) gsub("4", "Greencup", x))
      
      #on nettoie les dates
      df$cdate <- sapply(df$cdate, function(x) gsub('([0-9]+) .*', '\\1', x))
      df$cdate <- sapply(df$cdate, function(x) gsub(' ', '', x))
      df$cdate <- sapply(df$cdate, function(x) format(as.Date(x, "%Y-%m-%d"), "%Y-%m-%d"))
      #
      dbWriteTable(DB, "devis", df,append=TRUE)
      #dbClearResult(dbListResults(DB)[[1]])
      
      dfSQLDevis <<- rbind(dfSQLDevis, df)
    
      message(paste("Chargement des données Devis Vente du :", startDate, "au", endDate))
      idStart <<- idStart + 10
      idEnd <<- idEnd + 10
      
      

      if(startDate <= '2017-06-15') {
        message(paste("Terminé Devis"))
        # Sys.sleep(10)
        # #dbRemoveTable(DB, "devis")
        # # dbClearResult(dbListResults(DB)[[1]])
        # dbWriteTable(DB, "devis", dfSQLDevis)
        # message(paste("Terminé intégré in BDD"))
        # View(dfSQLDevis)
        
        idStart <<- 10
        idEnd <<- 1
      } else {
        importDevis()
      }
      
    },
    error=function(cond) {
      


      return("ERROR")
    },
    warning=function(cond) {
      message(paste("Warning sur l'URL :", url))
      return("WARNING")
    },
    finally={
      
     
      
      
    })
}

cleanDevis <- function (datas) {
  
  df <- datas
  # On nomme les entités
  df$id_entite <- sapply(df$id_entite, function(x) gsub("1", "EcocupFR", x))
  df$id_entite <- sapply(df$id_entite, function(x) gsub("2", "EcocupBE", x))
  df$id_entite <- sapply(df$id_entite, function(x) gsub("3", "EcocupUK", x))
  df$id_entite <- sapply(df$id_entite, function(x) gsub("4", "Greencup", x))
  
  #on nettoie les dates
  df$cdate <- sapply(df$cdate, function(x) gsub('([0-9]+) .*', '\\1', x))
  df$cdate <- sapply(df$cdate, function(x) gsub(' ', '', x))
  df$cdate <- sapply(df$cdate, function(x) format(as.Date(x, "%Y-%m-%d"), "%Y-%m-%d"))
  
  
  dfSQLDevis <<- rbind(dfSQLDevis, df)
 
  
}

dbRemoveTable(DB, "devis")

importDevis()