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
#setwd('/home/goach/Documents/workspace/R_Folder/ecocup-analytics/analytics/')
idStart <- 50
idEnd <- 40
dfProduit <- data.frame()
op <- options(stringsAsFactors=F) 
importProduit <- function () {
  out <- tryCatch(
    {
      startDate <-  Sys.Date()- idStart
      endDate <-  Sys.Date()- idEnd
      url <- 'http://personnel.ecocup.fr/bo2api/controllers/receipts/index.php?&start='
      message(paste("Chargement des données Facturation du :", startDate, "au", endDate))
      df <- jsonlite::fromJSON(txt = paste0(url, startDate, "&end=",endDate))
      cleanProduit (df)
      
      idStart <<- idStart + 10
      idEnd <<- idEnd + 10
      if(startDate <= '2019-12-31') {
        message(paste("Terminé Produits"))
        View(dfProduit)  
        
        idStart <<- 10
        idEnd <<- 1
      } else {
        importProduit()
      }
      
    },
    error=function(cond) {

      
      message(paste("Terminé", cond))
      idStart <<- 10
      idEnd <<- 1

      return("ERROR")
    },
    warning=function(cond) {
      message(cond)
      message(paste("Warning sur l'URL :", paste("Chargement des données Facturation du :", startDate, "au", endDate)))
      View(dfProduit)
      return("WARNING")
    },
    finally={
      
      
      
      
    })
}

cleanProduit <- function (datas, dateOrder) {
  
  produits <- data.frame()

  for (p in datas$contenu$panier$produits) {
    out <- tryCatch(
      {
      #message(paste(colnames(p$impression[['1']]), " "))
        #message(length(p$impression))
        produit <- data.frame (date=dateOrder,
                               categorie=if("categorie" %in% colnames(p)) {p$categorie } else { 0 },
                               idprod=if("idprod" %in% colnames(p)) {p$idprod } else { 0 },
                               quantite=if("quantite" %in% colnames(p)) {p$quantite } else { 0 },
                               produit =if("produit" %in% colnames(p)) {p$produit } else { 0 },
                               prixht =if("prixht" %in% colnames(p)) {p$prixht } else { 0 },
                               prixlavage = if("prixlavage" %in% colnames(p)) {p$prixlavage } else { 0 },
                               retro = if("retro" %in% colnames(p)) {p$retro } else { 0 },
                               type = if("type" %in% colnames(p)) {p$type } else { 0 },
                               couleur=if("couleur" %in% colnames(p)) {p$couleur } else { 0 },
                               fond=if("fond" %in% colnames(p)) {p$fond } else { 0 },
                               totaltechnique = if ("impression" %in% colnames(p)) {
                                 if(length(p$impression) > 0) {
                                   if ("totaltechnique" %in% colnames(p$impression[['1']])) {p$impression[['1']]$totaltechnique } else { 0 }
                                 } else {
                                   0
                                 } 
                               } else {
                                 0
                               }
                                 
                               
        )
        
        
        produits <- rbind( produits, produit)
      
      
      },
      error=function(cond) {
        
        message(p$impression)
        message(paste('ERROR', cond))
        
        
        return("ERROR")
      })
     
  

    
    
  }
  
  
  
  dfProduit <<- rbind(dfProduit, produits)
  
 
  
  
  
  
  #dfProduit <<- rbind(dfProduit, produits)
  
}

importProduit()