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
library('stringr')


url <- 'http://personnel.ecocup.fr/bo2api/controllers/receipts/index.php?&start='


df <- jsonlite::fromJSON(txt = paste0(url, "2020-01-06", "&end=","2020-03-31"))
View(df)
clean <- function (datas) {
  
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
  
  # On nomme les entités
  cleaned$id_entite <- sapply(cleaned$id_entite, function(x) gsub("1", "EcocupFR", x))
  cleaned$id_entite <- sapply(cleaned$id_entite, function(x) gsub("2", "EcocupBE", x))
  cleaned$id_entite <- sapply(cleaned$id_entite, function(x) gsub("3", "EcocupUK", x))
  cleaned$id_entite <- sapply(cleaned$id_entite, function(x) gsub("4", "Greencup", x))
  cleaned$montantHT <- round(as.numeric(as.character(cleaned$montant)) - as.numeric(as.character(cleaned$tva)), 2)


  cleaned <- cleaned %>% filter (reglee=="non" & as.numeric(as.character(acompte)) > 1000) 
  View (as.numeric(as.character(cleaned$acompte))) 

  
}



clean (df)
