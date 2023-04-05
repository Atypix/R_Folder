idStart <- 20
idEnd <- 1
op <- options(stringsAsFactors=F) 
dbRemoveTable(DB, "produits")

importProduit <- function () {
  
  
  
  out <- tryCatch(
    {
      startDate <-  Sys.Date()- idStart
      endDate <-  Sys.Date()- (idEnd -1)
      url <- 'http://personnel.ecocup.fr/bo2api/controllers/receipts/index.php?&start='
      message(paste("Chargement des données Produits du :", startDate, "au", endDate))
      df <- jsonlite::fromJSON(txt = paste0(url, startDate, "&end=",endDate))
      
      for (i in 1:nrow(df)) {
        
        cleanProduit(df[i, 'contenu'], df[i, 'date'], df[i, 'ref_facture'], df[i, 'id_entite'],  df[i, 'contenu']$client$codepostal, df[i, 'contenu']$client$pays)
        
      }
      
      
      
      
      
      idStart <<- idStart + 20
      idEnd <<- idEnd + 20
      
      if(startDate <= limit) {
        message(paste("Terminé Produits"))
        
        
        idStart <<- 20
        idEnd <<- 1
      } else {
        importProduit()
      }
      
    },
    error=function(cond) {
      
      
      message(paste("Terminé : ", cond))
      importProduit()
      
      return("ERROR")
    },
    finally={
      
      
      
      
    })
  
}

cleanProduit <- function (datas, dateOrder, refOrder, entite, cp, pays) {
  
  produits <- data.frame()
  
  
  for (p in datas$panier$produits) {
    if("produit" %in% colnames(p)) {
      produit <- data.frame (date=dateOrder,
                             ref_facture=refOrder,
                             id_entite=entite,
                             impression= "Sans impression",
                             type_produit="Aucun",
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
                             totaltechnique = if("impression" %in% colnames(p)) {getTotalTechnique(p$impression) } else { 0 },
                             totalimpression = if("impression" %in% colnames(p)) {getTotalImpression(p$impression) } else { 0 },
                             cp=cp,
                             pays=pays
                             
                             
                             
      )
      produits <- rbind( produits, produit)
      
    } else {
      
      for(j in p) {
        if("produit" %in% colnames(j)) {
          
          produit <- data.frame (date=dateOrder,
                                 ref_facture=refOrder,
                                 id_entite=entite,
                                 impression= "Sans impression",
                                 type_produit="Aucun",
                                 categorie=if("categorie" %in% colnames(j)) {j$categorie } else { 0 },
                                 idprod=if("idprod" %in% colnames(j)) {j$idprod } else { 0 },
                                 quantite=if("quantite" %in% colnames(j)) {j$quantite } else { 0 },
                                 produit =if("produit" %in% colnames(j)) {j$produit } else { 0 },
                                 prixht =if("prixht" %in% colnames(j)) {j$prixht } else { 0 },
                                 prixlavage = if("prixlavage" %in% colnames(j)) {j$prixlavage } else { 0 },
                                 retro = if("retro" %in% colnames(j)) {j$retro } else { 0 },
                                 type = if("type" %in% colnames(j)) {j$type } else { 0 },
                                 couleur=if("couleur" %in% colnames(j)) {j$couleur } else { 0 },
                                 fond=if("fond" %in% colnames(j)) {j$fond } else { 0 },
                                 totaltechnique = if("impression" %in% colnames(j)) {getTotalTechnique(j$impression) } else { 0 },
                                 totalimpression = if("impression" %in% colnames(j)) {getTotalImpression(j$impression) } else { 0 },
                                 cp=cp,
                                 pays=pays
                                 
                                 
                                 
          )
          produits <- rbind( produits, produit)
          
        } else {
          
          produit <- data.frame (date=dateOrder,
                                 ref_facture=refOrder,
                                 id_entite=entite,
                                 impression= "Sans impression",
                                 type_produit="Aucun",
                                 categorie=if(as.character(j['categorie']) != "NULL")  { j['categorie'] } else { 0 },
                                 idprod=if(as.character(j['idprod']) != "NULL")  { j['idprod'] } else { 0 },
                                 quantite=if(as.character(j['quantite']) != "NULL")  { j['quantite'] } else { 0 },
                                 produit =if(as.character(j['produit']) != "NULL")  { j['produit'] } else { 0 },
                                 prixht =if(as.character(j['prixht']) != "NULL")  { j['prixht'] } else { 0 },
                                 prixlavage = if(as.character(j['prixlavage']) != "NULL")  { j['prixlavage'] } else { 0 },
                                 retro = if(as.character(j['retro']) != "NULL")  { j['retro'] } else { 0 },
                                 type = if(as.character(j['type']) != "NULL")  { j['type'] } else { 0 },
                                 couleur=if(as.character(j['couleur']) != "NULL")  { j['couleur'] } else { 0 },
                                 fond=if(as.character(j['fond']) != "NULL")  { j['fond'] } else { 0 },
                                 totaltechnique = if(as.character(j['impression']) != "NULL")  {getTotalTechnique(j['impression']) } else { 0 },
                                 totalimpression = if(as.character(j['impression']) != "NULL")  {getTotalImpression(j['impression']) } else { 0 },
                                 cp=cp,
                                 pays=pays
                                 
                                 
                                 
          )
          produits <- rbind( produits, produit)
        }
      } 
    }
    
    
    
  }
  
  produits$id_entite <- sapply(produits$id_entite, function(x) gsub("1", "EcocupFR", x))
  produits$id_entite <- sapply(produits$id_entite, function(x) gsub("2", "EcocupBE", x))
  produits$id_entite <- sapply(produits$id_entite, function(x) gsub("3", "EcocupUK", x))
  produits$id_entite <- sapply(produits$id_entite, function(x) gsub("4", "Greencup", x))
  produits <- replace(produits, is.na(produits), '0')
  produits$categorie <- sapply(produits$categorie, function(x) as.character(x))
  produits$prixlavage <- sapply(produits$prixlavage, function(x) as.numeric(x))
  produits$prixht <- sapply(produits$prixht, function(x) as.numeric(x))
  produits$totalimpression <- sapply(produits$totalimpression, function(x) as.numeric(x))
  produits$date <- sapply(produits$date, function(x) format(as.Date(x, "%d/%m/%Y"), "%Y-%m-%d"))
  produits <- produits %>% mutate(departement = str_sub(cp, 0, 2))
  produits[is.na(produits)] = 0
  
  produits$impression <- ifelse (produits$categorie == "classique-seri", "Serigraphie", produits$impression)
  produits$impression <- ifelse (produits$categorie == "gob-digital", "Digitale", produits$impression)
  produits$impression <- ifelse (produits$categorie == "classique-quadri", "IML", produits$impression)
  produits$impression <- ifelse (produits$categorie == "neutres", "Sans impression", produits$impression)
  produits$impression <- ifelse (produits$categorie == "porte-verres", "Sans impression", produits$impression)
  produits$impression <- ifelse (produits$categorie == "materiels", "Sans impression", produits$impression)
  produits$impression <- ifelse (produits$categorie == "materiel", "Sans impression", produits$impression)
  produits$impression <- ifelse (produits$categorie == "carafe" & as.numeric(produits$totalimpression) > 0, "Serigraphie", produits$impression)
  produits$impression <- ifelse (produits$categorie == "carafe" & produits$totalimpression==0, "Sans impression", produits$impression)
  produits$impression <- ifelse (produits$categorie == "chics" & as.numeric(produits$totalimpression) > 0, "Serigraphie", produits$impression)
  produits$impression <- ifelse (produits$categorie == "chics" & produits$totalimpression==0, "Sans impression", produits$impression)
  produits$impression <- ifelse (produits$categorie == "vin" & as.numeric(produits$totalimpression) > 0, "Serigraphie", produits$impression)
  produits$impression <- ifelse (produits$categorie == "vin" & produits$totalimpression==0, "Sans impression", produits$impression)
  produits$impression <- ifelse (produits$categorie == "tubes" & as.numeric(produits$totalimpression) > 0, "Serigraphie", produits$impression)
  produits$impression <- ifelse (produits$categorie == "tubes" & produits$totalimpression==0, "Sans impression", produits$impression)  
  produits$impression <- ifelse (grepl("IML", produits$produit, fixed=TRUE), "IML", produits$impression)
  produits$impression <- ifelse (grepl("DIGITALE", produits$produit, fixed=TRUE), "Digitale", produits$impression)
  produits$impression <- ifelse (produits$impression == "Sans impression" & produits$couleur>0, "A définir", produits$impression) 
  
  
  produits$type_produit <- ifelse (grepl("VIN", produits$produit, fixed=TRUE), "ECO VIN", produits$type_produit)
  produits$type_produit <- ifelse (grepl("vin", produits$produit, fixed=TRUE), "ECO VIN", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Vin", produits$produit, fixed=TRUE), "ECO VIN", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO 30", produits$produit, fixed=TRUE), "ECO 30", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO30", produits$produit, fixed=TRUE), "ECO 30", produits$type_produit) 
  produits$type_produit <- ifelse (grepl("G30", produits$produit, fixed=TRUE), "G30", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO 60", produits$produit, fixed=TRUE), "ECO 60", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO60", produits$produit, fixed=TRUE), "ECO 60", produits$type_produit) 
  produits$type_produit <- ifelse (grepl("ECO 18", produits$produit, fixed=TRUE), "ECO 18", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO18", produits$produit, fixed=TRUE), "ECO 18", produits$type_produit) 
  produits$type_produit <- ifelse (grepl("CARAFE", produits$produit, fixed=TRUE), "CARAFE", produits$type_produit)
  produits$type_produit <- ifelse (grepl("CHAMP", produits$produit, fixed=TRUE), "ECO CHAMP", produits$type_produit)
  produits$type_produit <- ifelse (grepl("PV", produits$produit, fixed=TRUE), "PORTE-VERRES", produits$type_produit)
  produits$type_produit <- ifelse (produits$prixht<0, "REMISE", produits$type_produit) 
  produits$type_produit <- ifelse (grepl("ECO 28", produits$produit, fixed=TRUE), "ECO 28", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO 15", produits$produit, fixed=TRUE), "ECO 15", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO28", produits$produit, fixed=TRUE), "ECO 28", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO 12", produits$produit, fixed=TRUE), "ECO 12", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO12", produits$produit, fixed=TRUE), "ECO 12", produits$type_produit)
  produits$type_produit <- ifelse (grepl("G 60", produits$produit, fixed=TRUE), "G60", produits$type_produit)
  produits$type_produit <- ifelse (grepl("G60", produits$produit, fixed=TRUE), "G60", produits$type_produit)
  produits$type_produit <- ifelse (grepl("G25", produits$produit, fixed=TRUE), "G25", produits$type_produit)
  produits$type_produit <- ifelse (grepl("G28", produits$produit, fixed=TRUE), "G28", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO 40", produits$produit, fixed=TRUE), "ECO 40", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO40", produits$produit, fixed=TRUE), "ECO 40", produits$type_produit)
  produits$type_produit <- ifelse (grepl("CTD30", produits$produit, fixed=TRUE), "CTD30", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Lavage", produits$produit, fixed=TRUE), "LAVAGE", produits$type_produit)
  produits$type_produit <- ifelse (grepl("lavage", produits$produit, fixed=TRUE), "LAVAGE", produits$type_produit)
  produits$type_produit <- ifelse (grepl("douane", produits$produit, fixed=TRUE), "DOUANE", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Eco 60", produits$produit, fixed=TRUE), "ECO60", produits$type_produit)
  produits$type_produit <- ifelse (grepl("BODEGA", produits$produit, fixed=TRUE), "ECO BODEGA", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO 15", produits$produit, fixed=TRUE), "ECO 15", produits$type_produit)
  
  produits$type_produit <- ifelse (grepl("ECO 50", produits$produit, fixed=TRUE), "ECO 50", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO50", produits$produit, fixed=TRUE), "ECO 50", produits$type_produit)
  
  produits$type_produit <- ifelse (grepl("Eco30", produits$produit, fixed=TRUE), "ECO 30", produits$type_produit)
  
  produits$type_produit <- ifelse (grepl("TUBO", produits$produit, fixed=TRUE), "ECO TUBO", produits$type_produit)
  produits$type_produit <- ifelse (grepl("TUBO", produits$produit, fixed=TRUE), "ECO TUBO", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Compteuse", produits$produit, fixed=TRUE), "COMPTEUSE", produits$type_produit)
  produits$type_produit <- ifelse (grepl("compteuse", produits$produit, fixed=TRUE), "COMPTEUSE", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Affiche", produits$produit, fixed=TRUE), "AFFICHE", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Tour de cou", produits$produit, fixed=TRUE), "PORTE-VERRES", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Caisse plastique", produits$produit, fixed=TRUE), "CAISSE PLASTIQUE", produits$type_produit)
  produits$type_produit <- ifelse (produits$categorie == "vin", "ECO VIN", produits$type_produit)
  produits$type_produit <- ifelse (produits$categorie == "chics", "ECO CHAMP", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Eco12", produits$produit, fixed=TRUE), "ECO 12", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Eco18", produits$produit, fixed=TRUE), "ECO 18", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Eco40", produits$produit, fixed=TRUE), "ECO 40", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Eco50", produits$produit, fixed=TRUE), "ECO 50", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Eco60", produits$produit, fixed=TRUE), "ECO 60", produits$type_produit)
  produits$type_produit <- ifelse (grepl("ECO MUG", produits$produit, fixed=TRUE), "ECO MUG", produits$type_produit)
  produits$type_produit <- ifelse (grepl("G40", produits$produit, fixed=TRUE), "G40", produits$type_produit)
  produits$type_produit <- ifelse (grepl("Assiette", produits$produit, fixed=TRUE), "ASSIETTE", produits$type_produit)
  produits$type_produit <- ifelse (produits$categorie == "tubes", "ECO TUBO", produits$type_produit) 
  
  
  
  dbWriteTable(DB, "produits", produits,append=TRUE)
  #dfProduit <<- rbind(dfProduit, produits)
  
}



getTotalTechnique <- function (impression) {
  total <- 0
  for(p in impression) {
    
    if("totaltechnique" %in% colnames(p)) {
      if(is.na(p$totaltechnique[1]) == FALSE) {
        total <- total + as.numeric(p$totaltechnique[1])
      }
    }
  }
  
  if (as.numeric(total) == 0 ) {
    for(p in impression) {
      for(k in p) {
        
        total <- total + as.numeric(k['totaltechnique'][1])
      }
    }
  }
  return (total)
}

getTotalImpression <- function (impression) {
  total <- 0
  
  for(p in impression) {
    
    if("totalimpression" %in% colnames(p)) {
      if(is.na(p$totalimpression[1]) == FALSE) {
        
        total <- total + as.numeric(p$totalimpression[1])
      }
    }
  }
  
  if (as.numeric(total) == 0 ) {
    for(p in impression) {
      for(k in p) {
        
        total <- total + as.numeric(k['totalimpression'][1])
      }
    }
  }
  
  return (total)
}

importProduit()