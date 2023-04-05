library('jsonlite')
library('writexl')
#clé client : ck_7499cc93588cd3ce128e5deb07572ef7e6f53375
#clé secrete : cs_f8e1e5ec4d3d3a2840d7cfad5823293145f2b9ff
#
#xl <- data.frame (code=c(), date=c(), tva=c(), compte=c(), tiers=c(), sage=c(), idfacture=c(), label=c(), debit=c(), credit=c(), type=c(),lettrage=c(), section=c(), Analytique=c(),Interco=c(), Devise=c(),Taux=c(), Devise_montant=c())
xl <- data.frame ()
df <- fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?per_page=100&status=delivery-final&consumer_key=ck_7499cc93588cd3ce128e5deb07572ef7e6f53375&consumer_secret=cs_f8e1e5ec4d3d3a2840d7cfad5823293145f2b9ff&after=2020-02-13T00:00:00-05:00&before=2020-04-01T00:00:00-05:00"), flatten = TRUE)
id <- 1
idCustomer <- 1
op <- options(stringsAsFactors=F) 
for (p in df[,1]) {
  dftmp <- c("VEN", as.character(df[id,9]), "", '411100', df[id,21], idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), df$total[[id]], "", "G","","","","CHORS GROUPE","","","",as.character(df[id,9]), "")
  dftmp2 <- c("VEN", as.character(df[id,9]), "C20", '708501', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", df$shipping_total[[id]], "G","","","","CHORS GROUPE","","","","", "")
  dftmp3 <- c("VEN", as.character(df[id,9]), "C20", '708501', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", df$shipping_total[[id]], "A","","1","11201","CHORS GROUPE","","","","", "")
  dftmp4 <- c("VEN", as.character(df[id,9]), "C20", '707001', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", (as.numeric(df$total[[id]])-as.numeric(df$total_tax[[id]])-as.numeric(df$shipping_total[[id]])), "G","","","","CHORS GROUPE","","","","", "")
  dftmp5 <- c("VEN", as.character(df[id,9]), "C20", '707001', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", (as.numeric(df$total[[id]])-as.numeric(df$total_tax[[id]])-as.numeric(df$shipping_total[[id]])), "A","","2","219900","CHORS GROUPE","","","","", "")
  dftmp6 <- c("VEN", as.character(df[id,9]), "C20", '445701', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", df$total_tax[[id]], "G","","","","CHORS GROUPE","","","","", "")
  xl<- rbind(xl,dftmp)
  xl<- rbind(xl,dftmp2)
  xl<- rbind(xl,dftmp3)
  xl<- rbind(xl,dftmp4)
  xl<- rbind(xl,dftmp5)
  xl<- rbind(xl,dftmp6)
  
  prestation <- fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders/", df[id,1], "?consumer_key=ck_7499cc93588cd3ce128e5deb07572ef7e6f53375&consumer_secret=cs_f8e1e5ec4d3d3a2840d7cfad5823293145f2b9ff"), flatten = TRUE)
  lines <- prestation["line_items"]
  for(p in lines) {
    meta <- p$meta_data
    for(q in meta) {
      message(q$key[[2]])
      if (q$key[[2]] == "Assistance Ecocup (19,92€)") {
        dftmp7 <- c("VEN", as.character(df[id,9]), "C20", '706511', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", "16.60", "G","","","","CHORS GROUPE","","","","", "")
        xl<- rbind(xl,dftmp7)
      }
      if (q$key[[2]] == "Assistance Ecocup (51,36€)") {
        dftmp7 <- c("VEN", as.character(df[id,9]), "C20", '706511', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", "42,80", "G","","","","CHORS GROUPE","","","","", "")
        xl<- rbind(xl,dftmp7)
      }

    }
  }
  
  id <- id + 1
  idCustomer <- idCustomer + 1
}

df <- fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?per_page=100&status=completed&consumer_key=ck_7499cc93588cd3ce128e5deb07572ef7e6f53375&consumer_secret=cs_f8e1e5ec4d3d3a2840d7cfad5823293145f2b9ff&after=2020-02-13T00:00:00-05:00&before=2020-04-01T00:00:00-05:00"), flatten = TRUE)
id <- 1
op <- options(stringsAsFactors=F) 
for (p in df[,1]) {
  dftmp <- c("VEN", as.character(df[id,9]), "", '411100', df[id,21], idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), df$total[[id]], "", "G","","","","CHORS GROUPE","","","",as.character(df[id,9]), "")
  dftmp2 <- c("VEN", as.character(df[id,9]), "C20", '708501', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", df$shipping_total[[id]], "G","","","","CHORS GROUPE","","","","", "")
  dftmp3 <- c("VEN", as.character(df[id,9]), "C20", '708501', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", df$shipping_total[[id]], "A","","1","11201","CHORS GROUPE","","","","", "")
  dftmp4 <- c("VEN", as.character(df[id,9]), "C20", '707001', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", (as.numeric(df$total[[id]])-as.numeric(df$total_tax[[id]])-as.numeric(df$shipping_total[[id]])), "G","","","","CHORS GROUPE","","","","", "")
  dftmp5 <- c("VEN", as.character(df[id,9]), "C20", '707001', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", (as.numeric(df$total[[id]])-as.numeric(df$total_tax[[id]])-as.numeric(df$shipping_total[[id]])), "A","","2","219900","CHORS GROUPE","","","","", "")
  dftmp6 <- c("VEN", as.character(df[id,9]), "C20", '445701', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", df$total_tax[[id]], "G","","","","CHORS GROUPE","","","","", "")
  xl<- rbind(xl,dftmp)
  xl<- rbind(xl,dftmp2)
  xl<- rbind(xl,dftmp3)
  xl<- rbind(xl,dftmp4)
  xl<- rbind(xl,dftmp5)
  xl<- rbind(xl,dftmp6)
  
  prestation <- fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders/", df[id,1], "?consumer_key=ck_7499cc93588cd3ce128e5deb07572ef7e6f53375&consumer_secret=cs_f8e1e5ec4d3d3a2840d7cfad5823293145f2b9ff"), flatten = TRUE)
  lines <- prestation["line_items"]
  for(p in lines) {
    meta <- p$meta_data
    for(q in meta) {
      message(q$key[[2]])
      if (q$key[[2]] == "Assistance Ecocup (19,92€)") {
        dftmp7 <- c("VEN", as.character(df[id,9]), "C20", '706511', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", "16.60", "G","","","","CHORS GROUPE","","","","", "")
        xl<- rbind(xl,dftmp7)
      }
      if (q$key[[2]] == "Assistance Ecocup (51,36€)") {
        dftmp7 <- c("VEN", as.character(df[id,9]), "C20", '706511', "", idCustomer, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", "42.80", "G","","","","CHORS GROUPE","","","","", "")
        xl<- rbind(xl,dftmp7)
      }
      
    }
  }
  
  id <- id + 1
  idCustomer <- idCustomer + 1
}


colnames(xl) <- list("Code journal", "Date écriture", "Code TVA", "Cpt Général", "Compte tiers", "N° écriture SAGE", "Numéro facture", "Libellé écriture", "Débit euro", "Crédit euro", "Type écriture", "Lettrage", "Section", "Analytique", "Interco", "Devise", "Taux change", "Montant devise", "Date échéance"  )
xl[["Date écriture"]] <- sapply(xl[["Date écriture"]], function(x) format(as.Date(x, "%Y-%m-%dT%H:%M:%S"), "%d/%m/%Y"))
xl[["Date échéance"]] <- sapply(xl[["Date échéance"]], function(x) format(as.Date(x, "%Y-%m-%dT%H:%M:%S"), "%d/%m/%Y"))






write_xlsx(xl, paste0(getwd(),'/woocommerce.xlsx'))