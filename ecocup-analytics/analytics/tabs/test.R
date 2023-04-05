library('jsonlite')
library('writexl')
client <- 'ck_ad8ee3b61231e1e894312e59c39f1618e8cc68f3'
secret <- 'cs_635ca972f3eb343d0bee2c8aeaab3088bfd81cda'
#
#xl <- data.frame (code=c(), date=c(), tva=c(), compte=c(), tiers=c(), sage=c(), idfacture=c(), label=c(), debit=c(), credit=c(), type=c(),lettrage=c(), section=c(), Analytique=c(),Interco=c(), Devise=c(),Taux=c(), Devise_montant=c())
date_range <- "&after=2020-06-01T00:00:00-05:00&before=2020-06-31T00:00:00-05:00"

xl <- data.frame ()
message(paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range))
df <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=2&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=3&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=4&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=5&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=6&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=7&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=8&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=9&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=10&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
View (df)
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
  
  prestation <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders/", df[id,1], "?consumer_key=", client, "&consumer_secret=",secret,""), flatten = TRUE)
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

df <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=2&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=3&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=4&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=5&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=6&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=7&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=8&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=9&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=10&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
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
  
  prestation <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders/", df[id,1], "?consumer_key=", client,'&consumer_secret=',secret), flatten = TRUE)
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