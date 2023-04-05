#setwd('/home/goach/Documents/workspace/R_Folder/exports_site/')
rm(list=ls())
library('lubridate')
library('dplyr')
library('jsonlite')
library('writexl')
library('stringr')
library('tidyr')


xl <- data.frame ()
client <- 'ck_ad8ee3b61231e1e894312e59c39f1618e8cc68f3'
secret <- 'cs_635ca972f3eb343d0bee2c8aeaab3088bfd81cda'

start <- '2023-01-01'
end <- '2023-03-09'

start_before <- as.Date(start)-months(2)

csv <- read.csv("commande_web.csv", header = FALSE, sep = ";")
commandes <- csv[6]
colnames(commandes) <- list("number")


page <- 1
date_range <- paste0("&after=",start_before,"T00:00:00-05:00&before=",end,"T00:00:00-05:00")

bool <- TRUE

df <- data.frame()
dforders <- data.frame()

while(bool == TRUE) {

  url <- paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range, "&page=",page)
  message (url)
  tmp <- tempfile()
  h <- curl::new_handle(ssl_verifypeer= 0)
  curl::curl_download(url, destfile = tmp, handle = h)
  orders <- jsonlite::fromJSON(txt = tmp, flatten = TRUE)
  page <- page + 1

  if (nrow(orders) < 100) {
    bool <- FALSE
  }

  if (page == 1) {
    df <- orders
  } else {
    df <- rbind(df, orders)
  }

}


op <- options(stringsAsFactors=F) 


id <- 1
for(p in 1:nrow(df)) {
    tarif <- 0
    lines <- df$line_items[[p]]
    meta <- as.data.frame(lines$meta_data[1])
    graphisme <- meta %>% filter (str_detect(key, 'Assistance'))
    
    if(nrow(graphisme) > 0) {
      graphisme <- graphisme$key
      graphisme <- str_extract(graphisme, "\\(.*?\\)")
      graphisme <- str_remove(graphisme, "[(]")
      graphisme <- str_remove(graphisme, "[€]")
      graphisme <- str_remove(graphisme, "[)]")
      graphisme <- graphisme %>% str_replace_all(",", ".")
      #message(graphisme[1,2])
      graphisme <- as.data.frame(graphisme)
      idGraph <- 1
      for(s in graphisme[,1]) {
        
        tarif <- as.numeric(graphisme[idGraph,1]) - ((as.numeric(graphisme[idGraph,1])/120)*20)
        
        idGraph <- idGraph + 1
        
        
      }
    }
  
  for(q in df[p,34]) {
    
    for(r in 1:length(q$quantity)) {
      
      dftmp <- c(df[p,3], df[p,7], df[p,9], df[p,31], df[p,42], df[p,41], 'ECO30', q['name'][r,], q['quantity'][r,], q['price'][r,], q['total'][r,], q['total_tax'][r,], (as.numeric(q['total'][r,]) + as.numeric(q['total_tax'][r,]) ), df[p, 15], df[p,49],  df[p,13], tarif)
      dftmp <- unname(dftmp)
      dforders <- rbind(dforders,dftmp)
      
    }
    
  }
  id <- id + 1
}

names(dforders)[1] <- 'number'
names(dforders)[2] <- 'status'
names(dforders)[3] <- 'Date_commande'
names(dforders)[4] <- 'Date de la facture'
names(dforders)[5] <- 'Nom'
names(dforders)[6] <- 'Prénom'
names(dforders)[7] <- 'Code produit'
names(dforders)[8] <- 'Désignation produit'
names(dforders)[9] <- 'Quantité'
names(dforders)[10] <- 'Prix unitaire'
names(dforders)[11] <- 'Montant HT'
names(dforders)[12] <- 'TVA'
names(dforders)[13] <- 'Montant TTC'
names(dforders)[14] <- 'Livraison'
names(dforders)[15] <- 'Pays'
names(dforders)[16] <- 'Discount' 
names(dforders)[17] <- 'Prestation graphiste'
dforders <- dforders %>% filter (Date_commande >= as.Date(start))
dforders <- dforders %>% filter (Date_commande <= as.Date(end))


write_xlsx(dforders, paste0(getwd(),'/orders_all_with_status.xlsx'))
dforders <- dforders %>% filter (Date_commande >= as.Date('2021-01-01'))
dforders <- dforders %>% filter (Date_commande <= as.Date(end))
dforders <- dforders %>% filter (status=="delivery-final" | status=="completed")
dforders <- anti_join(dforders, commandes, by = "number")
write_xlsx(dforders, paste0(getwd(),'/invoices.xlsx'))

df <- df %>% filter (status=="delivery-final" | status=="completed")

df <- anti_join(df, commandes, by = "number")
df$date_completed <- sapply(df$date_completed, function(x) format(as.Date(x, origin="1970-01-01", format = "%Y-%m-%d"), "%Y-%m-%d"))
idstatus <- 1
for (p in df[,1]) {
  
  
  if (df$status[idstatus] == 'delivery-final' & is.na(df$date_completed[idstatus]) == TRUE) {
    message(df$number[idstatus])
    df$date_completed[idstatus] = end
  }
  
  idstatus <- idstatus + 1
}
df <- df %>% filter (date_completed >= as.Date(start))
df <- df %>% filter (date_completed <= as.Date(end))




id <- 1
idCustomer <- 1
op <- options(stringsAsFactors=F) 
for (p in df[,1]) {
  #message(df$number[[id]], " plop")
  line_facture <- df$meta_data[[id]] %>% filter (key == '_invoice_number_display') 
  
  if (nrow(line_facture)>0) {
    num_facture <- line_facture$value[1]
    
  } else {
    num_facture <- df[id,3]
  }
  
  df$num_facture = num_facture
  message(num_facture, " plop")
  if (num_facture != 'WEFR527400' & num_facture != 'W17671' & num_facture !=  'W14035' &  num_facture !='W13253' & num_facture != 'W13304' & num_facture != 'W13486' & num_facture != 'W13870' & num_facture != 'W13485') {
    

      if (df$billing.country[[id]] == 'FR') {
        code <- 'VEN'
        transport <- '708501'
        marchandise <- '707001'
      } else {
        code <- 'VEC'
        transport <- '708511'
        marchandise <- '707011'
      }
      
      dftmp <- c(code, as.character(df[id,9]), "", '411100', "CWEBFR", idCustomer, num_facture, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), df$total[[id]], "", "G","","","","CHORS GROUPE","","","",as.character(df[id,9]), "")
      dftmp2 <- c(code, as.character(df[id,9]), "C20", transport, "", idCustomer, num_facture, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", df$shipping_total[[id]], "G","","","","CHORS GROUPE","","","","", "")
      dftmp3 <- c(code, as.character(df[id,9]), "C20", transport, "", idCustomer, num_facture, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", df$shipping_total[[id]], "A","","1","11201","CHORS GROUPE","","","","", "")
      dftmp4 <- c(code, as.character(df[id,9]), "C20", marchandise, "", idCustomer, num_facture, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", (as.numeric(df$total[[id]])-as.numeric(df$total_tax[[id]])-as.numeric(df$shipping_total[[id]])), "G","","","","CHORS GROUPE","","","","", "")
      dftmp5 <- c(code, as.character(df[id,9]), "C20", marchandise, "", idCustomer, num_facture, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", (as.numeric(df$total[[id]])-as.numeric(df$total_tax[[id]])-as.numeric(df$shipping_total[[id]])), "A","","2","219900","CHORS GROUPE","","","","", "")
      dftmp6 <- c(code, as.character(df[id,9]), "C20", '445701', "", idCustomer, num_facture, df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", df$total_tax[[id]], "G","","","","CHORS GROUPE","","","","", "")
      xl<- rbind(xl,dftmp)
      xl<- rbind(xl,dftmp2)
      xl<- rbind(xl,dftmp3)
      xl<- rbind(xl,dftmp4)
      xl<- rbind(xl,dftmp5)
      xl<- rbind(xl,dftmp6)
      
      lines <- df$line_items[[id]]
      meta <- as.data.frame(lines$meta_data)
      graphisme <- meta %>% filter (str_detect(key, 'Assistance'))
      
      if(nrow(graphisme) > 0) {
        graphisme <- graphisme$key
        graphisme <- str_extract(graphisme, "\\(.*?\\)")
        graphisme <- str_remove(graphisme, "[(]")
        graphisme <- str_remove(graphisme, "[€]")
        graphisme <- str_remove(graphisme, "[)]")
        graphisme <- graphisme %>% str_replace_all(",", ".")
        #message(graphisme[1,2])
        graphisme <- as.data.frame(graphisme)
        idGraph <- 1
        for(q in graphisme[,1]) {
          if (code == 'VEN') {
            tarif <- as.numeric(graphisme[idGraph,1]) - ((as.numeric(graphisme[idGraph,1])/120)*20)
            compte <- '706501'

          } else {
            tarif <- as.numeric(graphisme[idGraph,1]) - ((as.numeric(graphisme[idGraph,1])/120)*20)
            compte <- '706511'
          }
          
          idGraph <- idGraph + 1
          message(tarif, num_facture)
          dftmp7 <- c(code, as.character(df[id,9]), "C20", compte, "", idCustomer, num_facture,df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", tarif, "G","","","","CHORS GROUPE","","","","", "")
          xl<- rbind(xl,dftmp7)
          dftmp7 <- c(code, as.character(df[id,9]), "C20", compte, "", idCustomer, num_facture,df[id,3], paste(df$billing.first_name[[id]], df$billing.last_name[[id]], df$billing.company[[id]]), "", tarif, "A","","3","10700","CHORS GROUPE","","","","", "")
          xl<- rbind(xl,dftmp7)
        }
      }

  

  }
  
  id <- id + 1
  idCustomer <- idCustomer + 1
}

colnames(xl) <- list("Code journal", "Date écriture", "Code TVA", "Cpt Général", "Compte tiers", "N° écriture SAGE", "Numéro facture","Numéro de commande", "Libellé écriture", "Débit euro", "Crédit euro", "Type écriture", "Lettrage", "Section", "Analytique", "Interco", "Devise", "Taux change", "Montant devise", "Date échéance"  )
xl[["Date écriture"]] <- sapply(xl[["Date écriture"]], function(x) '09/03/2023')
xl[["Date échéance"]] <- sapply(xl[["Date échéance"]], function(x) format(as.Date(x, "%Y-%m-%dT%H:%M:%S"), "%d/%m/%Y"))






write_xlsx(xl, paste0(getwd(),'/woocommerce.xlsx'))