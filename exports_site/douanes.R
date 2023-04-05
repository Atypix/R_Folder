#setwd('/home/goach/Documents/workspace/R_Folder/exports_site/')

rm(list=ls())
library('lubridate')
library('dplyr')
library('jsonlite')
library('writexl')
library('stringr')
library('tidyr')
library('xlsx')



xl <- data.frame ()
client <- 'ck_ad8ee3b61231e1e894312e59c39f1618e8cc68f3'
secret <- 'cs_635ca972f3eb343d0bee2c8aeaab3088bfd81cda'

start <- '2020-02-13'
end <- '2021-10-01'

start_before <- as.Date(start)-months(2)

csv <- read.csv("commande_web.csv", header = FALSE, sep = ";")
commandes <- csv[12]
colnames(commandes) <- list("number")



page <- 1
date_range <- paste0("&after=",start_before,"T00:00:00-05:00&before=",end,"T00:00:00-05:00")

bool <- TRUE

df <- data.frame()
dforders <- data.frame()

while(bool == TRUE) {
  
  url <- paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range, "&page=",page)
  message (url)
  
  orders <- jsonlite::fromJSON(txt = url, flatten = TRUE)
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
dforders <- data.frame()



for (p in df[,1]) {
  country <- df$billing.country[[id]]
  last_name <- df$billing.last_name[[id]]
  tva <-  df$total_tax[[id]]
  line_facture <- df$meta_data[[id]] %>% filter (key == '_invoice_number_display') 
  lines <- df$line_items[[id]]
 
  if (nrow(line_facture)>0) {
    num_facture <- line_facture$value[1]
    
  } else {
    num_facture <- df[id,3]
  }
  shippinglines <- df$meta_data[[id]]
  num_tva <- shippinglines %>% filter (str_detect(key, '_billing_numro_de_tva'))
  num_tva_client <- num_tva$value
  df$num_facture = num_facture
  lines <- df$line_items[[id]]
  idLine <- 1
  for(s in lines[,1]) {
    
    quantity <- lines[idLine,5]
    subtotal <- lines[idLine,7]
    tax <- lines[idLine,8]
    
    dftmp <- data.frame()
    dftmp <- c(last_name, num_facture, country, num_tva_client, quantity, subtotal, tax )

    if (id == 1) {
      
      dforders <-  as.data.frame(t(dftmp))
    } else {
      dforders <- rbind(dforders,as.data.frame(t(dftmp)))
      
    }
    
    
    idLine <- idLine + 1
  }
  id <- id + 1
  
}

write.xlsx(as.data.frame(dforders), paste0(getwd(),'/douane2.xlsx'))




