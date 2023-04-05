library('jsonlite')
library('writexl')
client <- 'ck_ad8ee3b61231e1e894312e59c39f1618e8cc68f3'
secret <- 'cs_635ca972f3eb343d0bee2c8aeaab3088bfd81cda'

date_range <- "&after=2020-02-13T00:00:00-05:00&before=2020-07-01T00:00:00-05:00"
orders <- data.frame()
# df <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=2&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=3&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=4&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=5&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=6&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=7&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=8&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=9&per_page=100&status=delivery-final&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=1&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=2&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=3&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=4&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=5&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=6&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=7&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=8&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)
# df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=9&per_page=100&status=completed&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
# df <- rbind(df, df2)

df <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=2&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=3&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=4&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=5&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=6&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=7&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=8&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=9&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=10&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=11&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=12&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=13&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=14&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=15&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=16&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=17&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=18&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=19&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
df2 <- jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders?page=20&per_page=100&consumer_key=", client, "&consumer_secret=",secret,date_range), flatten = TRUE)
df <- rbind(df, df2)
id <- 1
op <- options(stringsAsFactors=F) 


for(p in 1:nrow(df)) {
  for(q in df[p,34]) {
   
    for(r in 1:length(q$quantity)) {
      
      dftmp <- c(df[p,3], df[p,7], df[p,9], df[p,31], df[p,42], df[p,41], 'ECO30', q['name'][r,], q['quantity'][r,], q['price'][r,], q['total'][r,], q['total_tax'][r,], (as.numeric(q['total'][r,]) + as.numeric(q['total_tax'][r,]) ))
      dftmp <- unname(dftmp)
      orders <- rbind(orders,dftmp)
    }
    
  }
}
names(orders)[1] <- 'N° de facture'
names(orders)[2] <- 'Statut commande'
names(orders)[3] <- 'Date de la commande'
names(orders)[4] <- 'Date de la facture'
names(orders)[5] <- 'Nom'
names(orders)[6] <- 'Prénom'
names(orders)[7] <- 'Code produit'
names(orders)[8] <- 'Désignation produit'
names(orders)[9] <- 'Quantité'
names(orders)[10] <- 'Prix unitaire'
names(orders)[11] <- 'Montant HT'
names(orders)[12] <- 'TVA'
names(orders)[13] <- 'Montant TTC'
View (orders)

write_xlsx(orders, paste0(getwd(),'/orders_all_with_status.xlsx'))