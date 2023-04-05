library('jsonlite')
library('writexl')
library('httr')
client <- 'ck_ad8ee3b61231e1e894312e59c39f1618e8cc68f3'
secret <- 'cs_635ca972f3eb343d0bee2c8aeaab3088bfd81cda'

date_range <- "&after=2020-02-13T00:00:00-05:00&before=2020-03-30T00:00:00-05:00"
orders <- data.frame()

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
id <- 1

View(df)
op <- options(stringsAsFactors=F) 

for(p in 1:nrow(df)) {
  
  if (is.na(df[p, 30])) {
    
    if (df[p,7] != "cancelled" && df[p,7] != 'failed' && df[p,7] != 'pending' && df[p,7] != 'completed' && df[p,7] != 'refunded') {
      message(df[p,7])
      b2 <- paste0("https://ecocup.com/fr/wp-json/wc/v2/orders/", df[p,1], "?consumer_key=", client,'&consumer_secret=',secret)
      #jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders/", df[id,1], "?status=completed&consumer_key=", client,'&consumer_secret=',secret), flatten = TRUE)
      test <- PUT(b2, body = list('status'= "completed"), encode = "json")
      
    }
   
  }
  
}
message('-------------------------------------------')
for(p in 1:nrow(df)) {
  
  if (df[p,7] != "cancelled" && df[p,7] != 'failed' && df[p,7] != 'pending' && df[p,7] != 'refunded' && df[p,7] != 'delivery-final') {
    message(df[p,7])
    b2 <- paste0("https://ecocup.com/fr/wp-json/wc/v2/orders/", df[p,1], "?consumer_key=", client,'&consumer_secret=',secret)
    #jsonlite::fromJSON(txt = paste0("https://ecocup.com/fr/wp-json/wc/v2/orders/", df[id,1], "?status=completed&consumer_key=", client,'&consumer_secret=',secret), flatten = TRUE)
    test <- PUT(b2, body = list('status'= "delivery-final"), encode = "json")
    
  }
  
}