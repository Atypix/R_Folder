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

start <- '2021-06-01'
end <- '2021-07-05'

page <- 1
date_range <- paste0("&after=",start,"T00:00:00-05:00&before=",end,"T00:00:00-05:00")

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
dfEnd <-  data.frame()
id <- 1
for(p in 1:nrow(df)) {
  message(df$number[[id]])
  # for(q in df[p,33]) {
  #   message(q[2])
  # }
  # message(df[p]$metadata)
  id <- id + 1
}
