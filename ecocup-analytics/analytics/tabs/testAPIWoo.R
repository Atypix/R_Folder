library('jsonlite')



prestation <- jsonlite::fromJSON(txt = paste0("https://staging.ecocup.com/fr/wp-json/wc/v2/orders/","39449", "?consumer_key=ck_b02a4d599d29b1875b968df31f8cc9f6c2261840&consumer_secret=cs_04993e56d36ce93186af5f9869940e5b73fe623f"), flatten = TRUE)

View(prestation)
