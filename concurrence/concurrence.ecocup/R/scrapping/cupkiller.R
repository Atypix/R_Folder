library("dplyr")
library('rvest')
library('stringr')
library('V8')
library('ggplot2')
library('plotly')
library('writexl')
library('RSelenium')
library('wdman')
library('tidyr')
library('reshape2')

message('***************************************************')
message('            CHARGEMENT DE CUPKILLER')
message('***************************************************')

pricetounit <- function (df,a,b) {
  a <- df[, a]
  b <- df[, b]
  b/a
}

csv <- read.csv(file="data/cupkiller.csv", header=TRUE, sep=";",encoding = "UTF-8")
#Shell('docker run -d -p 4445:4444 --shm-size 2g selenium/standalone-firefox')
message('Pensez au demarage du docker seleneium 2g port 4445 -> docker run -d -p 4445:4444 --shm-size 2g selenium/standalone-firefox')
plotprice <- c()
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox")
remDr$open(silent=T)
message('ouverture du remoteServer avec RSelenium : browserName -> firefox')



id <- 0
for(url in csv[,c(4)]) {
  id <- id+1
  #url <- 'https://cupkiller.com/fr/gobelet-a-personnaliser-25-cl-impression-quadri-multicolore/1084-gobelet-personnalise-qualite-photo-25cl.html'
  out <- tryCatch(
    {
       # You dont need to use the open method
      remDr$navigate(url)
      #read_html(url)

    },
    error=function(cond) {
      message(paste("L'URL n'existe pas :", url))
      return("ERROR")
    },
    warning=function(cond) {
      message(paste("Warning sur l'URL :", url))
      return("WARNING")
    },
    finally={

      message(paste("Chargement URL :", url))


    }
  )


   Sys.sleep(5)
   grille <- c()
   tarif <- remDr$findElement(using = 'class', value = 'tablepricetable')
   results <- tarif$getElementText()
   results_chr <- unlist(strsplit(results[[1]], "\n"))

   x <- data.frame(qty=results_chr)
   x <- x[-1,]
   x <- sapply(x, function(x) gsub(",", ".", x))

   rexp <- "^(\\w+)\\s?(.*)$"
   priceTemp = sub(rexp,"\\2",x)
   priceTempSplit <- strsplit(priceTemp," €")

  y <- c()
  leg <- c()
  leg <- append(leg, paste0("cupkiller-",csv[id,c(1)],"-",csv[id,c(2)],"-",csv[id,c(3)],'j' ))

  if (length(priceTempSplit[[1]]) == 3 ) {
    for(p in 1:length(priceTempSplit)) {
      tmpy <- data.frame(qty=as.numeric(sub(rexp,"\\1",x)[p]), price=priceTempSplit[[p]][1], legende=paste0("cupkiller-",csv[id,c(1)],"-",csv[id,c(2)],"-",'2j' ), type=paste(csv[id,c(1)]), impression=paste(csv[id,c(2)]), delai=2, site="cupkiller")
      y <- rbind(y, tmpy)
    }
    for(p in 1:length(priceTempSplit)) {
      tmpy <- data.frame(qty=as.numeric(sub(rexp,"\\1",x)[p]), price=priceTempSplit[[p]][2], legende=paste0("cupkiller-",csv[id,c(1)],"-",csv[id,c(2)],"-",'8j' ), type=paste(csv[id,c(1)]), impression=paste(csv[id,c(2)]), delai=8,  site="cupkiller")
      y <- rbind(y, tmpy)
    }
    for(p in 1:length(priceTempSplit)) {
      tmpy <- data.frame(qty=as.numeric(sub(rexp,"\\1",x)[p]), price=priceTempSplit[[p]][3], legende=paste0("cupkiller-",csv[id,c(1)],"-",csv[id,c(2)],"-",'20j' ), type=paste(csv[id,c(1)]), impression=paste(csv[id,c(2)]), delai=20, site="cupkiller")
      y <- rbind(y, tmpy)
    }

  } else {
    y <- data.frame(qty=as.numeric(sub(rexp,"\\1",x)), price=sub(rexp,"\\2",x), legende=leg, type=csv[id,c(1)], impression=paste(csv[id,c(2)]), delai=csv[id,c(3)] , site="cupkiller")
  }

   y$price <- sapply(y$price, function(x) gsub(" €", "", x))
   y$price <- sapply(y$price, function(x) gsub(" ", "", x))
   y$price <- sapply(y$price, function(x) as.numeric(x))
   unit <- pricetounit(y, "qty", "price")
   y$unit=unit
   y <- subset(y, select=c(qty,price, unit, legende, type, impression, delai, site))
   row.names(y) <- 1 : length(y$qty)

   plotprice <- rbind(plotprice, y)


}

main.addGrille(plotprice, "cupkiller")

plotdelai <- data.frame(delai=c(), impression=c(), site=c())
serigraphie <- filter (plotprice, grepl("serigraphie",plotprice$impression))
test <- serigraphie
View(test)
serigraphie <- serigraphie %>% filter(serigraphie$qty == 2000)
serigraphie <- serigraphie  %>% summarise(delai = mean(as.numeric(delai)))
serigraphie$impression <- 'serigraphie'
serigraphie$site <- 'cupkiller'
plotdelai <- rbind(plotdelai, serigraphie)

digital <- filter (plotprice, grepl("digital",plotprice$impression))
digital <- digital %>% filter(digital$qty == 150)
digital <- digital  %>% summarise(delai = mean(as.numeric(delai), na.rm=TRUE))
digital$impression <- 'digital'
digital$site <- 'cupkiller'
plotdelai <- rbind(plotdelai, digital)

IML <- filter (plotprice, grepl("IML",plotprice$impression))
IML <- IML %>% filter(IML$qty == 1200)
IML <- IML  %>% summarise(delai = mean(as.numeric(delai), na.rm=TRUE))
IML$impression <- 'IML'
IML$site <- 'cupkiller'
plotdelai <- rbind(plotdelai, IML)

vierge <- filter (plotprice, grepl("vierge",plotprice$impression))
vierge <- vierge %>% filter(vierge$qty == 1000)
vierge <- vierge  %>% summarise(delai = mean(as.numeric(delai), na.rm=TRUE))
vierge$impression <- 'vierge'
vierge$site <- 'cupkiller'
plotdelai <- rbind(plotdelai, vierge)
plotdelai$delai <- sapply(plotdelai$delai, function(x) gsub("NaN", 0, x))
main.addDelai (plotdelai, 'cupkiller')


message('fermeture du remoteServer avec RSelenium : browserName -> firefox')
remDr$close()





