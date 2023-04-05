library("dplyr")
library('rvest')
library('stringr')
library('V8')
library('ggplot2')
library('plotly')
library('writexl')

message('***************************************************')
message('              CHARGEMENT DE WOBZ')
message('***************************************************')

csv <- read.csv(file="data/wobz.csv", header=TRUE, sep=";",encoding = "UTF-8")

plotprice <- c()
id <- 0
unitoprice <- function (df,a,b) {
  a <- df[, a]
  b <- df[, b]
  b*a
}
for(url in csv[,c(4)]) {

  id <- id+1
  out <- tryCatch(
    {
      read_html(url)

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

  firsttarif <- out %>% html_node(xpath='//*[@class="price"]') %>% html_text()

  firstqty <- out %>% html_node(xpath='//*[@class="qtyMinimum"]/span[3]/strong[1]') %>% html_text()

  tariftable <- out %>% html_node(xpath='//*[@id="zoo-main-content"]/div[2]/div[2]/div[2]/div/div[1]/div[1]/div[1]/div[2]/div[6]/ul/table') %>% html_table()
  tarif = t(tariftable)
  tarif <- tarif[-1,]
  tarifdf <- data.frame(rbind(tarif))
  tarifdf <- select(tarifdf, -3)
  names(tarifdf)[1]<-paste("qty")
  names(tarifdf)[2]<-paste("unit")
  row.names(tarifdf) <- 1 : length(tarifdf$qty)

  y <- data.frame (qty=firstqty, unit=firsttarif)
  y <- rbind(y, tarifdf)
  y$unit <- sapply(y$unit, function(x) gsub("€", "", x))
  y$unit <- sapply(y$unit, function(x) gsub("\u00A0", "", x, fixed =TRUE))
  y$unit <- sapply(y$unit, function(x) gsub(" ", "", x))
  y$unit <- sapply(y$unit, function(x) gsub(",", ".", x))
  y$unit <- sapply(y$unit, function(x) as.numeric(x))
  y$qty <- sapply(y$qty, function(x) gsub(" ", "", x))
  y$qty <- sapply(y$qty, function(x) as.numeric(x))

  price <- unitoprice(y, "qty", "unit")
  y$price=price
  leg <- c()
  leg <- append(leg, paste0('wobz-',csv[id,c(1)],"-",csv[id,c(2)],"-",csv[id,c(3)],'j' ))
  y$legende =leg
  type <- c()
  type <- append(type, paste(csv[id,c(1)]))
  y$type =type
  impression <- c()
  impression <- append(impression, paste(csv[id,c(2)]))
  y$impression =impression
  delai <- c()
  delai <- append(delai, paste(csv[id,c(3)]))
  y$delai =delai
  plotprice <- rbind(plotprice, y)


}
site <- c()
plotprice$site = site
plotprice$site <- "wobz"

plotdelai <- data.frame(delai=c(), impression=c(), site=c())
serigraphie <- filter (plotprice, grepl("serigraphie",tolower(plotprice$impression)))

serigraphie <- serigraphie %>% filter(serigraphie$qty == 1000)

serigraphie <- serigraphie  %>% summarise(delai = mean(as.numeric(delai)))
View(serigraphie)
serigraphie$impression <- 'serigraphie'
serigraphie$site <- 'wobz'
plotdelai <- rbind(plotdelai, serigraphie)

digital <- filter (plotprice, grepl("digital",tolower(plotprice$impression)))

digital <- digital %>% filter(digital$qty == 500)
digital <- digital  %>% summarise(delai = mean(as.numeric(delai), na.rm=TRUE))
digital$impression <- 'digital'
digital$site <- 'wobz'
plotdelai <- rbind(plotdelai, digital)

IML <- filter (plotprice, grepl("iml",tolower(plotprice$impression)))
IML <- IML %>% filter(IML$qty == 1000)
IML <- IML  %>% summarise(delai = mean(as.numeric(delai), na.rm=TRUE))
IML$impression <- 'IML'
IML$site <- 'wobz'
plotdelai <- rbind(plotdelai, IML)

vierge <- filter (plotprice, grepl("vierge",tolower(plotprice$impression)))
vierge <- vierge %>% filter(vierge$qty == 1000)
vierge <- vierge  %>% summarise(delai = mean(as.numeric(delai), na.rm=TRUE))
vierge$impression <- 'vierge'
vierge$site <- 'wobz'
plotdelai <- rbind(plotdelai, vierge)
plotdelai$delai <- sapply(plotdelai$delai, function(x) gsub("NaN", 0, x))


main.addGrille(plotprice, 'wobz')

main.addDelai (plotdelai, 'wobz')




