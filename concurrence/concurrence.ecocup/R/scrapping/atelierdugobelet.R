library("dplyr")
library('rvest')
library('stringr')
library('V8')
library('ggplot2')
library('plotly')
library('writexl')

message('***************************************************')
message('     CHARGEMENT DE L\'ATELIER DU GOBELET')
message('***************************************************')

csv <- read.csv(file="data/atelier.csv", header=TRUE, sep=";",encoding = "UTF-8")

plotprice <- c()
id <- 0

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

  tarif <- out %>% html_node('script') %>% html_text()
  js <- v8()
  js$eval(tarif)
  combinations <- js$get("combinationsFromController")

  qty <- c()
  price <- c()
  grille <- c()
  plotgrille <- c()
  legende <- c()
  unit <- c()
  type <- c()
  impression <- c()
  delai <- c()
  site <- c()

  for(p in combinations) {

    valueqty <- as.numeric(gsub("[a-zA-Z .]", "", p$attributes_values))
    price <- append( price, p$price)
    qty <- append( qty, valueqty)
    prixunitaire <- p$price/valueqty
    unit <- append(unit, prixunitaire)
    legende <- append( legende, paste0('atelierdugobelet-',csv[id,c(1)],"-",csv[id,c(2)],"-",csv[id,c(3)],'j' ))
    type <- append(type, paste(csv[id,c(1)]))
    impression <- append(impression, paste(csv[id,c(2)]))
    delai <- append(delai, csv[id,c(3)])
    site <- append(site,'atelierdugobelet')


  }

  grille <- data.frame(qty, price, unit, legende, type, impression, delai, site)
  plotgrille <- data.frame(qty, price, unit, legende, type, impression, delai, site)
  plotprice <- rbind(plotprice, plotgrille)



}



main.addGrille(plotprice, 'atelierdugobelet')

plotdelai <- data.frame(delai=c(), impression=c(), site=c())
serigraphie <- filter (plotprice, grepl("serigraphie",plotprice$impression))
serigraphie <- serigraphie %>% filter(serigraphie$qty == 100)
serigraphie <- serigraphie  %>% summarise(delai = mean(delai))
serigraphie$impression <- 'serigraphie'
serigraphie$site <- 'atelierdugobelet'
plotdelai <- rbind(plotdelai, serigraphie)

digital <- filter (plotprice, grepl("digital",plotprice$impression))
digital <- digital %>% filter(digital$qty == 100)
digital <- digital  %>% summarise(delai = mean(delai, na.rm=TRUE))
digital$impression <- 'digital'
digital$site <- 'atelierdugobelet'
plotdelai <- rbind(plotdelai, digital)

IML <- filter (plotprice, grepl("IML",plotprice$impression))
IML <- IML %>% filter(IML$qty == 1000)
IML <- IML  %>% summarise(delai = mean(delai, na.rm=TRUE))
IML$impression <- 'IML'
IML$site <- 'atelierdugobelet'
plotdelai <- rbind(plotdelai, IML)

vierge <- filter (plotprice, grepl("vierge",plotprice$impression))
vierge <- vierge %>% filter(vierge$qty == 1000)
vierge <- vierge  %>% summarise(delai = mean(delai, na.rm=TRUE))
vierge$impression <- 'vierge'
vierge$site <- 'atelierdugobelet'
plotdelai <- rbind(plotdelai, vierge)
plotdelai$delai <- sapply(plotdelai$delai, function(x) gsub("NaN", 0, x))


main.addDelai (plotdelai, 'atelierdugobelet')




