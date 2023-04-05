rm(list=ls())

library("dplyr")
library('rvest')
library('stringr')
library('V8')
library('ggplot2')
library('plotly')
library('writexl')

sites <-c('serigraphie', 'digital', 'IML', 'vierge')

dfPlot <- data.frame(qty=c(), price=c(), unit=c(), legende=c(), type=c(), impression=c(), delai=c(), site=c())
dfDelai <- data.frame(impression=sites, atelierdugobelet=sites, cupkiller=sites, wobz=sites)



main <- function () {

}

main.addGrille <- function (grille, site) {

  dfPlot <<- rbind(dfPlot, grille)

  strpath <- paste0(getwd(),'/graphs/tarifs/', site,"_prix_qty.html" )

  q <- ggplotly(ggplot(data=grille, aes(x=qty, y=price, colour=legende)) + geom_point(size=0.5)+ geom_line(size=0.2))
  htmlwidgets::saveWidget(as_widget(q), strpath)

  strpath <- paste0(getwd(),'/graphs/tarifs/', site,"_prixunitaires_qty.html" )
  r <- ggplotly(ggplot(data=grille, aes(x=qty, y=unit, colour=legende)) + geom_point(size=0.5)+ geom_line(size=0.2))
  htmlwidgets::saveWidget(as_widget(r), strpath)

  s <- ggplotly(ggplot(data=dfPlot, aes(x=qty, y=price, color=legende)) + geom_point(size=0.5)+ geom_line(size=0.2))
  htmlwidgets::saveWidget(as_widget(s), paste0(getwd(),"/graphs/tarifs/total_graph_prix_qty.html"))

  strxlsx <- paste0(getwd(),'/xlsx/',site,'.xlsx')
  write_xlsx(grille, strxlsx)

  write_xlsx(grille, paste0(getwd(),'/xlsx/scrapping.xlsx'))

}

main.addDelai <- function (plotDelai, site) {

  dfDelai$impression <<- plotDelai$impression
  dfDelai[site] <<- plotDelai$delai
  if (site == "cupkiller") {
    fig <- plot_ly(dfDelai, y = dfDelai$impression, x = as.numeric(dfDelai$atelierdugobelet), type = 'bar', name = 'atelierdugobelet')
    fig <- fig %>% add_trace(x = as.numeric(dfDelai$wobz), name = 'wobz')
    fig <- fig %>% add_trace(x = as.numeric(dfDelai$cupkiller), name = 'cupkiller')
    fig <- fig %>% layout(xaxis = list(title = 'Jours'), barmode = 'group')
    r <- ggplotly(fig)
    htmlwidgets::saveWidget(as_widget(r), paste0(getwd(),"/graphs/tarifs/total_delai_impression.html"))
  }


}



scrappe <- function () {

  source('R/scrapping/atelierdugobelet.R')
  source('R/scrapping/wobz.R')
  source('R/scrapping/cupkiller.R')

}


main ()





