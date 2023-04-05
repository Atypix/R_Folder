





devis <- function () {
  
 
}

devis.transformByPeriod <- function (periode, dfGraph) {
  
  if (periode == 'W') {
    
    dfGraph <- dfGraph %>% group_by(week = ISOweek(cdate)) %>% summarise_at(.vars = 2:ncol(dfGraph), sum)
    
    dfGraph$week <- sapply(dfGraph$week, function(x) ISOweek2date(paste0(as.character(x), '-1')))
    names(dfGraph)[1]<-paste("cdate")
    
    
  } else if (periode == "M") {
    
    dfGraph <- dfGraph %>% mutate(month = month(cdate), year = year(cdate)) %>% group_by(month, year) %>% summarise_at(.vars = 2:ncol(dfGraph), sum)
    dfGraph$month <-paste(dfGraph$year, dfGraph$month, "1",sep="-")
    dfGraph$year <- NULL
    dfGraph$month <- sapply(dfGraph$month, function(x) as.Date(x, origin="1970-01-01", format = "%Y-%m-%d"))
    dfGraph <- dfGraph[order(as.Date(dfGraph$month, origin='1970-01-01')),]
    names(dfGraph)[1]<-paste("cdate")
    
  }
  
  return(dfGraph)
}



devis.getCount <- function (dfDevis, value) {
  
  value <- append(value, "Total")
  df <- jsonlite::fromJSON(dfDevis)
  if (length(value) > 2) {
    df <- df[ , (names(df) %in% value)]
  }
  #on recalcule les totaux
  df$Total <- NULL
  
  #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
  df <- df %>% mutate(Total = select(., 1:ncol(df)) %>% rowSums(na.rm = TRUE))

  sumTotal <-sum(df$Total)
  total <- format(sumTotal, digits=9, decimal.mark=",", big.mark=" ",small.mark="", , small.interval=3)
  return (as.character(total))
  
}



devis.createGraph <- function (dfDevis, periode, value) {
  
  dfGraph <- jsonlite::fromJSON(dfDevis)
  dfGraph <- devis.transformByPeriod (periode, dfGraph)

  
  value <- append(value, "Total")
  val <- value
  value <- append(value, "cdate")

  dfFig <- dfGraph
  
  if (length(value) > 2) {
    dfFig <- dfFig[ , (names(dfFig) %in% value)]
  }

  fig <- plot_ly(dfFig, x = as.Date(dfFig$cdate,origin="1970-01-01"))
  
  for(p in val) {

    fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p),mode = 'lines+markers', hovertemplate = 'Quantité: %{y:.0f}<extra>%{x}</extra>')
  
  }
  fig <- fig %>% layout(legend = list(orientation = 'h'))


  
}

devis.createGraphMontant <- function(value, periode, jsonDevis, devis_id) {
  
  dfGraphMontant <- jsonlite::fromJSON(jsonDevis)
  
  
  dfGraphMontant$id <- NULL
  dfGraphMontant$numdevis <- NULL
  
  
  dfGraphMontant <- dfGraphMontant[(dfGraphMontant$id_entite %in% value), ]
  dfGraphMontant$id_entite <- NULL
  
  dfGraphMontant <- dfGraphMontant %>% group_by(cdate, etat) %>% summarise(montant = sum(as.numeric(montant)))
  
  dfGraphMontant <- dcast(dfGraphMontant, cdate ~ etat, value.var="montant", fill=0)
  
  dfGraphMontant <- devis.transformByPeriod (periode, dfGraphMontant)
  
  
  dfGraphMontant <- dfGraphMontant %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraphMontant)) %>% rowSums(na.rm = TRUE))
  
  write_xlsx(dfGraphMontant, paste0(getwd(),'/../../www/html/devis-periode-statut-', devis_id,'.xlsx'))
  
  figMontant <- plot_ly(dfGraphMontant, x = as.Date(dfGraphMontant$cdate,origin="1970-01-01"))
  
  val <- c("Validé", "Refusé", "En attente")
  
  for(p in val) {
    figMontant <- figMontant %>% add_trace(y = dfGraphMontant[[as.character(p)]], name = as.character(p), type = 'bar', hovertemplate = 'Montant HT: %{y:.2f } €<extra>%{x}</extra>')
  }
  
  
  
  figMontant <- figMontant %>% add_trace(y = dfGraphMontant$Total, name = "Total",mode = 'lines+markers', hovertemplate = 'Montant HT: %{y:.2f} €<extra>%{x}</extra>')
  
  figMontant <- figMontant %>% layout(legend = list(orientation = 'h'))
  figMontant <- figMontant %>% layout(yaxis = list(title = 'Euros'), barmode = 'stack')
  
  return(figMontant)
  
}

devis.createGraphMontantEntite <- function(value, periode, jsonDevis, devis_id) {
  
  dfGraphMontantEntite <- jsonlite::fromJSON(jsonDevis)
  
  dfGraphMontantEntite$id <- NULL
  dfGraphMontantEntite$numdevis <- NULL
  
  
  dfGraphMontantEntite <- dfGraphMontantEntite[(dfGraphMontantEntite$id_entite %in% value), ]
  dfGraphMontantEntite$etat <- NULL
  
  dfGraphMontantEntite <- dfGraphMontantEntite %>% group_by(cdate, id_entite) %>% summarise(montant = sum(as.numeric(montant)))
  
  dfGraphMontantEntite <- dcast(dfGraphMontantEntite, cdate ~ id_entite, value.var="montant", fill=0)
  
  dfGraphMontantEntite <- devis.transformByPeriod (periode, dfGraphMontantEntite)
  
  dfGraphMontantEntite <- dfGraphMontantEntite %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraphMontantEntite)) %>% rowSums(na.rm = TRUE))
  
  write_xlsx(dfGraphMontantEntite, paste0(getwd(),'/../../www/html/devis-periode-entite-', devis_id,'.xlsx'))
  
  figMontantEntite <- plot_ly(dfGraphMontantEntite, x = as.Date(dfGraphMontantEntite$cdate,origin="1970-01-01"))
  
  
  
  for(p in value) {
    figMontantEntite <- figMontantEntite %>% add_trace(y = dfGraphMontantEntite[[as.character(p)]], name = as.character(p), type = 'bar', hovertemplate = paste0('Montant HT: %{y:.2f} €<extra>',p, '</extra>'))
  }
  
  figMontantEntite <- figMontantEntite %>% add_trace(y = dfGraphMontantEntite$Total, name = "Total",mode = 'lines+markers', hovertemplate = paste0('Montant HT: %{y:.2f} €<extra>Total</extra>'))
  
  figMontantEntite <- figMontantEntite %>% layout(legend = list(orientation = 'h'))
  figMontantEntite <- figMontantEntite %>% layout(yaxis = list(title = 'Euros'), barmode = 'stack')
  
  return(figMontantEntite)
  
}



devis.getDevis <- function (dfDevis, value, periode, devis_id) {
  
  value <- append(value, "Total")
  value <- append(value, "cdate")
  
  df <- jsonlite::fromJSON(dfDevis)


  df <- df[ , (names(df) %in% value)]
  
  if (periode == 'W') {
    
    df$cdate <- sapply( df$cdate, function(x) ISOweek(as.Date(x,origin="1970-01-01")))
    
  } else if (periode == "M") {
    
    df$cdate <- sapply(df$cdate, function(x) format(as.Date(x, origin="1970-01-01", format = "%Y-%m-%d"), '%Y-%m'))
    
  }
  
  write_xlsx(df, paste0(getwd(),'/../../www/html/devis-', devis_id,'.xlsx'))
  

  return(df)
  
}

devis.createPieStatut <- function (jsonDevis, value, devis_id) {
  
  dfStatut <- jsonlite::fromJSON(jsonDevis)
  value <- append(value, "Total")
  value <- append(value, "cdate")
  
  dfStatut <- dfStatut[(dfStatut$id_entite %in% value), ]
  
  dfStatut$id <- NULL
  dfStatut$numdevis <- NULL
  dfStatut$cdate <- NULL
  dfStatut$id_entite <- NULL
  
  dfStatut <- dfStatut %>% group_by(etat) %>% summarise(count=length(etat))
  
  write_xlsx(dfStatut, paste0(getwd(),'/../../www/html/devis-statut-', devis_id,'.xlsx')) 
  
  figPieStatut <- dfStatut %>% plot_ly(labels = ~etat, values = ~count)
  figPieStatut <- figPieStatut %>% add_pie(hole = 0.5)
  figPieStatut <- figPieStatut %>% layout(
    showlegend = T,
    legend = list(orientation = 'h'),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(figPieStatut)
  
}

devis.createPie <- function(dfDevis, value, devis_id) {
  
  dfPie <- jsonlite::fromJSON(dfDevis)
  dfPie <- dfPie[ , (names(dfPie) %in% value)]
  dfPie <- as.data.frame(t(dfPie))

  
  dfPie[] <- lapply(dfPie, function(x) { if(is.factor(x)) as.numeric(as.character(x)) else x})
  
  dfPie <- rowSums(dfPie[, 1:length(dfPie), drop = TRUE])
  
  dfPie <- tibble::rownames_to_column(as.data.frame(dfPie), "entite")
  
  names(dfPie)[2]<-paste("num")

  write_xlsx(dfPie, paste0(getwd(),'/../../www/html/devis-pie-', devis_id,'.xlsx'))  
  
  figPie <- dfPie %>% plot_ly(labels = ~entite, values = ~num)
  figPie <- figPie %>% add_pie(hole = 0.5)
  figPie <- figPie %>% layout(
                          showlegend = T,
                          legend = list(orientation = 'h'),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  

    

}



devis.createTotaux <- function(dfDevis, value, devis_id) {
  
  dfTotaux <- jsonlite::fromJSON(dfDevis)
  dfTotaux <- dfTotaux[ , (names(dfTotaux) %in% value)]
  dfTotaux <- as.data.frame(t(dfTotaux))
  

  dfTotaux[] <- lapply(dfTotaux, function(x) { if(is.factor(x)) as.numeric(as.character(x)) else x})
  
  dfTotaux <- rowSums(dfTotaux[, 1:length(dfTotaux), drop = TRUE])
  dfTotaux <- tibble::rownames_to_column(as.data.frame(dfTotaux), "entite")

  names(dfTotaux)[2]<-paste("num")
  
  write_xlsx(dfTotaux, paste0(getwd(),'/../../www/html/devis-entite-', devis_id,'.xlsx')) 
  
  figTotaux <- plot_ly(x = dfTotaux$num, y = dfTotaux$entite, type = 'bar', orientation = 'h', hovertemplate = 'Quantité: %{x:.0f}<extra>%{y}</extra>')


  
}






app$callback(
  output = list(id='principal-graph-devis', property='figure'),
  params = list(input(id='dfDevis-value', property='children'),
                input(id='periode-value', property='children'),
                input(id='filtre-devis', property='value')),
  function (dfDevis, periode_value, value) {
    periode <- jsonlite::fromJSON(periode_value)
    return (devis.createGraph(dfDevis, periode, value))
  }
  
)

app$callback(
  output = list(id='count-devis', property='children'),
  params = list(input(id='dfDevis-value', property='children'),
                input(id='filtre-devis', property='value')),
  function (children, value) {
    sprintf (devis.getCount(children, value))
  }
  
)


app$callback(
  output = list(id='pie-devis', property='figure'),
  params = list(input(id='dfDevis-value', property='children'),
                input(id='filtre-devis', property='value'),
                input(id='devis-id', property='children'),
                input(id='principal-graph-devis', property='figure')),
  function (dfDevis, value, devis_id, fig) {
    return (devis.createPie(dfDevis, value, devis_id))
  }
  
)

app$callback(
  output = list(id='totaux-devis', property='figure'),
  params = list(input(id='dfDevis-value', property='children'),
                input(id='filtre-devis', property='value'),
                input(id='devis-id', property='children'),
                input(id='pie-devis', property='figure')),
  function (dfDevis, value, devis_id, fig) {
    return (devis.createTotaux(dfDevis, value, devis_id))
  }
  
)

app$callback(
  output = list(id='pie-statut-devis', property='figure'),
  params = list(input(id='jsonDevis-value', property='children'),
                input(id='filtre-devis', property='value'),
                input(id='devis-id', property='children'),
                input(id='totaux-devis', property='figure')),
  function (jsonDevis, value, devis_id, fig) {
    return (devis.createPieStatut(jsonDevis, value, devis_id))
  }
  
)


app$callback(
  output = list(id='graph-devis-montant', property='figure'),
  params = list(input(id='filtre-devis', property='value'),
                input(id='periode-value', property='children'),
                input(id='jsonDevis-value', property='children'),
                input(id='devis-id', property='children'),
                input(id='pie-statut-devis', property='figure')),
  function (value, periode_value, jsonDevis, devis_id, fig) {
    periode <- jsonlite::fromJSON(periode_value)
    return (devis.createGraphMontant(value, periode, jsonDevis, devis_id))
  }
  
)

app$callback(
  output = list(id='graph-devis-montant-entite', property='figure'),
  params = list(input(id='filtre-devis', property='value'),
                input(id='periode-value', property='children'),
                input(id='jsonDevis-value', property='children'),
                input(id='devis-id', property='children'),
                input(id='graph-devis-montant', property='figure')),
  function (value, periode_value, jsonDevis, devis_id, fig) {
    periode <- jsonlite::fromJSON(periode_value)
    return (devis.createGraphMontantEntite(value, periode, jsonDevis, devis_id))
  }
  
)










app$callback(
  output = list(id='table-devis', property='columns'),
  params = list(input(id='dfDevis-value', property='children'),
                input(id='filtre-devis', property='value'),
                input(id='periode-value', property='children'),
                input(id='devis-id', property='children'),
                input(id='graph-devis-montant-entite', property='figure')),
  function (children, value, valuePeriode, devis_id, fig) {
    periode <- jsonlite::fromJSON(valuePeriode)
    columns = lapply(colnames(devis.getDevis(children, value, periode, devis_id)), 
                     function(colName){
                       list(
                         id = colName,
                         name = colName
                       )
                     })
    return (columns)
  }
  
)

app$callback(
  output = list(id='table-devis', property='data'),
  params = list(input(id='dfDevis-value', property='children'),
                input(id='filtre-devis', property='value'),
                input(id='periode-value', property='children'),
                input(id='devis-id', property='children')),
  function (children, value, valuePeriode, devis_id) {
    periode <- jsonlite::fromJSON(valuePeriode)
     return (devis.getDevis(children, value, periode, devis_id))
  }

)

app$callback(
  output=list(id='devis-id', property='children'),
  params = list(input(id = 'startDate-value', property = 'children'),
                input(id = 'endDate-value', property = 'children'),
                input(id='filtre-devis', property='value')),
  function(startDate_value, endDate_value, value) {
    return(sample(1:100000000, 1))
  }
)

app$callback(
  output=list(id='devis-download', property='children'),
  params = list(input(id='devis-id', property='children')),
  function(devis_id) {
     
     sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/devis-',  devis_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='devis-download-pie', property='children'),
  params = list(input(id='devis-id', property='children')),
  function(devis_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/devis-pie-',  devis_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='devis-download-entite', property='children'),
  params = list(input(id='devis-id', property='children')),
  function(devis_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/devis-entite-',  devis_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='devis-download-statut', property='children'),
  params = list(input(id='devis-id', property='children')),
  function(devis_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/devis-statut-',  devis_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='devis-download-periode-statut', property='children'),
  params = list(input(id='devis-id', property='children')),
  function(devis_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/devis-periode-statut-',  devis_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='devis-download-periode-entite', property='children'),
  params = list(input(id='devis-id', property='children')),
  function(devis_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/devis-periode-entite-',  devis_id,'.xlsx)')))
  }
)


  

  

devis ()







