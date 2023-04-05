





location <- function () {
  
  
}

location.transformByPeriod <- function (periode, dfGraph) {
  
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



location.getCount <- function (dfLocation, value) {
  
  value <- append(value, "Total")
  df <- jsonlite::fromJSON(dfLocation)
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



location.createGraph <- function (dfLocation, periode, value) {

  dfGraph <- jsonlite::fromJSON(dfLocation)
  dfGraph <- location.transformByPeriod (periode, dfGraph)
  
  
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
  #dfGraph$cdate <- sapply( dfGraph$cdate, function(x) ISOweek(as.Date(x,origin="1970-01-01")))
  #write_xlsx(dfGraph, paste0(getwd(),'/export-devis-location.xlsx')) 
  
  
}

location.createGraphMontant <- function(value, periode, jsonLocation, location_id) {
  
  dfGraphMontant <- jsonlite::fromJSON(jsonLocation)
  
  
  dfGraphMontant$id <- NULL
  dfGraphMontant$numlocation <- NULL
  
  
  dfGraphMontant <- dfGraphMontant[(dfGraphMontant$id_entite %in% value), ]
  dfGraphMontant$id_entite <- NULL
  
  dfGraphMontant <- dfGraphMontant %>% group_by(cdate, etat) %>% summarise(montant = sum(as.numeric(montant)))
  
  dfGraphMontant <- dcast(dfGraphMontant, cdate ~ etat, value.var="montant", fill=0)
  
  dfGraphMontant <- location.transformByPeriod (periode, dfGraphMontant)
  
  
  dfGraphMontant <- dfGraphMontant %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraphMontant)) %>% rowSums(na.rm = TRUE))
  
  write_xlsx(dfGraphMontant, paste0(getwd(),'/../../www/html/location-periode-statut-', location_id,'.xlsx'))
  
  figMontant <- plot_ly(dfGraphMontant, x = as.Date(dfGraphMontant$cdate,origin="1970-01-01"))
  
  val <- c("Validé", "Refusé", "En attente", "Dépassé")
  
  for(p in val) {
    figMontant <- figMontant %>% add_trace(y = dfGraphMontant[[as.character(p)]], name = as.character(p), type = 'bar', hovertemplate = 'Montant HT: %{y:.2f } €<extra>%{x}</extra>')
  }
  
  
  
  figMontant <- figMontant %>% add_trace(y = dfGraphMontant$Total, name = "Total",mode = 'lines+markers', hovertemplate = 'Montant HT: %{y:.2f} €<extra>%{x}</extra>')
  
  figMontant <- figMontant %>% layout(legend = list(orientation = 'h'))
  figMontant <- figMontant %>% layout(yaxis = list(title = 'Euros'), barmode = 'stack')
  
  return(figMontant)
  
}

location.createGraphMontantEntite <- function(value, periode, jsonLocation, location_id) {
  
  dfGraphMontantEntite <- jsonlite::fromJSON(jsonLocation)
  
  dfGraphMontantEntite$id <- NULL
  dfGraphMontantEntite$numlocation <- NULL
  
  
  dfGraphMontantEntite <- dfGraphMontantEntite[(dfGraphMontantEntite$id_entite %in% value), ]
  dfGraphMontantEntite$etat <- NULL
  
  dfGraphMontantEntite <- dfGraphMontantEntite %>% group_by(cdate, id_entite) %>% summarise(montant = sum(as.numeric(montant)))
  
  dfGraphMontantEntite <- dcast(dfGraphMontantEntite, cdate ~ id_entite, value.var="montant", fill=0)
  
  dfGraphMontantEntite <- location.transformByPeriod (periode, dfGraphMontantEntite)
  
  dfGraphMontantEntite <- dfGraphMontantEntite %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraphMontantEntite)) %>% rowSums(na.rm = TRUE))
  
  write_xlsx(dfGraphMontantEntite, paste0(getwd(),'/../../www/html/location-periode-entite-', location_id,'.xlsx'))
  
  figMontantEntite <- plot_ly(dfGraphMontantEntite, x = as.Date(dfGraphMontantEntite$cdate,origin="1970-01-01"))
  
  
  
  for(p in value) {
    figMontantEntite <- figMontantEntite %>% add_trace(y = dfGraphMontantEntite[[as.character(p)]], name = as.character(p), type = 'bar', hovertemplate = paste0('Montant HT: %{y:.2f} €<extra>',p, '</extra>'))
  }
  
  figMontantEntite <- figMontantEntite %>% add_trace(y = dfGraphMontantEntite$Total, name = "Total",mode = 'lines+markers', hovertemplate = paste0('Montant HT: %{y:.2f} €<extra>Total</extra>'))
  
  figMontantEntite <- figMontantEntite %>% layout(legend = list(orientation = 'h'))
  figMontantEntite <- figMontantEntite %>% layout(yaxis = list(title = 'Euros'), barmode = 'stack')
  
  return(figMontantEntite)
  
}



location.getlocation <- function (dfLocation, value, periode, location_id) {
  
  value <- append(value, "Total")
  value <- append(value, "cdate")
  
  df <- jsonlite::fromJSON(dfLocation)
  
  
  df <- df[ , (names(df) %in% value)]
  
  if (periode == 'W') {
    
    df$cdate <- sapply( df$cdate, function(x) ISOweek(as.Date(x,origin="1970-01-01")))
    
  } else if (periode == "M") {
    
    df$cdate <- sapply(df$cdate, function(x) format(as.Date(x, origin="1970-01-01", format = "%Y-%m-%d"), '%Y-%m'))
    
  }
  
  write_xlsx(df, paste0(getwd(),'/../../www/html/location-', location_id,'.xlsx'))
  
  
  return(df)
  
}

location.createPieStatut <- function (jsonLocation, value, location_id) {
  
  dfStatut <- jsonlite::fromJSON(jsonLocation)
  value <- append(value, "Total")
  value <- append(value, "cdate")
  
  dfStatut <- dfStatut[(dfStatut$id_entite %in% value), ]
  
  dfStatut$id <- NULL
  dfStatut$numlocation <- NULL
  dfStatut$cdate <- NULL
  dfStatut$id_entite <- NULL
  
  dfStatut <- dfStatut %>% group_by(etat) %>% summarise(count=length(etat))
  
  write_xlsx(dfStatut, paste0(getwd(),'/../../www/html/location-statut-', location_id,'.xlsx')) 
  
  figPieStatut <- dfStatut %>% plot_ly(labels = ~etat, values = ~count)
  figPieStatut <- figPieStatut %>% add_pie(hole = 0.5)
  figPieStatut <- figPieStatut %>% layout(
    showlegend = T,
    legend = list(orientation = 'h'),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(figPieStatut)
  
}

location.createPie <- function(dfLocation, value, location_id) {
  
  dfPie <- jsonlite::fromJSON(dfLocation)
  dfPie <- dfPie[ , (names(dfPie) %in% value)]
  dfPie <- as.data.frame(t(dfPie))
  
  
  dfPie[] <- lapply(dfPie, function(x) { if(is.factor(x)) as.numeric(as.character(x)) else x})
  
  dfPie <- rowSums(dfPie[, 1:length(dfPie), drop = TRUE])
  
  dfPie <- tibble::rownames_to_column(as.data.frame(dfPie), "entite")
  
  names(dfPie)[2]<-paste("num")
  
  write_xlsx(dfPie, paste0(getwd(),'/../../www/html/location-pie-', location_id,'.xlsx'))  
  
  figPie <- dfPie %>% plot_ly(labels = ~entite, values = ~num)
  figPie <- figPie %>% add_pie(hole = 0.5)
  figPie <- figPie %>% layout(
    showlegend = T,
    legend = list(orientation = 'h'),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  
  
}



location.createTotaux <- function(dfLocation, value, location_id) {
  
  dfTotaux <- jsonlite::fromJSON(dfLocation)
  dfTotaux <- dfTotaux[ , (names(dfTotaux) %in% value)]
  dfTotaux <- as.data.frame(t(dfTotaux))
  
  
  dfTotaux[] <- lapply(dfTotaux, function(x) { if(is.factor(x)) as.numeric(as.character(x)) else x})
  
  dfTotaux <- rowSums(dfTotaux[, 1:length(dfTotaux), drop = TRUE])
  dfTotaux <- tibble::rownames_to_column(as.data.frame(dfTotaux), "entite")
  
  names(dfTotaux)[2]<-paste("num")
  
  write_xlsx(dfTotaux, paste0(getwd(),'/../../www/html/location-entite-', location_id,'.xlsx')) 
  
  figTotaux <- plot_ly(x = dfTotaux$num, y = dfTotaux$entite, type = 'bar', orientation = 'h', hovertemplate = 'Quantité: %{x:.0f}<extra>%{y}</extra>')
  
  
  
}






app$callback(
  output = list(id='principal-graph-location', property='figure'),
  params = list(input(id='dfLocation-value', property='children'),
                input(id='periode-value', property='children'),
                input(id='filtre-location', property='value')),
  function (dfLocation, periode_value, value) {
    periode <- jsonlite::fromJSON(periode_value)
    return (location.createGraph(dfLocation, periode, value))
  }
  
)

app$callback(
  output = list(id='count-location', property='children'),
  params = list(input(id='dfLocation-value', property='children'),
                input(id='filtre-location', property='value')),
  function (children, value) {
    sprintf (location.getCount(children, value))
  }
  
)


app$callback(
  output = list(id='pie-location', property='figure'),
  params = list(input(id='dfLocation-value', property='children'),
                input(id='filtre-location', property='value'),
                input(id='location-id', property='children'),
                input(id='principal-graph-location', property='figure')),
  function (dfLocation, value, location_id, fig) {
    return (location.createPie(dfLocation, value, location_id))
  }
  
)

app$callback(
  output = list(id='totaux-location', property='figure'),
  params = list(input(id='dfLocation-value', property='children'),
                input(id='filtre-location', property='value'),
                input(id='location-id', property='children'),
                input(id='pie-location', property='figure')),
  function (dfLocation, value, location_id, fig) {
    return (location.createTotaux(dfLocation, value, location_id))
  }
  
)

app$callback(
  output = list(id='pie-statut-location', property='figure'),
  params = list(input(id='jsonLocation-value', property='children'),
                input(id='filtre-location', property='value'),
                input(id='location-id', property='children'),
                input(id='totaux-location', property='figure')),
  function (jsonLocation, value, location_id, fig) {
    return (location.createPieStatut(jsonLocation, value, location_id))
  }
  
)


app$callback(
  output = list(id='graph-location-montant', property='figure'),
  params = list(input(id='filtre-location', property='value'),
                input(id='periode-value', property='children'),
                input(id='jsonLocation-value', property='children'),
                input(id='location-id', property='children'),
                input(id='pie-statut-location', property='figure')),
  function (value, periode_value, jsonLocation, location_id, fig) {
    periode <- jsonlite::fromJSON(periode_value)
    return (location.createGraphMontant(value, periode, jsonLocation, location_id))
  }
  
)

app$callback(
  output = list(id='graph-location-montant-entite', property='figure'),
  params = list(input(id='filtre-location', property='value'),
                input(id='periode-value', property='children'),
                input(id='jsonLocation-value', property='children'),
                input(id='location-id', property='children'),
                input(id='graph-location-montant', property='figure')),
  function (value, periode_value, jsonLocation, location_id, fig) {
    periode <- jsonlite::fromJSON(periode_value)
    return (location.createGraphMontantEntite(value, periode, jsonLocation, location_id))
  }
  
)










app$callback(
  output = list(id='table-location', property='columns'),
  params = list(input(id='dfLocation-value', property='children'),
                input(id='filtre-location', property='value'),
                input(id='periode-value', property='children'),
                input(id='location-id', property='children'),
                input(id='graph-location-montant-entite', property='figure')),
  function (children, value, valuePeriode, location_id, fig) {
    periode <- jsonlite::fromJSON(valuePeriode)
    columns = lapply(colnames(location.getlocation(children, value, periode, location_id)), 
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
  output = list(id='table-location', property='data'),
  params = list(input(id='dfLocation-value', property='children'),
                input(id='filtre-location', property='value'),
                input(id='periode-value', property='children'),
                input(id='location-id', property='children')),
  function (children, value, valuePeriode, location_id) {
    periode <- jsonlite::fromJSON(valuePeriode)
    return (location.getlocation(children, value, periode, location_id))
  }
  
)

app$callback(
  output=list(id='location-id', property='children'),
  params = list(input(id = 'startDate-value', property = 'children'),
                input(id = 'endDate-value', property = 'children'),
                input(id='filtre-location', property='value')),
  function(startDate_value, endDate_value, value) {
    return(sample(1:100000000, 1))
  }
)

app$callback(
  output=list(id='location-download', property='children'),
  params = list(input(id='location-id', property='children')),
  function(location_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/location-',  location_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='location-download-pie', property='children'),
  params = list(input(id='location-id', property='children')),
  function(location_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/location-pie-',  location_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='location-download-entite', property='children'),
  params = list(input(id='location-id', property='children')),
  function(location_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/location-entite-',  location_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='location-download-statut', property='children'),
  params = list(input(id='location-id', property='children')),
  function(location_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/location-statut-',  location_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='location-download-periode-statut', property='children'),
  params = list(input(id='location-id', property='children')),
  function(location_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/location-periode-statut-',  location_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='location-download-periode-entite', property='children'),
  params = list(input(id='location-id', property='children')),
  function(location_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/location-periode-entite-',  location_id,'.xlsx)')))
  }
)






location ()







