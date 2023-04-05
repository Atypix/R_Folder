



facturation <- function () {
  
}


facturation.transformByPeriod <- function (periode, dfGraph) {
  
  if (periode == 'W') {
    
    dfGraph <- dfGraph %>% group_by(week = ISOweek(date)) %>% summarise_at(.vars = 2:ncol(dfGraph), sum)
    
    dfGraph$week <- sapply(dfGraph$week, function(x) ISOweek2date(paste0(as.character(x), '-1')))
    names(dfGraph)[1]<-paste("date")
    
    
  } else if (periode == "M") {
    
    dfGraph <- dfGraph %>% mutate(month = month(date), year = year(date)) %>% group_by(month, year) %>% summarise_at(.vars = 2:ncol(dfGraph), sum)
    
    dfGraph$month <-paste(dfGraph$year, dfGraph$month, "1",sep="-")
    
    dfGraph$year <- NULL
    dfGraph$month <- sapply(dfGraph$month, function(x) as.Date(x, origin="1970-01-01", format = "%Y-%m-%d"))
    
    dfGraph <- dfGraph[order(as.Date(dfGraph$month, origin='1970-01-01')),]
    
    names(dfGraph)[1]<-paste("date")
    
    
  }
  
  return(dfGraph)
}


facturation.transformValueSlider <- function (value) {
  if(value== -5) return (-100000)
  if(value==0) return (0)
  if(value==5) return (100)
  if(value==10) return (500)
  if(value==15) return (1000)
  if(value==20) return (2500)
  if(value==25) return (5000)
  if(value==30) return (10000)
  if(value==35) return (25000)
  if(value==40) return (50000)
  if(value==45) return (100000)
  if(value==50) return (350000)
  
}

facturation.getCount <- function(jsonFacturation) {
  df <- jsonlite::fromJSON(jsonFacturation)


  df <- df %>% summarise(sum(as.numeric(df$montantHT)))
  #on ajoute une colonne Total qui fait la somme des 3 autres colonnes

  sumTotal <-round(df[[1]],2)
  total <- format(sumTotal, digits=9, decimal.mark=",", big.mark=" ",small.mark="", , small.interval=3)
  return (paste(as.character(total), "€"))
  
}



facturation.getCountTVA <- function(jsonFacturation) {
  
  df <- jsonlite::fromJSON(jsonFacturation)

  dfDu <- df %>% summarise(count=sum(as.numeric(as.character(tva))))
  
  sumTotal <- round(dfDu[[1]], 2)
  total <- format(sumTotal, digits=9, decimal.mark=",", big.mark=" ",small.mark="", , small.interval=3)
  return (paste(total, "€"))
  
}

facturation.createGraph <- function (dfFacturation, value, periode) {
  
  dfGraph <- jsonlite::fromJSON(dfFacturation)

  dfGraph$Total <- NULL
  
  dfGraph <- facturation.transformByPeriod (periode, dfGraph)
  
  val <- value
  value <- append(value, "date")
  dfGraph <- dfGraph %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraph)) %>% rowSums(na.rm = TRUE))
  dfFig <- dfGraph
  if (length(value) > 2) {
    dfFig <- dfFig[ , (names(dfFig) %in% value)]
  }

  if (nrow(dfFig) > 0) {
    fig <- plot_ly(dfFig, x = as.Date(dfFig$date,origin="1970-01-01"))
    for(p in val) {
      fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p), type = 'bar', hovertemplate = 'Montant HT: %{y:.2f} €<extra>%{x}</extra>')
    }
    fig <- fig %>% add_trace(y = dfGraph$Total, name = "Total", mode = 'lines+markers', hovertemplate = 'Montant HT: %{y:.2f} €<extra>%{x}</extra>')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig <- fig %>% layout(yaxis = list(title = 'Euros'), barmode = 'stack')
  } else {
    xdate = list(startDate_value, endDate_value)
    fig <- plot_ly(dfFig, x = xdate)
    fig <- fig %>% add_trace(y = list(0, 0), type = 'bar')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  }
  
  
}

facturation.createMap <- function (jsonFacturation, value, facturation_id) {
  
  df <- jsonlite::fromJSON(jsonFacturation)
  dfMap <- df %>% filter (pays=="FR" & as.numeric(as.character(departement)) > 0)
  dfMap <- dfMap %>% filter(id_entite %in% value)

  dfMap <- dfMap %>% group_by(departement) %>% summarise(count=sum(as.numeric(montantHT)))
  
  write_xlsx(dfMap, paste0(getwd(),'/../../www/html/map-facturation-', facturation_id,'.xlsx'))
  
  map <- NULL
  map <- plot_ly()


  
  if (nrow(dfMap) > 0) {
    map <- map %>% add_trace(
      type="choroplethmapbox",
      geojson=francejson,
      featureidkey='properties.code',
      locations=dfMap$departement,
      z=dfMap$count,
      colorscale="Viridis",
      marker=list(line=list(
        width=0),
        opacity=1
      ),
      hovertemplate = paste0('Montant HT: %{z:.2f} €<extra>',dfMap$departement,'</extra>')
    )
  }

  map <- map %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom =4,
      center=list(lon= 2.21, lat=46.22))
  )
  
  return(map)
  

}

facturation.createPieStatutMontant <- function ( jsonFacturation, facturation_id) {
  
  df <- jsonlite::fromJSON(jsonFacturation)

  dfAccompte <- df %>% filter (reglee=="non" & as.numeric(as.character(acompte)) > 0) %>% summarise(count=sum(as.numeric(restant_du)))
  dfAccompteRegle <- df %>% filter (reglee=="non" & as.numeric(as.character(acompte)) > 0) %>% summarise(count=sum(as.numeric(as.character(acompte))))
  dfNon <- df %>% filter (reglee=="non" & as.numeric(as.character(acompte)) == 0) %>% summarise(count=sum(as.numeric(montantHT)))
  dfOui <- df %>% filter (reglee=="oui") %>% summarise(count=sum(as.numeric(montantHT)))
  dfPie <- data.frame (label=c("Restant dû", "Accompte réglés", "Non réglées", "Reglées"), count=c(dfAccompte$count, dfAccompteRegle$count, dfNon$count, dfOui$count))
  
    
  write_xlsx(dfPie, paste0(getwd(),'/../../www/html/percent-statut-facturation-', facturation_id,'.xlsx'))

  figPieStatut <- dfPie %>% plot_ly(labels = ~label, values = ~count)
  figPieStatut <- figPieStatut %>% add_pie(hole = 0.5)
  figPieStatut <- figPieStatut %>% layout(
    showlegend = T,
    legend = list(orientation = 'h'),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

}

facturation.createPeriodeClientType <- function (jsonFacturation, value, periode,  facturation_id) {
  
  df <- jsonlite::fromJSON(jsonFacturation)

  
  df <- df %>% group_by(date, clientype) %>% summarise(count=length(date))
  #on reorganise le tableau
  df <- dcast(df, date ~ clientype, value.var="count", fill=0)
  

  dfGraph <- df
  
  write_xlsx(dfGraph, paste0(getwd(),'/../../www/html/qty-client-facturation-', facturation_id,'.xlsx'))
  
  dfGraph <- facturation.transformByPeriod (periode, dfGraph)

  
  dfVal <- dfGraph
  dfVal$date <- NULL
  val <- colnames(dfVal);
  
  
  value <- append(value, "date")
  dfGraph <- dfGraph %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraph)) %>% rowSums(na.rm = TRUE))

  dfFig <- dfGraph
  # if (length(value) > 2) {
  #   dfFig <- dfFig[ , (names(dfFig) %in% value)]
  # }
  
  if (nrow(dfFig) > 0) {
    fig <- plot_ly(dfFig, x = as.Date(dfFig$date,origin="1970-01-01"))
    for(p in val) {
      fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p), type = 'bar', hovertemplate = 'Quantité: %{y:.0f}<extra>%{x}</extra>')
    }
    fig <- fig %>% add_trace(y = dfGraph$Total, name = "Total", mode = 'lines+markers', hovertemplate = 'Quantité: %{y:.0f}<extra>%{x}</extra>')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig <- fig %>% layout(yaxis = list(title = 'Nombre de factures'), barmode = 'stack')
  } else {
    xdate = list(startDate_value, endDate_value)
    fig <- plot_ly(dfFig, x = xdate)
    fig <- fig %>% add_trace(y = list(0, 0), type = 'bar')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  }
  
}

facturation.createPieClientType <- function (jsonFacturation, value, periode,  facturation_id) {
  
  df <- jsonlite::fromJSON(jsonFacturation)

  df <- df %>% group_by(clientype) %>% summarise(count=sum(as.numeric(montantHT)))
  
  write_xlsx(df, paste0(getwd(),'/../../www/html/percent-client-facturation-', facturation_id,'.xlsx'))  
  figPieStatut <- df %>% plot_ly(labels = ~clientype, values = ~count)
  figPieStatut <- figPieStatut %>% add_pie(hole = 0)
  figPieStatut <- figPieStatut %>% layout(
    showlegend = T,
    legend = list(orientation = 'h'),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
}

facturation.createQtyFacturation <- function (value, periode_value, jsonFacturation, facturation_id) {
  
  df <- jsonlite::fromJSON(jsonFacturation)
  periode <- jsonlite::fromJSON(periode_value)
  df <- df %>% group_by(date, id_entite) %>% summarise(count=length(date))
  
  #on reorganise le tableau
  df <- dcast(df, date ~ id_entite, value.var="count", fill=0)

  write_xlsx(df, paste0(getwd(),'/../../www/html/qty-facturation-',facturation_id,'.xlsx'))
  
  dfGraph <- df
  dfGraph <- facturation.transformByPeriod (periode, dfGraph)
  
  
  val <- value
  value <- append(value, "date")
  
  dfFig <- dfGraph
  if (length(value) > 2) {
    dfFig <- dfFig[ , (names(dfFig) %in% value)]
  }
  dfFig <- dfFig %>% ungroup () %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))

  if (nrow(dfFig) > 0) {
    fig <- plot_ly(dfFig, x = as.Date(dfFig$date,origin="1970-01-01"))
    for(p in val) {
      fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p), mode = 'lines+markers', hovertemplate = 'Quantité: %{y:.0f}<extra>%{x}</extra>')
    }
    
    fig <- fig %>% add_trace(y = dfFig$Total, name = "Total", mode = 'lines+markers', hovertemplate = 'Quantité: %{y:.0f}<extra>%{x}</extra>')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  } else {
    xdate = list(startDate_value, endDate_value)
    fig <- plot_ly(dfFig, x = xdate)
    fig <- fig %>% add_trace(y = list(0, 0), type = 'bar')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  }
  
}



facturation.createMontantDu <- function (value, periode, jsonFacturation, facturation_id) {
  
  df <- jsonlite::fromJSON(jsonFacturation)
  dfPrint <- df %>% filter(as.numeric(as.character(restant_du))>0)
  df <- df %>% group_by(date, id_entite) %>% summarise(count=sum(as.numeric(as.character(restant_du))))
  
  write_xlsx(dfPrint, paste0(getwd(),'/../../www/html/revenus-dus-facturation-', facturation_id,'.xlsx'))
  
  #on reorganise le tableau
  df <- dcast(df, date ~ id_entite, value.var="count", fill=0)
  

  dfGraph <- df

  dfGraph <- facturation.transformByPeriod (periode, dfGraph)
  
  
  val <- value
  value <- append(value, "date")

  dfFig <- dfGraph
  if (length(value) > 2) {
    dfFig <- dfFig[ , (names(dfFig) %in% value)]
  }
  dfFig <- dfFig %>% ungroup () %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))
  if (nrow(dfFig) > 0) {
    fig <- plot_ly(dfFig, x = as.Date(dfFig$date,origin="1970-01-01"))
    for(p in val) {
      fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p), mode = 'lines+markers', hovertemplate = 'Montant HT: %{y:.0f} €<extra>%{x}</extra>')
    }
    
    fig <- fig %>% add_trace(y = dfFig$Total, name = "Total", mode = 'lines+markers', hovertemplate = 'Montant HT: %{y:.0f} €<extra>Total</extra>')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  } else {
    xdate = list(startDate_value, endDate_value)
    fig <- plot_ly(dfFig, x = xdate)
    fig <- fig %>% add_trace(y = list(0, 0), type = 'bar')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  }
  
}

facturations.getFacturation <- function (value, periode, dfFacturation, facturation_id) {

  
  df <- jsonlite::fromJSON(dfFacturation)
  df <- df %>% ungroup() %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))
  #df <- df[ , (names(df) %in% value)]
  
  if (periode == 'W') {
    
    df$date <- sapply( df$date, function(x) ISOweek(as.Date(x,origin="1970-01-01")))
    
  } else if (periode == "M") {
    
    df$date <- sapply(df$date, function(x) format(as.Date(x, origin="1970-01-01", format = "%Y-%m-%d"), "%Y-%m"))
    
  }
  
  write_xlsx(df, paste0(getwd(),'/../../www/html/facturation-', facturation_id,'.xlsx'))

  
  return(df)
}



facturation()



app$callback(
  output = list(id='count-facturation', property='children'),
  params = list(input(id='jsonFacturation-value', property='children')),
  function (jsonFacturation) {
    sprintf (facturation.getCount(jsonFacturation))
  }
  
)

app$callback(
  output = list(id='count-facturation-tva', property='children'),
  params = list(input(id='jsonFacturation-value', property='children')),
  function (jsonFacturation) {
    sprintf (facturation.getCountTVA(jsonFacturation))
  }
  
)

app$callback(
  output(id = 'container-range-slider-facturation', property='children'),
  params=list(input(id='slider-facturation', property='value')),
  function(value) {
   
    sprintf('Factures de %0.2f€ HT à %0.2f€ HT', facturation.transformValueSlider(value[1]), facturation.transformValueSlider(value[2]))
    
})

app$callback(
  output = list(id='graph-facturation', property='figure'),
  params = list(input(id='dfFacturation-value', property='children'),
                input(id='filtre-facturation', property='value'),
                input(id='periode-value', property='children')),
  function (dfFacturation, value, periode_value) {
    periode <- jsonlite::fromJSON(periode_value)
    return (facturation.createGraph(dfFacturation, value, periode))
  }
  
)

app$callback(
  output = list(id='qty-facturation', property='figure'),
  params = list( input(id='filtre-facturation', property='value'),
                 input(id='periode-value', property='children'),
                 input(id='jsonFacturation-value', property='children'),
                 input(id='facturation-id', property='children'),
                 input(id='graph-facturation', property='figure')),
  function (value, periode, jsonFacturation, facturation_id, fig) {
    return (facturation.createQtyFacturation(value, periode, jsonFacturation, facturation_id))
  }
  
)

app$callback(
  output = list(id='pie-statut-montant', property='figure'),
  params = list(input(id='jsonFacturation-value', property='children'),
                input(id='facturation-id', property='children'),
                input(id='qty-facturation', property='figure')),
  function ( jsonFacturation, facturation_id, fig) {
    return (facturation.createPieStatutMontant(jsonFacturation, facturation_id))
  }
  
)

app$callback(
  output = list(id='graph-du-montant', property='figure'),
  params = list(input(id='filtre-facturation', property='value'),
                input(id='periode-value', property='children'),
                input(id='jsonFacturation-value', property='children'),
                input(id='facturation-id', property='children'),
                input(id='pie-statut-montant', property='figure')),
  function (value, periode_value, jsonFacturation, facturation_id, fig) {
    periode <- jsonlite::fromJSON(periode_value)
    return (facturation.createMontantDu(value, periode, jsonFacturation, facturation_id))
  }
  
)

app$callback(
  output = list(id='periode-clientype', property='figure'),
  params = list(input(id='jsonFacturation-value', property='children'),
                input(id='filtre-facturation', property='value'),
                input(id='periode-value', property='children'),
                input(id='facturation-id', property='children'),
                input(id='pie-statut-montant', property='figure')),
  function (jsonFacturation, value, periode_value,  facturation_id, fig) {
    periode <- jsonlite::fromJSON(periode_value)
    return (facturation.createPeriodeClientType(jsonFacturation, value, periode, facturation_id))
  }
  
)


app$callback(
  output = list(id='pie-clientype', property='figure'),
  params = list(input(id='jsonFacturation-value', property='children'),
                input(id='filtre-facturation', property='value'),
                input(id='periode-value', property='children'),
                input(id='facturation-id', property='children'),
                input(id='periode-clientype', property='figure')),
  function (jsonFacturation,  value, periode_value, facturation_id, fig) {
    periode <- jsonlite::fromJSON(periode_value)
    return (facturation.createPieClientType(jsonFacturation, value, periode, facturation_id))
  }
  
)



app$callback(
  output = list(id='table-facturation', property='columns'),
  params = list(input(id='qty-facturation', property='figure'),
                input(id='facturation-id', property='children'),
                input(id='filtre-facturation', property='value'),
                input(id='periode-value', property='children'),
                input(id='dfFacturation-value', property='children')),
  function (fig, facturation_id, value, periode_value, dfFacturation) {
    periode <- jsonlite::fromJSON(periode_value)
    columns = lapply(colnames(facturations.getFacturation(value, periode, dfFacturation, facturation_id)),
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
  output = list(id='table-facturation', property='data'),
  params = list(input(id='qty-facturation', property = 'figure'),
                input(id='facturation-id', property='children'),
                input(id='filtre-facturation', property='value'),
                input(id='periode-value', property='children'),
                input(id='dfFacturation-value', property='children')),
  function (fig, facturation_id, value, periode_value, dfFacturation) {
    periode <- jsonlite::fromJSON(periode_value)
    return (facturations.getFacturation(value, periode, dfFacturation, facturation_id))
  }

)

app$callback(
  output = list(id='map-facturation', property='figure'),
  params = list(input(id='jsonFacturation-value', property='children'),
                input(id='filtre-facturation', property='value'),
                input(id='facturation-id', property='children'),
                input(id='pie-clientype', property='figure')),
  function (jsonFacturation, filtre, facturation_id, fig) {
    
    return (facturation.createMap(jsonFacturation, filtre, facturation_id))
  }
  
)

app$callback(
  output=list(id='facturation-id', property='children'),
  params = list(input(id = 'startDate-value', property = 'children'),
                input(id = 'endDate-value', property = 'children'),
                input(id='slider-facturation', property='value'),
                input(id='filtre-facturation', property='value')),
  function(startDate_value, endDate_value, slider, value) {
    return(sample(1:100000000, 1))
  }
)

app$callback(
  output=list(id='facturation-download-map', property='children'),
  params = list(input(id='facturation-id', property='children')),
  function(facturation_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/map-facturation-',  facturation_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='facturation-download-qty', property='children'),
  params = list(input(id='facturation-id', property='children')),
  function(facturation_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/qty-facturation-',  facturation_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='facturation-download-percent-revenus', property='children'),
  params = list(input(id='facturation-id', property='children')),
  function(facturation_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/percent-statut-facturation-',  facturation_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='facturation-download-du', property='children'),
  params = list(input(id='facturation-id', property='children')),
  function(facturation_id) {
    
    sprintf(paste0('[Télécharger la liste des contacts en .xlsx](',paste0('http://statics.ecocup.com/revenus-dus-facturation-',  facturation_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='facturation-download-qty-clienttype', property='children'),
  params = list(input(id='facturation-id', property='children')),
  function(facturation_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/qty-client-facturation-',  facturation_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='facturation-download-percent-clienttype', property='children'),
  params = list(input(id='facturation-id', property='children')),
  function(facturation_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/percent-client-facturation-',  facturation_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='facturation-download', property='children'),
  params = list(input(id='facturation-id', property='children')),
  function(facturation_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/facturation-',  facturation_id,'.xlsx)')))
  }
)






