


produits.transformByPeriod <- function (periode, dfGraph) {
  
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






produits.getCount <- function(jsonProduits) {
  df <- jsonlite::fromJSON(jsonProduits)
  
  #df <- with(df, df[as.Date(date) >= as.Date(session.startDate) & as.Date(date) <= as.Date(session.endDate) ,])
  df <- df %>% summarise(sum(as.numeric(df$quantite), na.rm = TRUE))
  #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
  
  sumTotal <-round(df[[1]],2)
  total <- format(sumTotal, digits=9, decimal.mark=",", big.mark=" ",small.mark="", , small.interval=3)
  return (paste(as.character(total)))
  
}

produits.createGraph <- function ( value, periode,  prods, dfProduits) {
  
  dfGraph <- jsonlite::fromJSON(dfProduits)
  dfGraph <- produits.transformByPeriod (periode, dfGraph)

  dfGraph$Total <- NULL
  
  val <- prods

  dfGraph <- dfGraph %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraph)) %>% rowSums(na.rm = TRUE))
  dfFig <- dfGraph


  if (nrow(dfFig) > 0) {
    fig <- plot_ly(dfFig, x = as.Date(dfFig$date,origin="1970-01-01"))
    for(p in val) {
      if(length(dfFig[[as.character(p)]]) > 0) {
        fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p), type = 'bar', hovertemplate = 'Quantité: %{y:.0f}<extra>%{x}</extra>')
      }
      
    }
    fig <- fig %>% add_trace(y = dfGraph$Total, name = "Total", mode = 'lines+markers', hovertemplate = 'Quantité: %{y:.0f}<extra>%{x}</extra>')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig <- fig %>% layout(yaxis = list(title = 'Quantité'), barmode = 'stack')
  } else {
    xdate = list('2016-10-01', Sys.Date() - 1)
    fig <- plot_ly(dfFig, x = xdate)
    fig <- fig %>% add_trace(y = list(0, 0), type = 'bar')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  }
  
  
}

produits.createMeanGraph <- function (value, prods, jsonProduits, produits_id) {
  
  df <- jsonlite::fromJSON(jsonProduits)
  df <- df %>% group_by(type_produit, quantite) %>% summarise_at(vars(prixht), list(unit = mean)) 
  df <- df %>% filter (as.numeric(quantite)>=10 & as.numeric(unit) > 0) 
  df <- df %>% filter (as.numeric(unit) < 3) 
  df <- df %>% filter (as.numeric(quantite)!=46) 

  df <- dcast(df, quantite ~ type_produit, value.var="unit", fill=0)
  
  
  
  
  df[df==0] <- NA
  df <- df[order(as.numeric(df$quantite)),]
  
  write_xlsx(df, paste0(getwd(),'/../../www/html/moyenne-produits-', produits_id,'.xlsx'))
  
  fig <- plot_ly(df, x = as.numeric(df$quantite))
  
  for(p in prods) {
    if(length(df[[as.character(p)]]) > 0) {
      fig <- fig %>% add_trace(y = df[[as.character(p)]] , name = as.character(p),mode = 'lines+markers',  connectgaps = TRUE,
                               hovertemplate = paste0('Quantité: %{x:.0f} / Prix Unitaire: %{y:.3f} €<extra>',p,'</extra>'))
    }
  }
  
  fig <- fig %>% layout(xaxis = list(range = c(0, 1000)))
  fig <- fig %>% layout(legend = list(orientation = 'h'))
  
  return(fig)
}

produits.createMap <- function (jsonProduits, value, produits_id) {
  
  df <- jsonlite::fromJSON(jsonProduits)
  dfMap <- df %>% filter (pays=="FR" & as.numeric(as.character(departement)) > 0)
  
  
  dfMap <- dfMap %>% group_by(departement) %>% summarise(count=sum(as.numeric(quantite)))
  
  
  write_xlsx(dfMap, paste0(getwd(),'/../../www/html/map-produit-', produits_id,'.xlsx'))
  
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
      hovertemplate = paste0('Quantité: %{z:.0f}<extra>',dfMap$departement,'</extra>')
    )
  }
  
  
  
  map <- map %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom = 4,
      center=list(lon= 2.21, lat=46.22))
  )

  return(map)
  
  
}

produits.createFrais <- function (periode, prods, impressions, jsonBrutProduits, produits_id) {
  
  df <- jsonlite::fromJSON(jsonBrutProduits)
  prods <- append(prods, 'REMISE')
  df <- df %>% filter(impression %in% impressions)
  df <- df %>% filter(type_produit %in% prods)
  
  dfFraisTechnique <- df %>% group_by(date) %>% summarise(totaltechnique=sum(as.numeric(totaltechnique), na.rm = TRUE))
  dfFraisImpression <- df %>% group_by(date) %>% summarise(totalimpression=sum(as.numeric(totalimpression), na.rm = TRUE))
  dfRemise <- df %>% group_by(date) %>% filter (type_produit == "REMISE") %>% summarise(remise=sum(as.numeric(prixht), na.rm = TRUE))
  dfGraph <- left_join(dfFraisTechnique, dfFraisImpression, 
                     by = c("date" = "date"))
  
  
  write_xlsx(dfGraph, paste0(getwd(),'/../../www/html/frais-produits-', produits_id,'.xlsx'))
  #dfGraph <- left_join(dfTmp, dfRemise, by = c("date" = "date"))
  
  dfGraph <- produits.transformByPeriod (periode, dfGraph)
  
  dfGraph <- dfGraph %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraph)) %>% rowSums(na.rm = TRUE))
  dfFig <- dfGraph
  

  if (nrow(dfFig) > 0) {
    fig <- plot_ly(dfFig, x = as.Date(dfFig$date,origin="1970-01-01"))
    
    fig <- fig %>% add_trace(y = dfFig$totaltechnique, name = "Frais technique", type = 'bar', hovertemplate = 'Montant HT: %{y:.2f } €<extra>%{x}</extra>')
    fig <- fig %>% add_trace(y = dfFig$totalimpression, name = "Frais d'impression", type = 'bar', hovertemplate = 'Montant HT: %{y:.2f } €<extra>%{x}</extra>')
    #fig <- fig %>% add_trace(y = dfFig$remise, name = "Remise", type = 'bar')
    
    fig <- fig %>% add_trace(y = dfGraph$Total, name = "Total", mode = 'lines+markers', hovertemplate = 'Montant HT: %{y:.2f } €<extra>%{x}</extra>')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig <- fig %>% layout(yaxis = list(title = 'Euros'), barmode = 'relative')
  } else {
    xdate = list(session.startDate, session.endDate)
    fig <- plot_ly(dfFig, x = xdate)
    fig <- fig %>% add_trace(y = list(0, 0), type = 'bar')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  }
  
  return (fig)
 
}

produits.createRemise <- function (periode, prods, impressions, value, jsonBrutProduits, produits_id) {
  
  df <- jsonlite::fromJSON(jsonBrutProduits)
  prods <- append(prods, 'REMISE')


  dfGraph <- df %>% group_by(date, id_entite) %>% filter (type_produit == "REMISE") %>% summarise(count=sum(as.numeric(prixht), na.rm = TRUE))
  dfGraph <- dcast(dfGraph, date ~ id_entite, value.var="count", fill=0)
  
  write_xlsx(dfGraph, paste0(getwd(),'/../../www/html/remise-produits-', produits_id,'.xlsx'))
  #dfGraph <- left_join(dfTmp, dfRemise, by = c("date" = "date"))
  
  dfGraph <- produits.transformByPeriod (periode, dfGraph)
  
  dfGraph <- dfGraph %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraph)) %>% rowSums(na.rm = TRUE))
  dfFig <- dfGraph

  
  if (nrow(dfFig) > 0) {
    fig <- plot_ly(dfFig, x = as.Date(dfFig$date,origin="1970-01-01"))
    
    for(p in value) {
      fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p), type = 'bar', hovertemplate = 'Montant HT: %{y:.2f } €<extra>%{x}</extra>')
    }
    #fig <- fig %>% add_trace(y = dfFig$remise, name = "Remise", type = 'bar')
    
    fig <- fig %>% add_trace(y = dfGraph$Total, name = "Total", mode = 'lines+markers', hovertemplate = 'Montant HT: %{y:.2f } €<extra>%{x}</extra>')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig <- fig %>% layout(yaxis = list(title = 'Euros'), barmode = 'relative')
  } else {
    xdate = list(session.startDate, session.endDate)
    fig <- plot_ly(dfFig, x = xdate)
    fig <- fig %>% add_trace(y = list(0, 0), type = 'bar')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  }
  
  return (fig)
  
}

produits.getProduits <- function (dfProduits, produits_id, periode) {
  
  df <- jsonlite::fromJSON(dfProduits)
  df <- df %>% ungroup() %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))
  #df <- df[ , (names(df) %in% value)]
  
  if (periode == 'W') {
    
    df$date <- sapply( df$date, function(x) ISOweek(as.Date(x,origin="1970-01-01")))
    
  } else if (periode == "M") {
    
    df$date <- sapply(df$date, function(x) format(as.Date(x, origin="1970-01-01", format = "%Y-%m-%d"), "%Y-%m"))
    
  }
  
  write_xlsx(df, paste0(getwd(),'/../../www/html/produits-', produits_id,'.xlsx'))
  return(df)
}

produits <- function () {
  
}

produits ()

app$callback(
  output = list(id='count-produits', property='children'),
  params = list(input(id='jsonProduits-value', property='children')),
  function (jsonProduits) {
    sprintf (produits.getCount(jsonProduits))
  }
  
)

app$callback(
  output = list(id='graph-produits', property='figure'),
  params = list(
                input(id='filtre-produits', property='value'),
                input(id='periode-value', property='children'),
                input(id='filtre-typeproduits', property='value'),
                input(id='dfProduits-value', property='children')),
  function (value, periode_value,  prods, dfProduits) {
    periode <- jsonlite::fromJSON(periode_value)
    return (produits.createGraph(value, periode, prods,  dfProduits))
  }
  
)

app$callback(
  output = list(id='graph-moyenne-produits', property='figure'),
  params = list(input(id='filtre-produits', property='value'),
                input(id='filtre-typeproduits', property='value'),
                input(id='jsonProduits-value', property='children'),
                input(id='produits-id', property='children'),
                input(id='graph-produits', property='figure')),
  function (value, prods, jsonProduits, produits_id, fig) {
    return (produits.createMeanGraph(value, prods, jsonProduits, produits_id))
  }
  
)

app$callback(
  output = list(id='graph-frais-produits', property='figure'),
  params = list(input(id='jsonBrutProduits-value', property='children'),
                input(id='filtre-typeproduits', property='value'),
                input(id='periode-value', property='children'),
                input(id='filtre-impression', property='value'),
                input(id='produits-id', property='children'),
                input(id='graph-moyenne-produits', property='figure')),
  function (jsonBrutProduits, prods, periode_value, impressions, produits_id, fig) {
    periode <- jsonlite::fromJSON(periode_value)
    return (produits.createFrais(periode, prods, impressions, jsonBrutProduits, produits_id))
  }
  
)

app$callback(
  output = list(id='graph-remise-produits', property='figure'),
  params = list(input(id='jsonBrutProduits-value', property='children'),
                input(id='filtre-typeproduits', property='value'),
                input(id='periode-value', property='children'),
                input(id='filtre-impression', property='value'),
                input(id='filtre-produits', property='value'),
                input(id='produits-id', property='children'),
                input(id='graph-frais-produits', property='figure')),
  function (jsonBrutProduits, prods, periode_value, impressions, entites, produits_id, fig) {
    periode <- jsonlite::fromJSON(periode_value)
    return (produits.createRemise(periode, prods, impressions, entites, jsonBrutProduits, produits_id))
  }
  
)

app$callback(
  output = list(id='map-produits', property='figure'),
  params = list(input(id='jsonProduits-value', property='children'),
                input(id='filtre-produits', property='value'),
                input(id='produits-id', property='children'),
                input(id='graph-remise-produits', property='figure')),
  function (jsonProduits, filtre, produits_id, fig) {
    
    return (produits.createMap(jsonProduits, filtre, produits_id))
  }
  
)

app$callback(
  output = list(id='table-produits', property='columns'),
  params = list(input(id='dfProduits-value', property='children'),
                input(id='filtre-typeproduits', property='value'),
                input(id='periode', property='value'),
                input(id='filtre-impression', property='value'),
                input(id='filtre-produits', property='value'),
                input(id='produits-id', property='children')),
  function (dfProduits, prods, periode, impressions, entites, produits_id) {
    columns = lapply(colnames(produits.getProduits(dfProduits, produits_id, periode)),
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
  output = list(id='table-produits', property='data'),
  params = list(input(id='dfProduits-value', property='children'),
                input(id='filtre-typeproduits', property='value'),
                input(id='periode', property='value'),
                input(id='filtre-impression', property='value'),
                input(id='filtre-produits', property='value'),
                input(id='produits-id', property='children')),
  function (dfProduits, prods, periode, impressions, entites, produits_id) {
    return (produits.getProduits(dfProduits, produits_id, periode))
  }
  
)

app$callback(
  output=list(id='produits-id', property='children'),
  params = list(input(id = 'startDate-value', property = 'children'),
                input(id = 'endDate-value', property = 'children'),
                input(id='filtre-typeproduits', property='value'),
                input(id='filtre-produits', property='value'),
                input(id='filtre-impression', property='value')),
  function(startDate_value, endDate_value, produits, value, impressions) {
    return(sample(1:100000000, 1))
  }
)

app$callback(
  output=list(id='produits-download-map', property='children'),
  params = list(input(id='produits-id', property='children')),
  function(produits_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/map-produit-',  produits_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='moyenne-produits-download', property='children'),
  params = list(input(id='produits-id', property='children')),
  function(produits_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/moyenne-produits-',  produits_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='frais-produits-download', property='children'),
  params = list(input(id='produits-id', property='children')),
  function(produits_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/frais-produits-',  produits_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='remise-produits-download', property='children'),
  params = list(input(id='produits-id', property='children')),
  function(produits_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/remise-produits-',  produits_id,'.xlsx)')))
  }
)

app$callback(
  output=list(id='produits-download', property='children'),
  params = list(input(id='produits-id', property='children')),
  function(produits_id) {
    
    sprintf(paste0('[Télécharger en .xlsx](',paste0('http://statics.ecocup.com/produits-',  produits_id,'.xlsx)')))
  }
)


