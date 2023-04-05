
dfFacturation <- data.frame()
jsonFacturation <- data.frame()


facturation <- function () {
  
}
facturation.getPeriode <- function (start_date, end_date) {
  
  d <- interval(start_date, end_date)
  v <- as.period(d)
  
  
  if (v$month < 1 & v$year == 0) {
    options=list(
      list(label = "Par jour", value = "D"),
      list(label = "Par semaine", value = "W")
    )
  } else {
    options=list(
      list(label = "Par jour", value = "D"),
      list(label = "Par semaine", value = "W"),
      list(label = "Par mois", value = "M")
    )
  }
  return (options)
}

facturation.getPeriodeByValue <- function (start_date, end_date) {
  
  d <- interval(start_date, end_date)
  v <- as.period(d)
  
  
  
  if (v$month < 12 & v$year == 0) {
    value = 'W'
  } else {
    value = "M"
  }
  return (value)
}



facturation.getDatas <- function (montantSlider, value) {
  
  # df <- jsonFacturation
  # url <- 'http://personnel.ecocup.fr/bo2api/controllers/receipts/index.php?&start='
  # 
  # 
  # df <- jsonlite::fromJSON(txt = paste0(url, startDate, "&end=",endDate))
  # 
  # df <- facturation.clean(df)

  dfdb <- dbGetQuery(DB, paste0("SELECT * FROM factures WHERE date >='", startDate, "' AND date <='", endDate,"';") )
  #dbClearResult(dfdb)
  df <- as.data.frame(dfdb)

  
  df <- df %>% filter (as.numeric(montantHT)>=as.numeric(facturation.transformValueSlider(montantSlider[1])) & as.numeric(montantHT)<=as.numeric(facturation.transformValueSlider(montantSlider[2])))
  df <- df %>% filter(id_entite %in% value)
  jsonFacturation <<- df

  #on groupe et on compte
  df <- df %>% group_by(date, id_entite) %>% summarise(count=sum(as.numeric(montantHT)))
  
  #df$date <- sapply(df$date, function(x) format(as.Date(x, "%d/%m/%Y"), "%Y-%m-%d"))

  #on reorganise le tableau
  df <- dcast(df, date ~ id_entite, value.var="count", fill=0)

  #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
  df <- df %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))
  dfFacturation <<- df


  return (dfFacturation)
  
}

facturation.transformValueSlider <- function (value) {
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

facturation.getCount <- function() {
  df <- jsonFacturation

  df <- with(df, df[as.Date(date) >= as.Date(startDate) & as.Date(date) <= as.Date(endDate) ,])
  df <- df %>% summarise(sum(as.numeric(df$montantHT)))
  #on ajoute une colonne Total qui fait la somme des 3 autres colonnes

  sumTotal <-round(df[[1]],2)
  return (paste(as.character(sumTotal), "€"))
}



facturation.getCountTVA <- function() {
  
  df <- jsonFacturation

  df <- with(df, df[as.Date(date) >= as.Date(startDate) & as.Date(date) <= as.Date(endDate) ,])
  dfDu <- df %>% summarise(count=sum(as.numeric(as.character(tva))))
  
  sumTotal <- round(dfDu[[1]], 2)
  return (paste(as.character(sumTotal), "€"))
  
}

facturation.createGraph <- function (start_date, end_date,value, periode, montant) {
  
  startDate <<- start_date
  endDate <<- end_date
  dfGraph <- facturation.getDatas(montant, value)
  dftest2 <- dfGraph
  #View(dftest2)
 # dfGraph <- with(dfGraph, dfGraph[as.Date(date) >= as.Date(startDate) & as.Date(date) <= as.Date(endDate) ,])
  dfGraph$Total <- NULL
  
  if (periode == 'W') {
    
    dfGraph <- dfGraph %>% group_by(week = ISOweek(date)) %>% summarise_at(.vars = 2:ncol(dfGraph), sum)
    dftest <- dfGraph
    #View(dftest)
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
      fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p), type = 'bar')
    }
    fig <- fig %>% add_trace(y = dfGraph$Total, name = "Total", mode = 'lines+markers')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
    fig <- fig %>% layout(yaxis = list(title = 'Euros'), barmode = 'stack')
  } else {
    xdate = list(startDate, endDate)
    fig <- plot_ly(dfFig, x = xdate)
    fig <- fig %>% add_trace(y = list(0, 0), type = 'bar')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  }
  
  
}

facturation.createMap <- function (fig, value) {

  
  dfMap <- jsonFacturation %>% filter (pays=="FR" & as.numeric(as.character(departement)) > 0)
  dfMap <- dfMap %>% filter(id_entite %in% value)
  dfMap <- with(dfMap, dfMap[as.Date(date) >= as.Date(startDate) & as.Date(date) <= as.Date(endDate) ,])
  
  dfMap <- dfMap %>% group_by(departement) %>% summarise(count=sum(as.numeric(montantHT)))


  map <<- NULL
  map <<- plot_ly()


  
  if (nrow(dfMap) > 0) {
    map <<- map %>% add_trace(
      type="choroplethmapbox",
      geojson=francejson,
      featureidkey='properties.code',
      locations=dfMap$departement,
      z=dfMap$count,
      colorscale="Viridis",
      marker=list(line=list(
        width=0),
        opacity=1
      )
    )
  }




  map <<- map %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom =4,
      center=list(lon= 2.21, lat=46.22))
  )
  
  # return(map)
  

}

facturation.createPieStatutMontant <- function (start_date, end_date,value, periode, montant, fig) {
  
  df <- jsonFacturation

  df <- with(df, df[as.Date(date) >= as.Date(startDate) & as.Date(date) <= as.Date(endDate) ,])
  
  dfAccompte <- df %>% filter (reglee=="non" & as.numeric(as.character(acompte)) > 0) %>% summarise(count=sum(as.numeric(as.numeric(montantHT) - as.numeric(as.character(acompte)))))
  dfAccompteRegle <- df %>% filter (reglee=="non" & as.numeric(as.character(acompte)) > 0) %>% summarise(count=sum(as.numeric(as.character(acompte))))
  dfNon <- df %>% filter (reglee=="non" & as.numeric(as.character(acompte)) == 0) %>% summarise(count=sum(as.numeric(montantHT)))
  dfOui <- df %>% filter (reglee=="oui") %>% summarise(count=sum(as.numeric(montantHT)))
  dfPie <- data.frame (label=c("Restant dû", "Accompte réglés", "Non réglées", "Reglées"), count=c(dfAccompte$count, dfAccompteRegle$count, dfNon$count, dfOui$count))


  figPieStatut <- dfPie %>% plot_ly(labels = ~label, values = ~count)
  figPieStatut <- figPieStatut %>% add_pie(hole = 0.5)
  figPieStatut <- figPieStatut %>% layout(
    showlegend = T,
    legend = list(orientation = 'h'),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

}

facturation.createQtyFacturation <- function (start_date, end_date,value, periode, montant, fig) {
  
  df <- jsonFacturation
  df <- df %>% group_by(date, id_entite) %>% summarise(count=length(date))
  

  
  #on reorganise le tableau
  df <- dcast(df, date ~ id_entite, value.var="count", fill=0)
  
  #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
  
  
  df <- with(df, df[as.Date(date) >= as.Date(startDate) & as.Date(date) <= as.Date(endDate) ,])
  dfGraph <- df
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
      fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p), mode = 'lines+markers')
    }
    
    fig <- fig %>% add_trace(y = dfFig$Total, name = "Total", mode = 'lines+markers')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  } else {
    xdate = list(startDate, endDate)
    fig <- plot_ly(dfFig, x = xdate)
    fig <- fig %>% add_trace(y = list(0, 0), type = 'bar')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  }
  
}



facturation.createMontantDu <- function (start_date, end_date,value, periode, montant, fig) {
  
  df <- jsonFacturation
  df <- df %>% group_by(date, id_entite) %>% summarise(count=sum(as.numeric(as.character(restant_du))))
  

  
  #on reorganise le tableau
  df <- dcast(df, date ~ id_entite, value.var="count", fill=0)
  
  #on ajoute une colonne Total qui fait la somme des 3 autres colonnes

  
  df <- with(df, df[as.Date(date) >= as.Date(startDate) & as.Date(date) <= as.Date(endDate) ,])
  dfGraph <- df

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
      fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p), mode = 'lines+markers')
    }
    
    fig <- fig %>% add_trace(y = dfGraph$Total, name = "Total", mode = 'lines+markers')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  } else {
    xdate = list(startDate, endDate)
    fig <- plot_ly(dfFig, x = xdate)
    fig <- fig %>% add_trace(y = list(0, 0), type = 'bar')
    fig <- fig %>% layout(legend = list(orientation = 'h'))
  }
  
}

facturations.getFacturation <- function (value, valuePeriode) {
  
}



facturation()





app$callback(
  output(id = 'container-range-slider-facturation', property='children'),
  params=list(input(id='slider-facturation', property='value')),
  function(value) {
   
    sprintf('Factures de %0.2f€ HT à %0.2f€ HT', facturation.transformValueSlider(value[1]), facturation.transformValueSlider(value[2]))
    
})

app$callback(
  output = list(id='graph-facturation', property='figure'),
  params = list(input(id='my-date-picker-facturation', property = 'start_date'),
                input(id = 'my-date-picker-facturation', property = 'end_date'),
                input(id='filtre-facturation', property='value'),
                input(id='facturation-periode', property='value'),
                input(id='slider-facturation', property='value')),
  function (start_date, end_date, value, periode, montant) {
    return (facturation.createGraph(start_date, end_date,value, periode, montant))
  }
  
)

app$callback(
  output = list(id='facturation-periode', property='value'),
  params = list(input(id='my-date-picker-facturation', property = 'start_date'),
                input(id = 'my-date-picker-facturation', property = 'end_date')),
  function (start_date, end_date) {
    return (facturation.getPeriodeByValue(start_date, end_date))
  }
  
)

app$callback(
  output = list(id='facturation-periode', property='options'),
  params = list(input(id='my-date-picker-facturation', property = 'start_date'),
                input(id = 'my-date-picker-facturation', property = 'end_date')),
  function (start_date, end_date) {
    return (facturation.getPeriode(start_date, end_date))
  }
  
)

app$callback(
  output = list(id='map-facturation', property='figure'),
  params = list(input(id='graph-facturation', property='figure'),
                input(id='slider-facturation', property='value'),
                input(id='my-date-picker-facturation', property = 'start_date'),
                input(id = 'my-date-picker-facturation', property = 'end_date'),
                input(id='filtre-facturation', property='value'),
                input(id='facturation-periode', property='value')),
  function (fig, value, start_date, end_date, filtre, periode) {
    
    return (facturation.createMap(fig, filtre))
  }
  
)

app$callback(
  output = list(id='pie-statut-montant', property='figure'),
  params = list(input(id='my-date-picker-facturation', property = 'start_date'),
                input(id = 'my-date-picker-facturation', property = 'end_date'),
                input(id='filtre-facturation', property='value'),
                input(id='facturation-periode', property='value'),
                input(id='slider-facturation', property='value'),
                input(id='graph-facturation', property='figure')),
  function (start_date, end_date, value, periode, montant, fig) {
    return (facturation.createPieStatutMontant(start_date, end_date,value, periode, montant, fig))
  }
  
)

app$callback(
  output = list(id='graph-du-montant', property='figure'),
  params = list(input(id='my-date-picker-facturation', property = 'start_date'),
                input(id = 'my-date-picker-facturation', property = 'end_date'),
                input(id='filtre-facturation', property='value'),
                input(id='facturation-periode', property='value'),
                input(id='slider-facturation', property='value'),
                input(id='graph-facturation', property='figure')),
  function (start_date, end_date, value, periode, montant, fig) {
    return (facturation.createMontantDu(start_date, end_date,value, periode, montant, fig))
  }
  
)





app$callback(
  output = list(id='qty-facturation', property='figure'),
  params = list(input(id='my-date-picker-facturation', property = 'start_date'),
                input(id = 'my-date-picker-facturation', property = 'end_date'),
                input(id='filtre-facturation', property='value'),
                input(id='facturation-periode', property='value'),
                input(id='slider-facturation', property='value'),
                input(id='graph-facturation', property='figure')),
  function (start_date, end_date, value, periode, montant, fig) {
    return (facturation.createQtyFacturation(start_date, end_date,value, periode, montant, fig))
  }
  
)

app$callback(
  output = list(id='count-facturation', property='children'),
  params = list(input(id='graph-facturation', property='figure')),
  function (fig) {
    sprintf (facturation.getCount())
  }
  
)



app$callback(
  output = list(id='count-facturation-tva', property='children'),
  params = list(input(id='graph-facturation', property='figure')),
  function (fig) {
    sprintf (facturation.getCountTVA())
  }
  
)

# app$callback(
#   output = list(id='table-facturation', property='columns'),
#   params = list(input(id='qty-facturation', property='figure'),
#                 input(id='filtre-facturation', property='value'),
#                 input(id='facturation-periode', property='value')),
#   function (fig, value, valuePeriode) {
#     columns = lapply(colnames(facturations.getFacturation(value, valuePeriode)),
#                      function(colName){
#                        list(
#                          id = colName,
#                          name = colName
#                        )
#                      })
#     return (columns)
#   }
# 
# )
# 
# app$callback(
#   output = list(id='table-facturation', property='data'),
#   params = list(input(id='qty-facturation', property = 'figure'),
#                 input(id='filtre-facturation', property='value'),
#                 input(id='facturation-periode', property='value')),
#   function (fig, value, valuePeriode) {
#     return (facturations.getFacturation(value, valuePeriode))
#   }
# 
# )




