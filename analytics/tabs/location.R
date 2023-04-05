


dfLocation <- data.frame()
jsonLocation <- data.frame()


location <- function () {
  
}

location.getDatas <- function () {
  
  df <- jsonLocation
  
  if (is.null(df$cdate)) {
    df <- jsonlite::fromJSON(txt = paste0("http://personnel.ecocup.fr/bo2api/controllers/quotes/index.php?rent=true&start=", startDate, "&end=",endDate))
    
  }
  
  
  minDate <- min(as.Date(df$cdate))
  maxDate <- max(as.Date(df$cdate))
  
  if (startDate < minDate | endDate > maxDate) {
    df <- jsonlite::fromJSON(txt = paste0("http://personnel.ecocup.fr/bo2api/controllers/quotes/index.php?rent=true&start=", startDate, "&end=",endDate))
    
  }
  
  
  # On nomme les entités
  df$id_entite <- sapply(df$id_entite, function(x) gsub("1", "EcocupFR", x))
  df$id_entite <- sapply(df$id_entite, function(x) gsub("2", "EcocupBE", x))
  df$id_entite <- sapply(df$id_entite, function(x) gsub("3", "EcocupUK", x))
  df$id_entite <- sapply(df$id_entite, function(x) gsub("4", "Greencup", x))
  
  #on nettoie les dates
  df$cdate <- sapply(df$cdate, function(x) gsub('([0-9]+) .*', '\\1', x))
  df$cdate <- sapply(df$cdate, function(x) gsub(' ', '', x))
  
  jsonLocation <<- df
  
  #on groupe et on compte
  df <- df %>% group_by(cdate, id_entite) %>% summarise(count=length(cdate))
  
  #on reorganise le tableau
  df <- dcast(df, cdate ~ id_entite, value.var="count", fill=0)
  
  #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
  df <- df %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))
  dfLocation <<- df
  
  
  return (dfLocation)
  
}

location.getCount <- function () {
  
  df <- dfLocation
  #on recalcule les totaux
  df$Total <- NULL
  #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
  df <- df %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))
  sumTotal <-sum(df$Total)
  return (as.character(sumTotal))
  
}

location.getPeriode <- function (start_date, end_date) {
  
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

location.getPeriodeByValue <- function (start_date, end_date) {
  
  d <- interval(start_date, end_date)
  v <- as.period(d)
  
  
  
  if (v$month < 12 & v$year == 0) {
    value = 'W'
  } else {
    value = "M"
  }
  return (value)
}

location.createGraph <- function (start_date, end_date, value, periode) {
  
  startDate <<- start_date
  endDate <<- end_date
  
  dfGraph <- location.getDatas()
  
  dfGraph <- with(dfGraph, dfGraph[as.Date(cdate) >= as.Date(startDate) & as.Date(cdate) <= as.Date(endDate) ,])
  
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
  
  value <- append(value, "Total")
  val <- value
  value <- append(value, "cdate")
  
  dfFig <- dfGraph
  if (length(value) > 2) {
    dfFig <- dfFig[ , (names(dfFig) %in% value)]
  }
  dfLocation <<- dfFig
  fig <- plot_ly(dfFig, x = as.Date(dfFig$cdate,origin="1970-01-01"))
  for(p in val) {
    fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p),mode = 'lines+markers')
  }
  fig <- fig %>% layout(legend = list(orientation = 'h'))
  
  # return (fig)
  
}

location.createGraphMontant <- function(start_date, end_date, value, periode) {
  
  dfGraph <- jsonLocation
  
  
  dfGraph$id <- NULL
  dfGraph$numlocation <- NULL
  
  
  dfGraph <- dfGraph[(dfGraph$id_entite %in% value), ]
  dfGraph$id_entite <- NULL
  
  dfGraph <- dfGraph %>% group_by(cdate, etat) %>% summarise(montant = sum(as.numeric(montant)))
  
  dfGraph <- dcast(dfGraph, cdate ~ etat, value.var="montant", fill=0)
  
  
  
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
  dfGraph <- dfGraph %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraph)) %>% rowSums(na.rm = TRUE))
  
  write_xlsx(dfGraph, paste0(getwd(),'/assets/datas/locations/location-periode-statut.xlsx'))
  
  figMontant <- plot_ly(dfGraph, x = as.Date(dfGraph$cdate,origin="1970-01-01"))
  
  val <- c("Validé", "Refusé", "En attente", "Dépassé")
  
  for(p in val) {
    figMontant <- figMontant %>% add_trace(y = dfGraph[[as.character(p)]], name = as.character(p), type = 'bar')
  }
  
  #dfGraph <- dfGraph %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraph)) %>% rowSums(na.rm = TRUE))
  
  figMontant <- figMontant %>% add_trace(y = dfGraph$Total, name = "Total",mode = 'lines+markers')
  
  figMontant <- figMontant %>% layout(legend = list(orientation = 'h'))
  figMontant <- figMontant %>% layout(yaxis = list(title = 'Euros'), barmode = 'stack')
  
  # return (figMontant)
  
}

location.createGraphMontantEntite <- function(start_date, end_date, value, periode) {
  
  dfGraph <- jsonLocation
  
  
  dfGraph$id <- NULL
  dfGraph$numlocation <- NULL
  
  
  
  dfGraph <- dfGraph[(dfGraph$id_entite %in% value), ]
  dfGraph$etat <- NULL
  
  dfGraph <- dfGraph %>% group_by(cdate, id_entite) %>% summarise(montant = sum(as.numeric(montant)))
  
  dfGraph <- dcast(dfGraph, cdate ~ id_entite, value.var="montant", fill=0)
  
  
  
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
  dfGraph <- dfGraph %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraph)) %>% rowSums(na.rm = TRUE))
  
  write_xlsx(dfGraph, paste0(getwd(),'/assets/datas/locations/location-periode-entite.xlsx'))
  
  figMontant <- plot_ly(dfGraph, x = as.Date(dfGraph$cdate,origin="1970-01-01"))
  
  
  
  for(p in value) {
    figMontant <- figMontant %>% add_trace(y = dfGraph[[as.character(p)]], name = as.character(p), type = 'bar')
  }
  
  #dfGraph <- dfGraph %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraph)) %>% rowSums(na.rm = TRUE))
  
  figMontant <- figMontant %>% add_trace(y = dfGraph$Total, name = "Total",mode = 'lines+markers')
  
  figMontant <- figMontant %>% layout(legend = list(orientation = 'h'))
  figMontant <- figMontant %>% layout(yaxis = list(title = 'Euros'), barmode = 'stack')
  
  # return (figMontant)
  
}



location.getLocation <- function (value, periode) {
  
  value <- append(value, "Total")
  value <- append(value, "cdate")
  
  df <- dfLocation
  
  df <- df[ , (names(df) %in% value)]
  
  if (periode == 'W') {
    
    df$cdate <- sapply( df$cdate, function(x) ISOweek(as.Date(x,origin="1970-01-01")))
    
  } else if (periode == "M") {
    
    df$cdate <- sapply(df$cdate, function(x) as.Date(x, origin="1970-01-01", format = "%Y-%m-%d"))
    
  }
  write_xlsx(df, paste0(getwd(),'/assets/datas/locations/location.xlsx'))
  #View(df)
  return(df)
  
}

location.createPieStatut <- function (value) {
  
  dfStatut <- jsonLocation
  
  
  dfStatut <- dfStatut[(dfStatut$id_entite %in% value), ]
  
  dfStatut$id <- NULL
  dfStatut$numlocation <- NULL
  dfStatut$cdate <- NULL
  dfStatut$id_entite <- NULL
  
  dfStatut <- dfStatut %>% group_by(etat) %>% summarise(count=length(etat))
  
  write_xlsx(dfStatut, paste0(getwd(),'/assets/datas/locations/location-statut.xlsx')) 
  
  figPieStatut <- dfStatut %>% plot_ly(labels = ~etat, values = ~count)
  figPieStatut <- figPieStatut %>% add_pie(hole = 0.5)
  figPieStatut <- figPieStatut %>% layout(
    showlegend = T,
    legend = list(orientation = 'h'),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(figPieStatut)
  
}

location.createPie <- function() {
  
  dfPie <- dfLocation
  
  dfPie <- as.data.frame(t(dfPie))
  
  dfPie <- dfPie[-1, ]
  
  dfPie[] <- lapply(dfPie, function(x) { if(is.factor(x)) as.numeric(as.character(x)) else x})
  
  dfPie <- rowSums(dfPie[, 1:length(dfPie), drop = TRUE])
  
  dfPie <- tibble::rownames_to_column(as.data.frame(dfPie), "entite")
  
  dfPie <-  as.data.frame(dfPie)[-nrow(as.data.frame(dfPie)),]
  
  
  
  names(dfPie)[2]<-paste("num")
  
  write_xlsx(dfPie, paste0(getwd(),'/assets/datas/locations/location-pie.xlsx'))  
  
  figPie <- dfPie %>% plot_ly(labels = ~entite, values = ~num)
  figPie <- figPie %>% add_pie(hole = 0.5)
  figPie <- figPie %>% layout(
    showlegend = T,
    legend = list(orientation = 'h'),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  # return(figPie)
  
  
}



location.createTotaux <- function() {
  
  dfTotaux <- dfLocation
  dfTotaux <- as.data.frame(t(dfTotaux))
  
  dfTotaux <- dfTotaux[-1, ]
  dfTotaux[] <- lapply(dfTotaux, function(x) { if(is.factor(x)) as.numeric(as.character(x)) else x})
  
  dfTotaux <- rowSums(dfTotaux[, 1:length(dfTotaux), drop = TRUE])
  dfTotaux <- tibble::rownames_to_column(as.data.frame(dfTotaux), "entite")
  dfTotaux <-  as.data.frame(dfTotaux)[-nrow(as.data.frame(dfTotaux)),]
  names(dfTotaux)[2]<-paste("num")
  
  write_xlsx(dfTotaux, paste0(getwd(),'/assets/datas/locations/location-entite.xlsx')) 
  
  figTotaux <- plot_ly(x = dfTotaux$num, y = dfTotaux$entite, type = 'bar', orientation = 'h')
  
  # return(figTotaux)
  
}



app$callback(
  output = list(id='location-periode', property='options'),
  params = list(input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date')),
  function (start_date, end_date) {
    return (location.getPeriode(start_date, end_date))
  }
  
)

app$callback(
  output = list(id='location-periode', property='value'),
  params = list(input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date')),
  function (start_date, end_date) {
    return (location.getPeriodeByValue(start_date, end_date))
  }
  
)


app$callback(
  output = list(id='graph-location', property='figure'),
  params = list(input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date'),
                input(id='filtre-location', property='value'),
                input(id='location-periode', property='value')),
  function (start_date, end_date, value, periode) {
    return (location.createGraph(start_date, end_date,value, periode))
  }
  
)

app$callback(
  output = list(id='count-location', property='children'),
  params = list(input(id='graph-location', property='figure')),
  function (fig) {
    sprintf (location.getCount())
  }
  
)





app$callback(
  output = list(id='graph-location-montant', property='figure'),
  params = list(input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date'),
                input(id='filtre-location', property='value'),
                input(id='location-periode', property='value'),
                input(id='graph-location', property='figure')),
  function (start_date, end_date, value, periode, fig) {
    return (location.createGraphMontant(start_date, end_date, value, periode))
  }
  
)

app$callback(
  output = list(id='graph-location-montant-entite', property='figure'),
  params = list(input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date'),
                input(id='filtre-location', property='value'),
                input(id='location-periode', property='value'),
                input(id='graph-location-montant', property='figure')),
  function (start_date, end_date, value, periode, fig) {
    return (location.createGraphMontantEntite(start_date, end_date, value, periode))
  }
  
)





app$callback(
  output = list(id='pie-location', property='figure'),
  params = list(input(id='graph-location', property='figure')),
  function (fig) {
    return (location.createPie())
  }
  
)

app$callback(
  output = list(id='pie-statut-location', property='figure'),
  params = list(input(id='pie-location', property='figure'),
                input(id='filtre-location', property='value')),
  function (fig, value) {
    return (location.createPieStatut(value))
  }
  
)

app$callback(
  output = list(id='totaux-location', property='figure'),
  params = list(input(id='pie-statut-location', property='figure')),
  function (fig) {
    return (location.createTotaux())
  }
  
)
app$callback(
  output = list(id='table-location', property='columns'),
  params = list(input(id='graph-location-montant', property='figure'),
                input(id='filtre-location', property='value'),
                input(id='location-periode', property='value')),
  function (fig, value, valuePeriode) {
    columns = lapply(colnames(location.getLocation(value, valuePeriode)), 
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
  params = list(input(id='graph-location-montant', property = 'figure'),
                input(id='filtre-location', property='value'),
                input(id='location-periode', property='value')),
  function (fig, value, valuePeriode) {
    return (location.getLocation(value, valuePeriode))
  }
  
)

app$callback(
  output=list(id='location-download', property='children'),
  params = list(input(id='graph-location', property='figure'),
                input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date'),
                input(id='filtre-location', property='value')),
  function(fig, start_date, end_date, value) {
    value <- append(value, "cdate")
    sprintf(paste0('[Télécharger en .xlsx](','/assets/datas/locations/location.xlsx)'))
  }
)


location ()







