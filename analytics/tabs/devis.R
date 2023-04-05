

startDate <- Sys.Date()- 90
endDate <- Sys.Date() - 1
dfDevis <- data.frame()
jsonDevis <- data.frame()


devis <- function () {
  
}

devis.getDatas <- function () {
  
  df <- jsonDevis
  
  if (is.null(df$cdate)) {
    df <- jsonlite::fromJSON(txt = paste0("http://personnel.ecocup.fr/bo2api/controllers/quotes/index.php?start=", startDate, "&end=",endDate))
    
  }
  

  minDate <- min(as.Date(df$cdate))
  maxDate <- max(as.Date(df$cdate))

  if (startDate < minDate | endDate > maxDate) {
    df <- jsonlite::fromJSON(txt = paste0("http://personnel.ecocup.fr/bo2api/controllers/quotes/index.php?start=", startDate, "&end=",endDate))
    
  }

  
  # On nomme les entités
  df$id_entite <- sapply(df$id_entite, function(x) gsub("1", "EcocupFR", x))
  df$id_entite <- sapply(df$id_entite, function(x) gsub("2", "EcocupBE", x))
  df$id_entite <- sapply(df$id_entite, function(x) gsub("3", "EcocupUK", x))
  df$id_entite <- sapply(df$id_entite, function(x) gsub("4", "Greencup", x))
  
  #on nettoie les dates
  df$cdate <- sapply(df$cdate, function(x) gsub('([0-9]+) .*', '\\1', x))
  df$cdate <- sapply(df$cdate, function(x) gsub(' ', '', x))
  
  jsonDevis <<- df
  
  #on groupe et on compte
  df <- df %>% group_by(cdate, id_entite) %>% summarise(count=length(cdate))
  
  #on reorganise le tableau
  df <- dcast(df, cdate ~ id_entite, value.var="count", fill=0)
  
  #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
  df <- df %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))
  dfDevis <<- df
  
  
  return (dfDevis)
  
}

devis.getCount <- function () {
  
  df <- dfDevis
  #on recalcule les totaux
  df$Total <- NULL
  #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
  df <- df %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))
  sumTotal <-sum(df$Total)
  return (as.character(sumTotal))
  
}

devis.getPeriode <- function (start_date, end_date) {
  
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

devis.getPeriodeByValue <- function (start_date, end_date) {
  
  d <- interval(start_date, end_date)
  v <- as.period(d)
  
  
  
  if (v$month < 12 & v$year == 0) {
    value = 'W'
  } else {
    value = "M"
  }
  return (value)
}

devis.createGraph <- function (start_date, end_date, value, periode) {
  
  startDate <<- start_date
  endDate <<- end_date
  
  dfGraph <- devis.getDatas()

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
  dfDevis <<- dfFig
  fig <- plot_ly(dfFig, x = as.Date(dfFig$cdate,origin="1970-01-01"))
  for(p in val) {
    fig <- fig %>% add_trace(y = dfFig[[as.character(p)]], name = as.character(p),mode = 'lines+markers')
  }
  fig <- fig %>% layout(legend = list(orientation = 'h'))
  
  # return (fig)
  
}

devis.createGraphMontant <- function(start_date, end_date, value, periode) {
  
  dfGraph <- jsonDevis
  

  dfGraph$id <- NULL
  dfGraph$numdevis <- NULL

  
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
  
  write_xlsx(dfGraph, paste0(getwd(),'/assets/datas/devis/devis-periode-statut.xlsx'))
  
  figMontant <- plot_ly(dfGraph, x = as.Date(dfGraph$cdate,origin="1970-01-01"))

  val <- c("Validé", "Refusé", "En attente")

  for(p in val) {
    figMontant <- figMontant %>% add_trace(y = dfGraph[[as.character(p)]], name = as.character(p), type = 'bar')
  }

  #dfGraph <- dfGraph %>% ungroup() %>% mutate(Total = select(., 2:ncol(dfGraph)) %>% rowSums(na.rm = TRUE))
  
  figMontant <- figMontant %>% add_trace(y = dfGraph$Total, name = "Total",mode = 'lines+markers')

  figMontant <- figMontant %>% layout(legend = list(orientation = 'h'))
  figMontant <- figMontant %>% layout(yaxis = list(title = 'Euros'), barmode = 'stack')

  # return (figMontant)
  
}

devis.createGraphMontantEntite <- function(start_date, end_date, value, periode) {
  
  dfGraph <- jsonDevis
  

  dfGraph$id <- NULL
  dfGraph$numdevis <- NULL
  

  
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
  
  write_xlsx(dfGraph, paste0(getwd(),'/assets/datas/devis/devis-periode-entite.xlsx'))
  
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



devis.getDevis <- function (value, periode) {
  
  value <- append(value, "Total")
  value <- append(value, "cdate")
  
  df <- dfDevis

  df <- df[ , (names(df) %in% value)]
  
  if (periode == 'W') {
    
    df$cdate <- sapply( df$cdate, function(x) ISOweek(as.Date(x,origin="1970-01-01")))
    
  } else if (periode == "M") {
    
    df$cdate <- sapply(df$cdate, function(x) as.Date(x, origin="1970-01-01", format = "%Y-%m-%d"))
    
  }
  write_xlsx(df, paste0(getwd(),'/assets/datas/devis/devis.xlsx'))

  return(df)
  
}

devis.createPieStatut <- function (value) {
  
  dfStatut <- jsonDevis

  
  dfStatut <- dfStatut[(dfStatut$id_entite %in% value), ]
  
  dfStatut$id <- NULL
  dfStatut$numdevis <- NULL
  dfStatut$cdate <- NULL
  dfStatut$id_entite <- NULL
  
  dfStatut <- dfStatut %>% group_by(etat) %>% summarise(count=length(etat))
  
  write_xlsx(dfStatut, paste0(getwd(),'/assets/datas/devis/devis-statut.xlsx')) 
  
  figPieStatut <- dfStatut %>% plot_ly(labels = ~etat, values = ~count)
  figPieStatut <- figPieStatut %>% add_pie(hole = 0.5)
  figPieStatut <- figPieStatut %>% layout(
    showlegend = T,
    legend = list(orientation = 'h'),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(figPieStatut)
  
}

devis.createPie <- function() {
  
  dfPie <- dfDevis
 
  dfPie <- as.data.frame(t(dfPie))
  
  dfPie <- dfPie[-1, ]
  
  dfPie[] <- lapply(dfPie, function(x) { if(is.factor(x)) as.numeric(as.character(x)) else x})
  
  dfPie <- rowSums(dfPie[, 1:length(dfPie), drop = TRUE])
  
  dfPie <- tibble::rownames_to_column(as.data.frame(dfPie), "entite")
 
  dfPie <-  as.data.frame(dfPie)[-nrow(as.data.frame(dfPie)),]
  
  
  
  names(dfPie)[2]<-paste("num")

  write_xlsx(dfPie, paste0(getwd(),'/assets/datas/devis/devis-pie.xlsx'))  
  
  figPie <- dfPie %>% plot_ly(labels = ~entite, values = ~num)
  figPie <- figPie %>% add_pie(hole = 0.5)
  figPie <- figPie %>% layout(
                          showlegend = T,
                          legend = list(orientation = 'h'),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  # return(figPie)
    

}



devis.createTotaux <- function() {
  
  dfTotaux <- dfDevis
  dfTotaux <- as.data.frame(t(dfTotaux))
  
  dfTotaux <- dfTotaux[-1, ]
  dfTotaux[] <- lapply(dfTotaux, function(x) { if(is.factor(x)) as.numeric(as.character(x)) else x})
  
  dfTotaux <- rowSums(dfTotaux[, 1:length(dfTotaux), drop = TRUE])
  dfTotaux <- tibble::rownames_to_column(as.data.frame(dfTotaux), "entite")
  dfTotaux <-  as.data.frame(dfTotaux)[-nrow(as.data.frame(dfTotaux)),]
  names(dfTotaux)[2]<-paste("num")
  
  write_xlsx(dfTotaux, paste0(getwd(),'/assets/datas/devis/devis-entite.xlsx')) 
  
  figTotaux <- plot_ly(x = dfTotaux$num, y = dfTotaux$entite, type = 'bar', orientation = 'h')

  # return(figTotaux)
  
}



app$callback(
  output = list(id='devis-periode', property='options'),
  params = list(input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date')),
  function (start_date, end_date) {
    return (devis.getPeriode(start_date, end_date))
  }
  
)

app$callback(
  output = list(id='devis-periode', property='value'),
  params = list(input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date')),
  function (start_date, end_date) {
    return (devis.getPeriodeByValue(start_date, end_date))
  }
  
)


app$callback(
  output = list(id='graph-devis', property='figure'),
  params = list(input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date'),
                input(id='filtre-devis', property='value'),
                input(id='devis-periode', property='value')),
  function (start_date, end_date, value, periode) {
    return (devis.createGraph(start_date, end_date,value, periode))
  }
  
)

app$callback(
  output = list(id='count-devis', property='children'),
  params = list(input(id='graph-devis', property='figure')),
  function (fig) {
    sprintf (devis.getCount())
  }
  
)





app$callback(
  output = list(id='graph-devis-montant', property='figure'),
  params = list(input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date'),
                input(id='filtre-devis', property='value'),
                input(id='devis-periode', property='value'),
                input(id='graph-devis', property='figure')),
  function (start_date, end_date, value, periode, fig) {
    return (devis.createGraphMontant(start_date, end_date, value, periode))
  }
  
)

app$callback(
  output = list(id='graph-devis-montant-entite', property='figure'),
  params = list(input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date'),
                input(id='filtre-devis', property='value'),
                input(id='devis-periode', property='value'),
                input(id='graph-devis-montant', property='figure')),
  function (start_date, end_date, value, periode, fig) {
    return (devis.createGraphMontantEntite(start_date, end_date, value, periode))
  }
  
)





app$callback(
  output = list(id='pie-devis', property='figure'),
  params = list(input(id='graph-devis', property='figure')),
  function (fig) {
    return (devis.createPie())
  }
  
)

app$callback(
  output = list(id='pie-statut-devis', property='figure'),
  params = list(input(id='pie-devis', property='figure'),
                input(id='filtre-devis', property='value')),
  function (fig, value) {
    return (devis.createPieStatut(value))
  }
  
)

app$callback(
  output = list(id='totaux-devis', property='figure'),
  params = list(input(id='pie-statut-devis', property='figure')),
  function (fig) {
    return (devis.createTotaux())
  }
  
)
app$callback(
  output = list(id='table-devis', property='columns'),
  params = list(input(id='graph-devis-montant', property='figure'),
                input(id='filtre-devis', property='value'),
                input(id='devis-periode', property='value')),
  function (fig, value, valuePeriode) {
    columns = lapply(colnames(devis.getDevis(value, valuePeriode)), 
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
  params = list(input(id='graph-devis-montant', property = 'figure'),
                input(id='filtre-devis', property='value'),
                input(id='devis-periode', property='value')),
  function (fig, value, valuePeriode) {
     return (devis.getDevis(value, valuePeriode))
  }

)

app$callback(
  output=list(id='devis-download', property='children'),
  params = list(input(id='graph-devis', property='figure'),
                input(id='my-date-picker', property = 'start_date'),
                input(id = 'my-date-picker', property = 'end_date'),
                input(id='filtre-devis', property='value')),
  function(fig, start_date, end_date, value) {
     value <- append(value, "cdate")
     sprintf(paste0('[Télécharger en .xlsx](','/assets/datas/devis/devis.xlsx)'))
  }
)


devis ()







