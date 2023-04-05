library('dash')
library('dashCoreComponents')
library('dashHtmlComponents')
library('dashTable')
library('jsonlite')
library('ggplot2')
library('plotly')
library('dplyr')
library('reshape2')
library('writexl')
library('xlsx')
library('htmlwidgets')
library('tidyverse')
library('lubridate')
library('ISOweek')
library('maps')
library ('geojsonR')
library('rjson')
library('RJSONIO')
library('RMySQL')


#phpmyadmin
#url https://bo.ecocup.fr/phpmyadmin/index.php
#login ecocup01
#password a5TdsKAG

external_scripts <- list(

  list(src = 'https://cdn.auth0.com/js/auth0-spa-js/1.7/auth0-spa-js.production.js')

)

killDbConnections <- function () {
  
  all_cons <- dbListConnections(MySQL())
  

  
  for(con in all_cons)
    +  dbDisconnect(con)
  
  
}

killDbConnections()


app <- Dash$new(external_scripts = external_scripts)



source('tabs/devis.R')
source('tabs/location.R')
source('tabs/facturation.R')
source('tabs/produits.R')

app$layout(htmlDiv(list(
  htmlDiv(id='startDate-value',
          children = jsonlite::toJSON(Sys.Date() - 100), style=list(display = 'none')
  ),
  htmlDiv(id='endDate-value',
          children = jsonlite::toJSON(Sys.Date() - 1), style=list(display = 'none')
  ),
  htmlDiv(id='periode-value',
          children = jsonlite::toJSON('W'), style=list(display = 'none')
  ),

  htmlDiv(
    className = 'container-header',
    list (
      htmlButton("Se déconnecter", id="logout",style = list(
        'margin-right' = '35px'
      ))
    )
  ),
  htmlDiv(
    className = "mini_container_date",
    list(
      htmlH3 ("Filtrer par période :",  style = list(
        textAlign = 'left'
      )),
      dccDatePickerRange(
        className = "range-periode",
        id='my-date-picker',
        display_format = "DD/MM/Y",
        min_date_allowed=as.Date("2016-10-01"),
        max_date_allowed=Sys.Date() - 1,
        start_date  =  Sys.Date() - 100,
        end_date  =  Sys.Date() - 1
      ),
      dccDropdown(
        className = "dropdown-periode",
        id = "periode",
        
        clearable = FALSE
      )
    ), style= list("text-align" = "left", "padding" = "15px", 'width' = "30%", 'height' = '110px')
  ),
  htmlH1 ("Analytics CUPS",  style = list(
    textAlign = 'center'
  )),
  htmlH5 ("Outil BIG DATA d'aide à la décision stratégique",  style = list(
    textAlign = 'center', 'font-weight' = 'normal'
  )),
  htmlDiv(
    className = 'container-content',
    list (
      dccTabs(id="tabs", value='tab-1', children=list(
        dccTab(label='Devis Vente', value='tab-1'),
        dccTab(label='Devis Location', value='tab-2'),
        dccTab(label='Facturation', value='tab-3'),
        dccTab(label='Produits', value='tab-4')
      )),
      htmlDiv(id='tabs-content')
    )
  )

)))

app$callback(
  output(id = 'startDate-value', property = 'children'),
  list(input(id='my-date-picker', property = 'start_date')),
  function(start_date) {
    
    jsonlite::toJSON(start_date)
  }
)



app$callback(
  output(id = 'endDate-value', property = 'children'),
  list(input(id='my-date-picker', property = 'end_date')),
  function(end_date) {
    
    jsonlite::toJSON(end_date)
  }
)

app$callback(
  output(id = 'periode-value', property = 'children'),
  list(input(id='periode', property = 'value')),
  function(value) {
    
    jsonlite::toJSON(value)
  }
)

app$callback(
  output(id = 'dfDevis-value', property = 'children'),
  list(input(id = 'startDate-value', property = 'children'),
       input(id = 'endDate-value', property = 'children'),
       input(id= 'filtre-devis', property='value')),
  
  function(startDate_value, endDate_value, value) {
    
    startDateValue <- jsonlite::fromJSON(startDate_value)
    endDateValue <- jsonlite::fromJSON(endDate_value)
    
    DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)
    dfdb <- dbGetQuery(DB, paste0("SELECT * FROM devis WHERE cdate >='", startDateValue, "' AND cdate <='", endDateValue,"';") )
    dbDisconnect(DB)
    
    df <- as.data.frame(dfdb)
    df$row_names <- NULL
    
    #on groupe et on compte
    df <- df %>% group_by(cdate, id_entite) %>% summarise(count=length(cdate))
    
    #on reorganise le tableau
    df <- dcast(df, cdate ~ id_entite, value.var="count", fill=0)
    
    #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
    df <- df %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))
    
    jsonlite::toJSON(df)
  }
)

app$callback(
  output(id = 'jsonDevis-value', property = 'children'),
  list(input(id = 'startDate-value', property = 'children'),
       input(id = 'endDate-value', property = 'children'),
       input(id= 'filtre-devis', property='value')),
  
  function(startDate_value, endDate_value, value) {
    
    startDateValue <- jsonlite::fromJSON(startDate_value)
    endDateValue <- jsonlite::fromJSON(endDate_value)
    
    DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)
    dfdb <- dbGetQuery(DB, paste0("SELECT * FROM devis WHERE cdate >='", startDateValue, "' AND cdate <='", endDateValue,"';") )
    dbDisconnect(DB)
    
    df <- as.data.frame(dfdb)
    df$row_names <- NULL
    
    jsonlite::toJSON(df)
  }
)





app$callback(
  output(id = 'dfLocation-value', property = 'children'),
  list(input(id = 'startDate-value', property = 'children'),
       input(id = 'endDate-value', property = 'children'),
       input(id= 'filtre-location', property='value')),
  
  function(startDate_value, endDate_value, value) {
    
    startDateValue <- jsonlite::fromJSON(startDate_value)
    endDateValue <- jsonlite::fromJSON(endDate_value)
    
    DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)
    dfdb <- dbGetQuery(DB, paste0("SELECT * FROM location WHERE cdate >='", startDateValue, "' AND cdate <='", endDateValue,"';") )
    dbDisconnect(DB)
    
    df <- as.data.frame(dfdb)
    df$row_names <- NULL
    
    #on groupe et on compte
    df <- df %>% group_by(cdate, id_entite) %>% summarise(count=length(cdate))
    
    #on reorganise le tableau
    df <- dcast(df, cdate ~ id_entite, value.var="count", fill=0)
    
    #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
    df <- df %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))
    
    jsonlite::toJSON(df)
  }
)

app$callback(
  output(id = 'jsonLocation-value', property = 'children'),
  list(input(id = 'startDate-value', property = 'children'),
       input(id = 'endDate-value', property = 'children'),
       input(id= 'filtre-location', property='value')),
  
  function(startDate_value, endDate_value, value) {
    
    startDateValue <- jsonlite::fromJSON(startDate_value)
    endDateValue <- jsonlite::fromJSON(endDate_value)
    
    DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)
    dfdb <- dbGetQuery(DB, paste0("SELECT * FROM location WHERE cdate >='", startDateValue, "' AND cdate <='", endDateValue,"';") )
    dbDisconnect(DB)
    
    df <- as.data.frame(dfdb)
    df$row_names <- NULL
    
    jsonlite::toJSON(df)
  }
)


app$callback(
  output(id = 'dfFacturation-value', property = 'children'),
  list(input(id='slider-facturation', property='value'),
       input(id = 'startDate-value', property = 'children'),
       input(id = 'endDate-value', property = 'children'),
       input(id= 'filtre-facturation', property='value')),
  
  function(montantSlider, startDate_value, endDate_value, value) {
    
    startDateValue <- jsonlite::fromJSON(startDate_value)
    endDateValue <- jsonlite::fromJSON(endDate_value)
    
    DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)
    dfdb <- dbGetQuery(DB, paste0("SELECT * FROM factures WHERE date >='", startDateValue, "' AND date <='", endDateValue,"';") )
    dbDisconnect(DB)
    
    df <- as.data.frame(dfdb)
    df$row_names <- NULL
    
    df <- df %>% filter (as.numeric(montantHT)>=as.numeric(facturation.transformValueSlider(montantSlider[1])) & as.numeric(montantHT)<=as.numeric(facturation.transformValueSlider(montantSlider[2])))
    df <- df %>% filter(id_entite %in% value)

    
    #on groupe et on compte
    df <- df %>% group_by(date, id_entite) %>% summarise(count=sum(as.numeric(montantHT)))
    
    #on reorganise le tableau
    df <- dcast(df, date ~ id_entite, value.var="count", fill=0)
    
    #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
    df <- df %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))
    
    jsonlite::toJSON(df)
  }
)

app$callback(
  output(id = 'jsonFacturation-value', property = 'children'),
  list(input(id='slider-facturation', property='value'),
       input(id = 'startDate-value', property = 'children'),
       input(id = 'endDate-value', property = 'children'),
       input(id= 'filtre-facturation', property='value')),
  
  function(montantSlider, startDate_value, endDate_value, value) {
    
    startDateValue <- jsonlite::fromJSON(startDate_value)
    endDateValue <- jsonlite::fromJSON(endDate_value)
    
    DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)
    dfdb <- dbGetQuery(DB, paste0("SELECT * FROM factures WHERE date >='", startDateValue, "' AND date <='", endDateValue,"';") )
    dbDisconnect(DB)
    
    df <- as.data.frame(dfdb)
    df$row_names <- NULL
    
    df <- df %>% filter (as.numeric(montantHT)>=as.numeric(facturation.transformValueSlider(montantSlider[1])) & as.numeric(montantHT)<=as.numeric(facturation.transformValueSlider(montantSlider[2])))
    df <- df %>% filter(id_entite %in% value)
    
    jsonlite::toJSON(df)
  }
)

app$callback(
  output(id = 'dfProduits-value', property = 'children'),
  list(input(id='filtre-typeproduits', property='value'),
       input(id = 'startDate-value', property = 'children'),
       input(id = 'endDate-value', property = 'children'),
       input(id= 'filtre-impression', property='value'),
       input(id= 'filtre-produits', property='value')),
  
  function(prods, startDate_value, endDate_value, impressions, value) {
    
    startDateValue <- jsonlite::fromJSON(startDate_value)
    endDateValue <- jsonlite::fromJSON(endDate_value)
    
    DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)
    dfdb <- dbGetQuery(DB, paste0("SELECT * FROM produits WHERE date >='", startDateValue, "' AND date <='", endDateValue,"';") )
    dbDisconnect(DB)
    
    df <- as.data.frame(dfdb)
    df$row_names <- NULL

    df <- df %>% filter(id_entite %in% value)
    df <- df %>% filter(impression %in% impressions)
    df <- df %>% filter(type_produit %in% prods)

    
    #on groupe et on compte
    df <- df %>% group_by(date, type_produit) %>% summarise(count=sum(as.numeric(quantite)))
    
    #on reorganise le tableau
    df <- dcast(df, date ~ type_produit, value.var="count", fill=0)
    
    #on ajoute une colonne Total qui fait la somme des 3 autres colonnes
    df <- df %>% mutate(Total = select(., 2:ncol(df)) %>% rowSums(na.rm = TRUE))

    
    jsonlite::toJSON(df)
    
  }
)

app$callback(
  output(id = 'jsonProduits-value', property = 'children'),
  list(input(id='filtre-typeproduits', property='value'),
       input(id = 'startDate-value', property = 'children'),
       input(id = 'endDate-value', property = 'children'),
       input(id= 'filtre-impression', property='value'),
       input(id= 'filtre-produits', property='value')),
  
  function(prods, startDate_value, endDate_value, impressions, value) {
    
    startDateValue <- jsonlite::fromJSON(startDate_value)
    endDateValue <- jsonlite::fromJSON(endDate_value)
    
    DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)
    dfdb <- dbGetQuery(DB, paste0("SELECT * FROM produits WHERE date >='", startDateValue, "' AND date <='", endDateValue,"';") )
    dbDisconnect(DB)
    
    df <- as.data.frame(dfdb)
    df$row_names <- NULL

    df <- df %>% filter(id_entite %in% value)
    df <- df %>% filter(impression %in% impressions)
    df <- df %>% filter(type_produit %in% prods)

    jsonlite::toJSON(df)
    
  }
)

app$callback(
  output(id = 'jsonBrutProduits-value', property = 'children'),
  list(input(id='filtre-typeproduits', property='value'),
       input(id = 'startDate-value', property = 'children'),
       input(id = 'endDate-value', property = 'children'),
       input(id= 'filtre-impression', property='value'),
       input(id= 'filtre-produits', property='value')),
  
  function(prods, startDate_value, endDate_value, impressions, value) {
    
    startDateValue <- jsonlite::fromJSON(startDate_value)
    endDateValue <- jsonlite::fromJSON(endDate_value)
    
    DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)
    dfdb <- dbGetQuery(DB, paste0("SELECT * FROM produits WHERE date >='", startDateValue, "' AND date <='", endDateValue,"';") )
    dbDisconnect(DB)
    
    df <- as.data.frame(dfdb)
    df$row_names <- NULL

    
    jsonlite::toJSON(df)
    
  }
)



app$callback(
  output = list(id='periode', property='options'),
  params = list(input(id = 'startDate-value', property = 'children'),
                input(id = 'endDate-value', property = 'children')),
  function (startDate_value, endDate_value) {
    start_date <- jsonlite::fromJSON(startDate_value)
    end_date <- jsonlite::fromJSON(endDate_value)
    return (getPeriode(start_date, end_date))
  }
  
)

app$callback(
  output = list(id='periode', property='value'),
  params = list(input(id = 'startDate-value', property = 'children'),
                input(id = 'endDate-value', property = 'children')),
  function (startDate_value, endDate_value) {
    start_date <- jsonlite::fromJSON(startDate_value)
    end_date <- jsonlite::fromJSON(endDate_value)
    return (getPeriodeByValue(start_date, end_date))
  }
  
)

getPeriode <- function (start_date, end_date) {
  
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

getPeriodeByValue <- function (start_date, end_date) {
  
  d <- interval(start_date, end_date)
  v <- as.period(d)
  
  if (v$month < 12 & v$year == 0) {
    value = 'W'
  } else {
    value = "M"
  }
  return (value)
}



app$callback(output('tabs-content', 'children'),
             params = list(input('tabs', 'value')),
             function(tab){
               if(tab == 'tab-1'){
                 return(htmlDiv(

                       list(
                         dccLoading(htmlDiv(id='dfDevis-value', style=list(display = 'none')), style =list("position" = "absolute", "top" = "100px")
                         ),
                         htmlDiv(id='jsonDevis-value', style=list(display = 'none')
                         ),
                         htmlDiv(id='devis-id', children=sample(1:100000000, 1), style=list(display = 'none')
                         ),
                         htmlDiv(


                           className = 'container-display',
                           list(
                             htmlDiv(
                               className = "mini_container",
                               list(
                                 htmlH1 ("0",  style = list(
                                   textAlign = 'left'
                                 ), id="count-devis"),
                                 dccMarkdown("Devis sur la période sélectionnée")
                               ), style= list("text-align" = "left", "padding" = "15px", 'width' = "47%")
                             ),
                             htmlDiv(
                               className = "mini_container",
                               list(
                                 
                                 htmlH3 ("Filtrer par entités :",  style = list(
                                   textAlign = 'left'
                                 )),
                                 dccDropdown(
                                   id = "filtre-devis",
                                   options = list(
                                     list(label="EcocupFR", value="EcocupFR"),
                                     list(label="EcocupBE", value="EcocupBE"),
                                     list(label="Greencup", value="Greencup")
                                   ),
                                   value = list("EcocupFR", "EcocupBE", "Greencup"),
                                   multi = TRUE,
                                   clearable = FALSE
                                 )
                               ), style= list("text-align" = "right", "padding" = "20px", 'width' = "47%")
                               
                             )
                           ), style = list('columnCount' = 2)


                         ),
                         htmlDiv(
                           className = "pretty_container",
                           dccLoading(id="loading-1",children=list(dccGraph(id = 'principal-graph-devis')), type="default")
                         ),
                         htmlDiv(
                           className = 'container-display',
                           list(
                             htmlDiv(
                               className = "mini_container",
                               list(
                                 htmlH2 ("Pourcentages de devis par entité",  style = list(
                                   textAlign = 'center'
                                 )),
                                 htmlDiv(
                                   list(
                                     dccMarkdown(
                                       paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/devis-pie-.xlsx)'),
                                       id = "devis-download-pie"
                                     )
                                   ), style=list( 'text-align' = "right")
                                 ),
                                 dccLoading(id="loading-2", children=list(dccGraph(id = 'pie-devis')), type="default")
                               ), style= list( "padding" = "15px", 'width' = "30%")
                             ),
                             htmlDiv(
                               className = "mini_container",
                               list(
                                 htmlH2 ("Nombre de devis par entité",  style = list(
                                   textAlign = 'center'
                                 )),
                                 htmlDiv(
                                   list(
                                     dccMarkdown(
                                       paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/devis-entite-.xlsx)'),
                                       id = "devis-download-entite"
                                     )
                                   ), style=list( 'text-align' = "right")
                                 ),
                                 dccLoading(id="loading-3", children=list(dccGraph(id = 'totaux-devis')), type="default")
                               ), style= list("padding" = "20px", 'width' = "30%")
                               
                             ),
                             htmlDiv(
                               className = "mini_container",
                               list(
                                 htmlH2 ("Pourcentages de devis par statut",  style = list(
                                   textAlign = 'center'
                                 )),
                                 htmlDiv(
                                   list(
                                     dccMarkdown(
                                       paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/devis-statut-.xlsx)'),
                                       id = "devis-download-statut"
                                     )
                                   ), style=list( 'text-align' = "right")
                                 ),
                                 dccLoading(id="loading-4", children=list(dccGraph(id = 'pie-statut-devis')), type="default")
                               ), style= list( "padding" = "15px", 'width' = "30%")
                             )
                           ), style = list('columnCount' = 3)
                           
                           
                         ),
                         htmlDiv(
                           className = 'container-display',
                           list(
                             
                             htmlDiv(
                               className = "pretty_container_big",
                               
                                 list(
                                   htmlH2 ("Montant des devis par période et par statut",  style = list(
                                     textAlign = 'center'
                                   )),
                                   htmlDiv(
                                     list(
                                       dccMarkdown(
                                         paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/devis-periode-statut-.xlsx)'),
                                         id = "devis-download-periode-statut"
                                       )
                                     ), style=list( 'text-align' = "right")
                                   ),
                                   dccLoading(id="loading-graph-devis-montant",children=list(dccGraph(id = 'graph-devis-montant')), type="default")
                                 ), style= list( "padding" = "15px", 'width' = "47%")
                               
                               
                             ),
                             htmlDiv(
                               className = "pretty_container_big",
                               
                               list(
                                 htmlH2 ("Montant des devis par période et par entite",  style = list(
                                   textAlign = 'center'
                                 )),
                                 htmlDiv(
                                   list(
                                     dccMarkdown(
                                       paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/devis-periode-entite-.xlsx)'),
                                       id = "devis-download-periode-entite"
                                     )
                                   ), style=list( 'text-align' = "right")
                                 ),
                                 dccLoading(id="loading-graph-devis-montant-entite",children=list(dccGraph(id = 'graph-devis-montant-entite')), type="default")
                               ), style= list( "padding" = "15px", 'width' = "47%")
                               
                               
                             )
                           ), style = list('columnCount' = 2)
                         ),
                         htmlDiv(
                           
                           className = "mini_container",
                           list(
                             htmlH2 ("Données sur la période sélectionnée",  style = list(
                               textAlign = 'center'
                             )),
                             htmlDiv(
                               list(
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/devis-.xlsx)'),
                                   id = "devis-download"
                                   )
                               ), style=list( 'text-align' = "right")
                             ),
                             dashDataTable(
                               id = "table-devis",
                               style_table= list(
                                 maxHeight = '300',
                                 border = 'thin lightgrey solid'
                               ),
                               
                               fixed_rows= list(headers = TRUE, data = 0),
                               style_cell = list(
                                 width = '150px',
                                 padding = '10px'
                               ),
                               style_header = list(
                                 backgroundColor = '#00a699',
                                 fontWeight = 'bold',
                                 color = "white"
                               ),
                               style_cell_conditional = lapply(c('cdate'),
                                                               function(name) {
                                                                 list(
                                                                   'if' = list('column_id' = name),
                                                                   textAlign = 'left'
                                                                 )
                                                               })
                             )
                           ), style= list("padding" = "20px")
                           
                           
                         )
                       )
                       

                     ))}
               else if(tab == 'tab-2'){
                 return(htmlDiv(
                   
                   list(
                     dccLoading(htmlDiv(id='dfLocation-value', style=list(display = 'none')), style =list("position" = "absolute", "top" = "100px")
                     ),
                     htmlDiv(id='jsonLocation-value', style=list(display = 'none')
                     ),
                     htmlDiv(id='location-id', children=sample(1:100000000, 1), style=list(display = 'none')
                     ),
                     htmlDiv(
                       
                       
                       className = 'container-display',
                       list(
                         htmlDiv(
                           className = "mini_container",
                           list(
                             htmlH1 ("0",  style = list(
                               textAlign = 'left'
                             ), id="count-location"),
                             dccMarkdown("Devis Locations sur la période sélectionnée")
                           ), style= list("text-align" = "left", "padding" = "15px", 'width' = "47%")
                         ),
                         htmlDiv(
                           className = "mini_container",
                           list(
                             
                             htmlH3 ("Filtrer par entités :",  style = list(
                               textAlign = 'left'
                             )),
                             dccDropdown(
                               id = "filtre-location",
                               options = list(
                                 list(label="EcocupFR", value="EcocupFR"),
                                 list(label="EcocupBE", value="EcocupBE")
                               ),
                               value = list("EcocupFR", "EcocupBE"),
                               multi = TRUE,
                               clearable = FALSE
                             )
                           ), style= list("text-align" = "right", "padding" = "20px", 'width' = "47%")
                           
                         )
                       ), style = list('columnCount' = 2)
                       
                       
                     ),
                     htmlDiv(
                       className = "pretty_container",
                       dccLoading(id="loading-1",children=list(dccGraph(id = 'principal-graph-location')), type="default")
                     ),
                     htmlDiv(
                       className = 'container-display',
                       list(
                         htmlDiv(
                           className = "mini_container",
                           list(
                             htmlH2 ("Pourcentages des devis locations par entité",  style = list(
                               textAlign = 'center'
                             )),
                             htmlDiv(
                               list(
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/location-pie-.xlsx)'),
                                   id = "location-download-pie",
                                   style = list("text-align" = "right")
                                 )
                               ), style=list( 'text-align' = "right")
                             ),
                             dccLoading(id="loading-2", children=list(dccGraph(id = 'pie-location')), type="default")
                           ), style= list( "padding" = "15px", 'width' = "30%")
                         ),
                         htmlDiv(
                           className = "mini_container",
                           list(
                             htmlH2 ("Nombre de location par entité",  style = list(
                               textAlign = 'center'
                             )),
                             htmlDiv(
                               list(
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/location-entite-.xlsx)'),
                                   id = "location-download-entite",
                                   style = list("text-align" = "right")
                                 )
                               ), style=list( 'text-align' = "right")
                             ),
                             dccLoading(id="loading-3", children=list(dccGraph(id = 'totaux-location')), type="default")
                           ), style= list("padding" = "20px", 'width' = "30%")
                           
                         ),
                         htmlDiv(
                           className = "mini_container",
                           list(
                             htmlH2 ("Pourcentages de devis locations par statut",  style = list(
                               textAlign = 'center'
                             )),
                             htmlDiv(
                               list(
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/location-statut-.xlsx)'),
                                   id = "location-download-statut",
                                   style = list("text-align" = "right")
                                 )
                               ), style=list( 'text-align' = "right")
                             ),
                             dccLoading(id="loading-4", children=list(dccGraph(id = 'pie-statut-location')), type="default")
                           ), style= list( "padding" = "15px", 'width' = "30%")
                         )
                       ), style = list('columnCount' = 3)
                       
                       
                     ),
                     htmlDiv(
                       className = 'container-display',
                       list(
                         
                         htmlDiv(
                           className = "pretty_container_big",
                           
                           list(
                             htmlH2 ("Montant des devis locations par période et par statut",  style = list(
                               textAlign = 'center'
                             )),
                             htmlDiv(
                               list(
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/location-periode-statut-.xlsx)'),
                                   id = "location-download-periode-statut",
                                   style = list("text-align" = "right")
                                 )
                               ), style=list( 'text-align' = "right")
                             ),
                             dccLoading(id="loading-graph-location-montant",children=list(dccGraph(id = 'graph-location-montant')), type="default")
                           ), style= list( "padding" = "15px", 'width' = "47%")
                           
                           
                         ),
                         htmlDiv(
                           className = "pretty_container_big",
                           
                           list(
                             htmlH2 ("Montant des devis locations par période et par entite",  style = list(
                               textAlign = 'center'
                             )),
                             htmlDiv(
                               list(
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/location-periode-entite-.xlsx)'),
                                   id = "location-download-periode-entite",
                                   style = list("text-align" = "right")
                                 )
                               ), style=list( 'text-align' = "right")
                             ),
                             dccLoading(id="loading-graph-location-montant",children=list(dccGraph(id = 'graph-location-montant-entite')), type="default")
                           ), style= list( "padding" = "15px", 'width' = "47%")
                           
                           
                         )
                       ), style = list('columnCount' = 2)
                     ),
                     htmlDiv(
                       
                       className = "mini_container",
                       list(
                         htmlH2 ("Données sur la période sélectionnée",  style = list(
                           textAlign = 'center'
                         )),
                         htmlDiv(
                           list(
                             dccMarkdown(
                               paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/location-.xlsx)'),
                               id = "location-download",
                               style = list("text-align" = "right")
                             )
                           ), style=list( 'text-align' = "right")
                         ),
                         dashDataTable(
                           id = "table-location",
                           style_table= list(
                             maxHeight = '300',
                             border = 'thin lightgrey solid'
                           ),
                           
                           fixed_rows= list(headers = TRUE, data = 0),
                           style_cell = list(
                             width = '150px',
                             padding = '10px'
                           ),
                           style_header = list(
                             backgroundColor = '#00a699',
                             fontWeight = 'bold',
                             color = "white"
                           ),
                           style_cell_conditional = lapply(c('cdate'),
                                                           function(name) {
                                                             list(
                                                               'if' = list('column_id' = name),
                                                               textAlign = 'left'
                                                             )
                                                           })
                         )
                       ), style= list("padding" = "20px")
                       
                       
                     )
                   )
                   
                   
                 ))}
               else if(tab == 'tab-3'){
                 
                 return(htmlDiv(
                   
                   list(
                     dccLoading(htmlDiv(id='dfFacturation-value', style=list(display = 'none')), style =list("position" = "absolute", "top" = "100px")
                     ),
                     htmlDiv(id='jsonFacturation-value', style=list(display = 'none')
                     ),
                     htmlDiv(id='facturation-id', children=sample(1:100000000, 1), style=list(display = 'none')
                     ),
                     htmlDiv(
                       
                       
                       className = 'container-display',
                       list(
                         htmlDiv(
                           className = "mini_container",
                           list(
                             htmlDiv(
                               id="ca",
                               list (
                                 htmlH2 ("0",  style = list(
                                   textAlign = 'left'
                                 ), id="count-facturation"),
                                 dccMarkdown("Chiffre d'affaire HT")
                               )
                             ),
                             htmlDiv(
                               id="tva",
                               list (
                                 htmlH3 ("0",  style = list(
                                   textAlign = 'left'
                                 ), id="count-facturation-tva"),
                                 dccMarkdown("TVA")
                               )
                             )
                           ), style= list("text-align" = "left", "padding" = "15px", 'width' = "47%")
                         ),
                         htmlDiv(
                           className = "mini_container",
                           list(
                             
                             htmlH3 ("Filtrer par entités :",  style = list(
                               textAlign = 'left'
                             )),
                             dccDropdown(
                               id = "filtre-facturation",
                               options = list(
                                 list(label="EcocupFR", value="EcocupFR"),
                                 list(label="EcocupBE", value="EcocupBE"),
                                 list(label="Greencup", value="Greencup")
                               ),
                               value = list("EcocupFR", "EcocupBE", "Greencup"),
                               multi = TRUE,
                               clearable = FALSE
                             )
                           ), style= list("text-align" = "right", "padding" = "20px", 'width' = "47%")
                           
                         )
                       ), style = list('columnCount' = 2)
                       
                       
                     ),
                     htmlDiv(
                       className = "pretty_container-slider",
                       list(
                         htmlH4 ("Factures de 0€ à 200000€ :",  id="container-range-slider-facturation", style = list(
                           textAlign = 'left', paddingRight = '20px'
                         )),
                         htmlH3 ("Filtrer par montant HT de facture :", style = list(
                           textAlign = 'left', paddingLeft = '20px'
                         )),
                         dccRangeSlider(
                           min=(-5),
                           id="slider-facturation",
                           max=50,
                           value=list(-5,50),
                           allowCross = FALSE,
                           step=5,
                           marks = list(
                             "-5" = list("label" = "-100000€"),
                             "0" = list("label" = "0€"),
                             "5" = list("label" = "100€"),
                             "10" = list("label" = "500€"),
                             "15" = list("label" = "1000€"),
                             "20" = list("label" = "2500€"),
                             "25" = list("label" = "5000€"),
                             "30" = list("label" = "10000€"),
                             "35" = list("label" = "25000€"),
                             "40" = list("label" = "50000€"),
                             "45" = list("label" = "100000€"),
                             "50" = list("label" = "350000€")
                           )
                         )
                       )
                     ),
                     
                     
                     
                     
                     
                     htmlDiv(
                       list (
                         htmlDiv(
                           className = "pretty_container",
                           dccLoading(id="loading-1",children=list(dccGraph(id = 'graph-facturation')), type="default")
                           
                         ),
                         htmlDiv(
                           className = 'container-display',
                           list(
                             htmlDiv(
                               className = "pretty_container-map",
                               list(
                                 htmlH2 ("Somme des montants de facture par département",  style = list(
                                   textAlign = 'center'
                                 )),
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/map-facturation-.xlsx)'),
                                   id = "facturation-download-map",
                                   style = list("text-align" = "right")
                                 ),
                                
                                  dccGraph(id = 'map-facturation')
                                 
                               ), style= list("text-align" = "left", "padding" = "15px", width = "47%"),
                               
                               
                             ),
                             htmlDiv(
                               className = "pretty_container-map",
                               list(
                                 htmlH2 ("Nombre de factures sur la période",  style = list(
                                   textAlign = 'center'
                                 )),
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/qty-facturation-.xlsx)'),
                                   id = "facturation-download-qty",
                                   style = list("text-align" = "right")
                                 ),
                                 htmlDiv(
                                   dccLoading(id="loading-facturation-qty",children=list(dccGraph(id = 'qty-facturation')), type="default")
                                 )
                               ), style= list("text-align" = "left", "padding" = "15px", width = "47%"),
                               
                               
                             )
                           ), style = list('columnCount' = 2)
                         ),
                         htmlDiv(
                           
                           
                           className = 'container-display',
                           list(
                             htmlDiv(
                               className = "mini_container",
                               list(
                                 htmlH2 ("Pourcentage des revenus par statut",  style = list(
                                   textAlign = 'center'
                                 )),
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/percent-statut-facturation-.xlsx)'),
                                   id = "facturation-download-percent-revenus",
                                   style = list("text-align" = "right")
                                 ),
                                 dccLoading(id="loading-location-2", children=list(dccGraph(id = 'pie-statut-montant')), type="default")
                               ), style= list("text-align" = "left", "padding" = "15px", 'width' = "30%")
                             ),
                             htmlDiv(
                               className = "mini_container",
                               list(
                                 htmlH2 ("Revenus dûs sur la période par entité",  style = list(
                                   textAlign = 'center'
                                 )),
                                 dccMarkdown(
                                   paste0('[Télécharger la liste des contacts en .xlsx](','http://statics.ecocup.com/revenus-dus-facturation-.xlsx)'),
                                   id = "facturation-download-du",
                                   style = list("text-align" = "right")
                                 ),
                                 dccLoading(id="loading-location-3", children=list(dccGraph(id = 'graph-du-montant')), type="default")
                                 
                               ), style= list("text-align" = "left", "padding" = "15px", 'width' = "67%")
                             )
                             
                             
                           ), style = list('columnCount' = 2)
                           
                           
                         ),                     
                         htmlDiv(
                           className = 'container-display',
                           list (
                             htmlDiv(
                               className = "mini_container",
                               list(
                                 htmlH2 ("Nombre de factures par type de clients",  style = list(
                                   textAlign = 'center'
                                 )),
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/qty-client-facturation-.xlsx)'),
                                   id = "facturation-download-qty-clienttype",
                                   style = list("text-align" = "right")
                                 ),
                                 dccLoading(id="loading-facturation-clienttype", children=list(dccGraph(id = 'periode-clientype')), type="default")
                               ), style= list("text-align" = "left", "padding" = "15px", 'width' = "67%")
                             ),
                             htmlDiv(
                               className = "mini_container",
                               list(
                                 htmlH2 ("Pourcentage des revenus par type de clients",  style = list(
                                   textAlign = 'center'
                                 )),
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/percent-client-facturation-.xlsx)'),
                                   id = "facturation-download-percent-clienttype",
                                   style = list("text-align" = "right")
                                 ),
                                 dccLoading(id="loading-facturation-pie-clienttype", children=list(dccGraph(id = 'pie-clientype')), type="default")
                                 
                               ), style= list("text-align" = "left", "padding" = "15px", 'width' = "30%")
                             )
                             
                           ), style = list('columnCount' = 2)
                           
                           
                         ),
                         htmlDiv(
                           
                           className = "mini_container",
                           list(
                             htmlH2 ("Données sur la période sélectionnée",  style = list(
                               textAlign = 'center'
                             )),
                             htmlDiv(
                               list(
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/facturation-.xlsx)'),
                                   id = "facturation-download"
                                 )
                               ), style=list( 'text-align' = "right")
                             ),
                             dashDataTable(
                               id = "table-facturation",
                               style_table= list(
                                 maxHeight = '300',
                                 border = 'thin lightgrey solid'
                               ),
                               
                               fixed_rows= list(headers = TRUE, data = 0),
                               style_cell = list(
                                 width = '150px',
                                 padding = '10px'
                               ),
                               style_header = list(
                                 backgroundColor = '#00a699',
                                 fontWeight = 'bold',
                                 color = "white"
                               ),
                               style_cell_conditional = lapply(c('date'),
                                                               function(name) {
                                                                 list(
                                                                   'if' = list('column_id' = name),
                                                                   textAlign = 'left'
                                                                 )
                                                               })
                             )
                           ), style= list("padding" = "20px")
                           
                           
                         )
                         
                         
                         
                       )
                     )
                     
                     
                     
                     
                     
                     
                       
                       
                       
                     
                   )
                   
                 ))
               }
               
               else if(tab == 'tab-4') {
                 return(htmlDiv(
                   list(
                     dccLoading(htmlDiv(id='dfProduits-value', style=list(display = 'none')), style =list("position" = "absolute", "top" = "100px")
                     ),
                     htmlDiv(id='jsonProduits-value', style=list(display = 'none')
                     ),
                     htmlDiv(id='jsonBrutProduits-value', style=list(display = 'none')
                     ),
                     htmlDiv(id='produits-id', children=sample(1:100000000, 1), style=list(display = 'none')
                     ),
                     htmlDiv(
                       
                       
                       className = 'container-display',
                       list(
                         htmlDiv(
                           className = "mini_container",
                           list(
                             htmlDiv(
                               
                               list (
                                 htmlH2 ("0",  style = list(
                                   textAlign = 'left'
                                 ), id="count-produits"),
                                 dccMarkdown("Nombre de produits sélectionnés sur la période")
                               )
                             )
                           ), style= list("text-align" = "left", "padding" = "15px", 'width' = "47%")
                         ),
                         htmlDiv(
                           className = "mini_container",
                           list(
                             
                             htmlH3 ("Filtrer par entités :",  style = list(
                               textAlign = 'left'
                             )),
                             dccDropdown(
                               id = "filtre-produits",
                               options = list(
                                 list(label="EcocupFR", value="EcocupFR"),
                                 list(label="EcocupBE", value="EcocupBE"),
                                 list(label="Greencup", value="Greencup")
                               ),
                               value = list("EcocupFR", "EcocupBE", "Greencup"),
                               multi = TRUE,
                               clearable = FALSE
                             )
                           ), style= list("text-align" = "right", "padding" = "20px", 'width' = "47%")
                           
                         )
                       ), style = list('columnCount' = 2)
                       
                       
                     ),
                     htmlDiv (
                       className = 'container-display',
                       list(
                         htmlDiv(
                           className = "mini_container",
                           list(
                             htmlH3 ("Filtrer par produits :",  style = list(
                               textAlign = 'left'
                             )),
                             dccDropdown(
                               id = "filtre-typeproduits",
                               options = list(
                                 list(label="ECO 12", value="ECO 12"),
                                 list(label="ECO 15", value="ECO 15"),
                                 list(label="ECO 18", value="ECO 18"),
                                 list(label="ECO 28", value="ECO 28"),
                                 list(label="ECO 30", value="ECO 30"),
                                 list(label="ECO 40", value="ECO 40"),
                                 list(label="ECO 50", value="ECO 50"),
                                 list(label="ECO 60", value="ECO 60"),
                                 
                                 list(label="G25", value="G25"),
                                 list(label="G28", value="G28"),
                                 list(label="G30", value="G30"),
                                 list(label="G40", value="G40"),
                                 list(label="G60", value="G60"),
                                 

                                 list(label="CARAFE", value="CARAFE"),
                                 list(label="ECO VIN", value="ECO VIN"),
                                 list(label="ECO CHAMP", value="ECO CHAMP"),
                                 list(label="COMPTEUSE", value="COMPTEUSE"),
                                 list(label="PORTE-VERRES", value="PORTE-VERRES"),
                                 list(label="ECO TUBO", value="ECO TUBO"),
                                 list(label="AFFICHE", value="AFFICHE"),
                                 list(label="ECO BODEGA", value="ECO BODEGA"),
                                 list(label="ECO MUG", value="ECO MUG"),
                                 list(label="ASSIETTE", value="ASSIETTE")
                                 
                                 
                               ),
                               value = list("ECO 12", "ECO 18", "ECO 28", "ECO 30", "ECO 60"),
                               multi = TRUE,
                               clearable = FALSE
                             )
                           ), style= list("text-align" = "left", "padding" = "15px", 'width' = "47%")
                         ),
                         htmlDiv(
                           className = "mini_container",
                           list(
                             htmlH3 ("Filtrer par type d'impression :",  style = list(
                               textAlign = 'left'
                             )),
                             dccDropdown(
                               id = "filtre-impression",
                               options = list(
                                 list(label="Serigraphie", value="Serigraphie"),
                                 list(label="Digitale", value="Digitale"),
                                 list(label="IML", value="IML"),
                                 list(label="A définir", value="A définir"),
                                 list(label="Sans impression", value="Sans impression")
                                 
                                 
                                 
                               ),
                               value = list("Serigraphie", "Digitale", "IML", "A définir", "Sans impression"),
                               multi = TRUE,
                               clearable = FALSE
                             )
                           ), style= list("text-align" = "left", "padding" = "15px", 'width' = "47%")
                         )
                         
                       ), style = list('columnCount' = 2)
                     ),
                     
                     
                     
                     
                     
                     htmlDiv(
                       list(
                         htmlDiv(
                           className = "pretty_container",
                           dccLoading(id="loading-graph-produits",children=list(dccGraph(id = 'graph-produits')), type="default")
                         ),
                         htmlDiv(
                           className = 'container-display',
                           list(
                             htmlDiv(
                               className = "pretty_container-map",
                               list(
                                 htmlH2 ("Quantité de produits par département",  style = list(
                                   textAlign = 'center'
                                 )),
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/map-produit-.xlsx)'),
                                   id = "produits-download-map",
                                   style = list("text-align" = "right")
                                 ),
                                 htmlDiv(
                                   id = "geo-map-loading-outer",
                                   children = list(
                                      dccGraph(id='map-produits')
                                   ))
                               ), style= list("text-align" = "left", "padding" = "15px", width = "40%"),
                               
                               
                             ),
                             htmlDiv(
                               className = "pretty_container-map",
                               list(
                                 htmlH2 ("Moyenne de prix à l'unité par quantité",  style = list(
                                   textAlign = 'center'
                                 )),
                                 htmlH5 ("Faîtes glisser l'axe horizontal pour voir plus de données",  style = list(
                                   textAlign = 'center', 'font-weight' = 'normal'
                                 )),
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/moyenne-produits-.xlsx)'),
                                   id = "moyenne-produits-download",
                                   style = list("text-align" = "right")
                                 ),
                                 dccLoading(id="loading-moyenne-produits",children=list(dccGraph(id = 'graph-moyenne-produits')), type="default")
                               ), style= list("text-align" = "left", "padding" = "15px", width = "57%"),
                               
                               
                             )
                           ), style = list('columnCount' = 2)
                         ),
                         htmlDiv(
                           className = 'container-display',
                           list(
                             htmlDiv(
                               className = "pretty_container",
                               id="frais-container",
                               list(
                                 htmlH2 ("Frais sur la période sélectionnée",  style = list(
                                   textAlign = 'center'
                                 )),
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/frais-produits-.xlsx)'),
                                   id = "frais-produits-download",
                                   style = list("text-align" = "right")
                                 ),
                                 
                                 dccLoading(id="loading-graph-frais",children=list(dccGraph(id = 'graph-frais-produits')), type="default")
                               ), style= list("text-align" = "left", "padding" = "15px", width = "47%")
                               
                             ),
                             htmlDiv(
                               className = "pretty_container",
                               id="remise-container",
                               list(
                                 htmlH2 ("Remises sur la période sélectionnée",  style = list(
                                   textAlign = 'center'
                                 )),
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/remise-produits-.xlsx)'),
                                   id = "remise-produits-download",
                                   style = list("text-align" = "right")
                                 ),
                                 
                                 dccLoading(id="loading-graph-remise",children=list(dccGraph(id = 'graph-remise-produits')), type="default")
                               ), style= list("text-align" = "left", "padding" = "15px", width = "47%")
                               
                             )
                             
                           ), style = list('columnCount' = 2)
                         ),
                         htmlDiv(
                           
                           className = "mini_container",
                           list(
                             htmlH2 ("Données sur la période sélectionnée",  style = list(
                               textAlign = 'center'
                             )),
                             htmlDiv(
                               list(
                                 dccMarkdown(
                                   paste0('[Télécharger en .xlsx](','http://statics.ecocup.com/produits-.xlsx)'),
                                   id = "produits-download"
                                 )
                               ), style=list( 'text-align' = "right")
                             ),
                             dashDataTable(
                               id = "table-produits",
                               style_table= list(
                                 maxHeight = '300',
                                 border = 'thin lightgrey solid'
                               ),
                               
                               fixed_rows= list(headers = TRUE, data = 0),
                               style_cell = list(
                                 width = '150px',
                                 padding = '10px'
                               ),
                               style_header = list(
                                 backgroundColor = '#00a699',
                                 fontWeight = 'bold',
                                 color = "white"
                               ),
                               style_cell_conditional = lapply(c('date'),
                                                               function(name) {
                                                                 list(
                                                                   'if' = list('column_id' = name),
                                                                   textAlign = 'left'
                                                                 )
                                                               })
                             )
                           ), style= list("padding" = "20px")
                           
                           
                         )
                       )
                     )
                     
                     
                     
                   )
                 ))
               }
               
               
             }
)

francejson <- NULL
#url <- 'https://framagit.org/tg/france-geojson/-/raw/master/departements.geojson'
#francejson <- rjson::fromJSON(file=url)


# url <- 'https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_provinces.geojson'
# italyjson <- rjson::fromJSON(file=url)

# url <- paste0(getwd(), '/assets/datas/geo/readme-swiss.json')
# suissejson <- rjson::fromJSON(file=url)
# 
# url <- paste0(getwd(), '/assets/datas/geo/spain-provinces.geojson')
# espagnejson <- rjson::fromJSON(file=url)
# 
# url <- 'https://raw.githubusercontent.com/isellsoap/deutschlandGeoJSON/master/4_kreise/1_sehr_hoch.geojson'
# allemagnejson <- rjson::fromJSON(file=url)
# 
# url <- paste0(getwd(), '/assets/datas/geo/belgique.geojson')
# belgiquejson <- rjson::fromJSON(file=url)






app$run_server(host = '0.0.0.0', port = Sys.getenv('PORT', 8050))


