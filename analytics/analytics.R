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


map <- plot_ly()
    DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)

app <- Dash$new()

source('tabs/devis.R')
source('tabs/location.R')
source('tabs/facturation.R')

app$layout(htmlDiv(list(
  htmlH1 ("Analytics CUPS",  style = list(
    textAlign = 'center'
  )),
  dccTabs(id="tabs", value='tab-1', children=list(
    dccTab(label='Devis Vente', value='tab-1'),
    dccTab(label='Devis Location', value='tab-2'),
    dccTab(label='Facturation', value='tab-3'),
    dccTab(label='Commandes', value='tab-4'),
    dccTab(label='Produits', value='tab-5'),
    dccTab(label='Concurrence', value='tab-6')
  )),
  htmlDiv(id='tabs-content')
)))

app$callback(output('tabs-content', 'children'),
             params = list(input('tabs', 'value')),
             function(tab){
               if(tab == 'tab-1'){
                 return(htmlDiv(

                       list(
                         
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
                               ), style= list("text-align" = "left", "padding" = "15px", 'width' = "30%")
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
                               ), style= list("text-align" = "right", "padding" = "20px", 'width' = "30%")
                               
                             ),
                             htmlDiv(
                               className = "mini_container",
                               list(
                                 htmlH3 ("Filtrer par période :",  style = list(
                                   textAlign = 'left'
                                 )),
                                 dccDatePickerRange(
                                   className = "range-periode",
                                   id='my-date-picker',
                                   display_format = "DD/MM/Y",
                                   min_date_allowed=as.Date("2015-01-01"),
                                   max_date_allowed=Sys.Date() - 1,
                                   start_date  =  as.Date(startDate),
                                   end_date  =  Sys.Date() - 1
                                 ),
                                 dccDropdown(
                                   className = "dropdown-periode",
                                   id = "devis-periode",
                                   
                                   clearable = FALSE
                                 )
                               ), style= list("text-align" = "left", "padding" = "15px", 'width' = "30%")
                             )
                           ), style = list('columnCount' = 3)


                         ),
                         htmlDiv(
                           className = "pretty_container",
                           dccLoading(id="loading-1",children=list(dccGraph(id = 'graph-devis')), type="default")
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
                                       paste0('[Télécharger en .xlsx](','/assets/datas/devis/devis-pie.xlsx)'),
                                       id = "devis-download"
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
                                       paste0('[Télécharger en .xlsx](','/assets/datas/devis/devis-entite.xlsx)'),
                                       id = "devis-download"
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
                                       paste0('[Télécharger en .xlsx](','/assets/datas/devis/devis-statut.xlsx)'),
                                       id = "devis-download"
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
                                         paste0('[Télécharger en .xlsx](','/assets/datas/devis/devis-periode-statut.xlsx)'),
                                         id = "devis-download"
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
                                       paste0('[Télécharger en .xlsx](','/assets/datas/devis/devis-periode-entite.xlsx)'),
                                       id = "devis-download"
                                     )
                                   ), style=list( 'text-align' = "right")
                                 ),
                                 dccLoading(id="loading-graph-devis-montant",children=list(dccGraph(id = 'graph-devis-montant-entite')), type="default")
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
                                   paste0('[Télécharger en .xlsx](','/assets/datas/devis/devis.xlsx)'),
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
                           ), style= list("text-align" = "left", "padding" = "15px", 'width' = "30%")
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
                           ), style= list("text-align" = "right", "padding" = "20px", 'width' = "30%")
                           
                         ),
                         htmlDiv(
                           className = "mini_container",
                           list(
                             htmlH3 ("Filtrer par période :",  style = list(
                               textAlign = 'left'
                             )),
                             dccDatePickerRange(
                               className = "range-periode",
                               id='my-date-picker',
                               display_format = "DD/MM/Y",
                               min_date_allowed=as.Date("2015-01-01"),
                               max_date_allowed=Sys.Date() - 1,
                               start_date  =  as.Date(startDate),
                               end_date  =  Sys.Date() - 1
                             ),
                             dccDropdown(
                               className = "dropdown-periode",
                               id = "location-periode",
                               
                               clearable = FALSE
                             )
                           ), style= list("text-align" = "left", "padding" = "15px", 'width' = "30%")
                         )
                       ), style = list('columnCount' = 3)
                       
                       
                     ),
                     htmlDiv(
                       className = "pretty_container",
                       dccLoading(id="loading-1",children=list(dccGraph(id = 'graph-location')), type="default")
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
                                   paste0('[Télécharger en .xlsx](','/assets/datas/locations/location-pie.xlsx)'),
                                   id = "location-download"
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
                                   paste0('[Télécharger en .xlsx](','/assets/datas/locations/location-entite.xlsx)'),
                                   id = "location-download"
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
                                   paste0('[Télécharger en .xlsx](','/assets/datas/locations/location-statut.xlsx)'),
                                   id = "location-download"
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
                                   paste0('[Télécharger en .xlsx](','/assets/datas/locations/location-periode-statut.xlsx)'),
                                   id = "location-download"
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
                                   paste0('[Télécharger en .xlsx](','/assets/datas/locations/location-periode-entite.xlsx)'),
                                   id = "location-download"
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
                               paste0('[Télécharger en .xlsx](','/assets/datas/locations/location.xlsx)'),
                               id = "location-download"
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
                           ), style= list("text-align" = "left", "padding" = "15px", 'width' = "30%")
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
                           ), style= list("text-align" = "right", "padding" = "20px", 'width' = "30%")
                           
                         ),
                         htmlDiv(
                           className = "mini_container",
                           list(
                             htmlH3 ("Filtrer par période :",  style = list(
                               textAlign = 'left'
                             )),
                             dccDatePickerRange(
                               className = "range-periode",
                               id='my-date-picker-facturation',
                               display_format = "DD/MM/Y",
                               min_date_allowed=as.Date("2015-01-01"),
                               max_date_allowed=Sys.Date() - 1,
                               start_date  =  as.Date(startDate),
                               end_date  =  Sys.Date() - 1
                             ),
                             dccDropdown(
                               className = "dropdown-periode",
                               id = "facturation-periode",
                               
                               clearable = FALSE
                             )
                           ), style= list("text-align" = "left", "padding" = "15px", 'width' = "30%")
                         )
                       ), style = list('columnCount' = 3)
                       
                       
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
                           min=0,
                           id="slider-facturation",
                           max=50,
                           value=list(0,50),
                           allowCross = FALSE,
                           step=5,
                           marks = list(
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
                       className = "pretty_container",
                       dccLoading(id="loading-facturation-1",children=list(dccGraph(id = 'graph-facturation')), type="default")
                       
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
                             htmlDiv(
                               dccGraph(id = 'map-facturation', figure=map)
                             )
                           ), style= list("text-align" = "left", "padding" = "15px", width = "47%"),
                           
                           
                         ),
                         htmlDiv(
                           className = "pretty_container-map",
                           list(
                             htmlH2 ("Nombre de factures sur la période",  style = list(
                               textAlign = 'center'
                             )),
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
                             dccLoading(id="loading-location-2", children=list(dccGraph(id = 'pie-statut-montant')), type="default")
                           ), style= list("text-align" = "left", "padding" = "15px", 'width' = "30%")
                         ),
                         htmlDiv(
                           className = "mini_container",
                           list(
                             htmlH2 ("Revenus dûs sur la période sélectionnée par entité",  style = list(
                               textAlign = 'center'
                             )),
                             dccLoading(id="loading-location-3", children=list(dccGraph(id = 'graph-du-montant')), type="default")
                             
                           ), style= list("text-align" = "left", "padding" = "15px", 'width' = "64%")
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
                               paste0('[Télécharger en .xlsx](','/assets/datas/factures/facture.xlsx)'),
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


