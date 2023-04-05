library('jsonlite')
library('ggplot2')
library('plotly')
library('dplyr')

dfDevis <- fromJSON(txt = "http://personnel.ecocup.fr/bo2api/controllers/quotes/index.php")
#1 = Ecocup Fr / 2 = EcocupBe / 3 = Ecocup UK / 4 = Greencup

# A garder pour les percent
#dfDevis$cdate <- sapply(dfDevis$cdate, function(x) as.Date(x,origin="1970-01-01", format="%d/%m/%y"))
#dfDevis <- mutate(dfDevis, cdate= as.Date(cdate, format= "%d.%m.%Y",origin="1970-01-01"))





dfDevis$id_entite <- sapply(dfDevis$id_entite, function(x) gsub("1", "Ecocup FR", x))
dfDevis$id_entite <- sapply(dfDevis$id_entite, function(x) gsub("2", "Ecocup BE", x))
dfDevis$id_entite <- sapply(dfDevis$id_entite, function(x) gsub("3", "Ecocup UK", x))
dfDevis$id_entite <- sapply(dfDevis$id_entite, function(x) gsub("4", "Greencup", x))
dfDevis$cdate <- sapply(dfDevis$cdate, function(x) gsub('([0-9]+) .*', '\\1', x))
dfDevis$cdate <- sapply(dfDevis$cdate, function(x) gsub(' ', '', x))

# dfDevis$cdate <- sapply(dfDevis$cdate, function(x) strptime(x,format="%Y-%m-%d"))

dfDevis <- dfDevis %>% group_by(cdate, id_entite) %>% summarise(count=length(cdate))

#


#dfDevis$cdate <- sapply(dfDevis$cdate, function(x) as.Date (x))

View(dfDevis)

q <- ggplotly(ggplot(data=dfDevis, aes(x=as.Date(cdate,origin="1970-01-01"), y=count, colour=id_entite,mode = 'lines', text = paste("days from today"))) + geom_point(size=0.5)+ geom_line(size=0.3))
htmlwidgets::saveWidget(as_widget(q), 'devis.html')
