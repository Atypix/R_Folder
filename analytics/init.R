r <- getOption('repos')
              r['CRAN'] <- 'http://cloud.r-project.org'
              options(repos=r)

              # ======================================================================

              # packages go here
              install.packages('remotes')
              install.packages('jsonlite')
              install.packages('ggplot2')
              install.packages('plotly')
              install.packages('dplyr')
              install.packages('reshape2')
              install.packages('writexl')
              install.packages('htmlwidgets')
              install.packages('tidyverse')
              install.packages('lubridate')
              install.packages('ISOweek')
              install.packages('maps')
              install.packages ('geojsonR')
              install.packages('rjson')
              install.packages('RJSONIO')
              install.packages('RMySQL')

              remotes::install_github('plotly/dashR', upgrade=TRUE)