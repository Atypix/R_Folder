#!/usr/local/bin/Rscript

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
library('odbc')
library('RMySQL')

limit <- '2016-10-01'
DB <- dbConnect(RMySQL::MySQL(), user="goach", host="127.0.0.1", password="M0dp4s66", dbname="ecocup", port=3306)
source ('produits.R')
source ('devisVente.R')
source ('devisLocation.R')
source ('factures.R')


dbDisconnect(DB)

#setwd('/home/goach/Documents/workspace/R_Folder/ecocup-analytics/analytics/imports')