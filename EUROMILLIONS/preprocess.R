library('reticulate')
library('listarrays')
library('keras')
library('data.table')
library('dplyr')
library('tidyverse')
library('writexl')
library('caret')
library('tidyverse')
#setwd('/home/goach/Documents/workspace/R_Folder/EUROMILLIONS/')
set.seed(3033)

deleteCol <-function (df, name) {
  if (name == 'euro') {
    df[1:2] <- NULL
    df[2] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  if (name == 'euro2') {
    df[1:2] <- NULL
    df[2] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  if (name == 'euro3') {
    df[1:2] <- NULL
    df[2] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  if (name == 'euro4') {
    df[1:2] <- NULL
    df[2:3] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  if (name == 'euro5') {
    df[1:2] <- NULL
    df[2:3] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  if (name == 'euro6') {
    df[1:2] <- NULL
    df[2:3] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  names(df)[1] <- 'date'
  names(df)[2] <- 'boule_1'
  names(df)[3] <- 'boule_2'
  names(df)[4] <- 'boule_3'
  names(df)[5] <- 'boule_4'
  names(df)[6] <- 'boule_5'
  names(df)[7] <- 'etoile_1'
  names(df)[8] <- 'etoile_2'
  if(name == 'euro') {
    df$date <- sapply(df$date, function(x) format(as.Date(x, "%Y%m%d"), "%Y-%m-%d"))
  } else {
    df$date <- sapply(df$date, function(x) format(as.Date(x, "%d/%m/%Y"), "%Y-%m-%d"))
  }
  df$date[df$date=='16-09-23'] <- '2016-09-23' 
  return (df)
}

euro5 <- read.csv(file = 'euros/euromillions_5.csv',header = FALSE, sep=';')
euro5 = euro5[-1,]
euro5 = deleteCol (euro5, 'euro5')

euro6 <- read.csv(file ='euros/euromillions_202002.csv',header = FALSE, sep=';')
euro6 = euro6[-1,]
euro6 = deleteCol (euro6, 'euro6')

df <- rbind(euro5, euro6)

df <- df[order(as.Date(df$date, format="%Y-%m-%d")),]
df$date <- sapply(df$date, function(x) as.numeric(as.Date(as.character(x), origin="1970-01-01")))
df <- df[80:180,]



df$boule_1 <- sapply(df$boule_1, function(x) as.numeric(as.character(x)))

boule = as.data.frame(df$boule_1)
names(boule)[names(boule) == "boule_1"] <- "boule"
training_size=floor(0.9*length(boule))
boule.train <- boule[1:training_size]
boule.test <- boule[training_size+1:length(boule)-training_size]

data_preprocess_value=preProcess(boule.train, method=c("center", "scale"))
boule.train.scaled = predict(data_preprocess_value, boule.train)
boule.test.scaled = predict(data_preprocess_value, boule.test)



revPredict <- function(preproc, data, digits=0) {
  data %>%
    select(one_of(preproc$mean %>% names)) %>%
    map2_df(preproc$std, ., function(sig, dat) dat * sig) %>%
    map2_df(preproc$mean, ., function(mu, dat) dat + mu)
}

datas <- revPredict(data_preprocess_value, boule.train.scaled)

View(datas)