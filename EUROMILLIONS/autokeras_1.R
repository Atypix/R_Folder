rm(list=ls())
library('keras')

library("autokeras")
#
#use_condaenv('py35',required=TRUE)
# reticulate::py_config()
# tf_config()

seed = 42
reticulate::py_set_seed(seed)
set.seed(seed)
# tensorflow::tf$random$set_seed(42)

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
euro2 <- read.csv(file = 'euros/euromillions_2.csv',header = FALSE, sep=';')
euro2 = euro2[-1,]
euro2 = deleteCol (euro2, 'euro2')

euro3 <- read.csv(file = 'euros/euromillions_3.csv',header = FALSE, sep=';')
euro3 = euro3[-1,]
euro3 = deleteCol (euro3, 'euro3')

euro4 <- read.csv(file = 'euros/euromillions_4.csv',header = FALSE, sep=';')
euro4 = euro4[-1,]
euro4 = deleteCol (euro4, 'euro4')

euro5 <- read.csv(file = 'euros/euromillions_5.csv',header = FALSE, sep=';')
euro5 = euro5[-1,]
euro5 = deleteCol (euro5, 'euro5')

euro6 <- read.csv(file ='euros/euromillions_202002.csv',header = FALSE, sep=';')
euro6 = euro6[-1,]
euro6 = deleteCol (euro6, 'euro6')
# df <- rbind(euro, euro2)
 df <- rbind(euro2, euro3)
 df <- rbind(df, euro4)
 df <- rbind(df, euro5)
 df <- rbind(df, euro6)
#df <- rbind(euro5, euro6)

df <- df[order(as.Date(df$date, format="%Y-%m-%d")),]
df$date <- sapply(df$date, function(x) as.numeric(as.Date(as.character(x), origin="1970-01-01")))

df$boule_1 <- sapply(df$boule_1, function(x) as.numeric(as.character(x)))
df$boule_2 <- sapply(df$boule_2, function(x) as.numeric(as.character(x)))
df$boule_3 <- sapply(df$boule_3, function(x) as.numeric(as.character(x)))
df$boule_4 <- sapply(df$boule_4, function(x) as.numeric(as.character(x)))
df$boule_5 <- sapply(df$boule_5, function(x) as.numeric(as.character(x)))

df$etoile_1 <- sapply(df$etoile_1, function(x) as.numeric(as.character(x)))
df$etoile_2 <- sapply(df$etoile_2, function(x) as.numeric(as.character(x)))

train <- df[1:(nrow(df)-9),]
test <- df[10:nrow(df),]

train_boule <- train$boule_1
test_boule <- test$boule_1

reverse_scaling <- function(scaled, scaler, feature_range = c(0,1)) {
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for(i in 1:t) {
    X = (scaled[i] - mins) / (maxs - mins)
    rawValues = X * (max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

scale_boule <- function(boule_train, boule_test, feature_range = c(0,1)) {
  
  x = boule_train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  
  std_boule_train = (boule_train - min(x)) / (max(x) - min(x))
  std_boule_test = (boule_test - min(x)) / (max(x) - min(x))
  

  
  scaled_boule_train = std_boule_train * (fr_max - fr_min) + fr_min
  scaled_boule_test = std_boule_test * (fr_max - fr_min) + fr_min
  

  
  
  return( list(scaled_boule_train = as.vector(scaled_boule_train), 
               scaled_boule_test = as.vector(scaled_boule_test),
               scaler= c(min =min(x), max = max(x))) )
  
}

Scaled <- scale_boule(as.data.frame(train_boule), as.data.frame(test_boule), c(0,1))
x_train <- as.data.frame(Scaled$scaled_boule_train[,1])
x_test <- as.data.frame(Scaled$scaled_boule_test[,1])

x_train <- cbind (x_train, x_test)

names(x_train)[1] <- "boule"
names(x_test)[1] <- "boule"


train_file <- paste0(tempdir(), "/train.csv")
write.csv(train, train_file, row.names = FALSE)

test_file_to_predict <- paste0(tempdir(), "/test_2_pred.csv")
write.csv(test, test_file_to_predict, row.names = FALSE)

reg <- model_structured_data_regressor(max_trials = 100) %>% 
  fit(train_file, 'boule_2', epochs = 1000)

(predicted_y <- reg %>% predict(test_file_to_predict))
