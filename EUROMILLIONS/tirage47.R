library('reticulate')
library('listarrays')
library('keras')
library('data.table')
library('dplyr')
library('tidyverse')
library('writexl')

library('tsibble')
#set.seed(124)
#setwd('/home/goach/Documents/workspace/R_Folder/EUROMILLIONS/')



n_timesteps <- 30
n_predictions <- 30
n_features <- 1
batch_size <- 1

epochs <- 500000




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

# euro <- read.csv(file = 'euros/euromillions.csv',header = FALSE, sep=';')
# euro = euro[-1,]
# euro = deleteCol (euro, 'euro')
# 
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

euro6 <- read.csv(file = 'euros/euromillions_202002.csv',header = FALSE, sep=';')
#
euro6 = euro6[-1,]
euro6 = deleteCol (euro6, 'euro6')
#Stop()
# df <- rbind(euro, euro2)
# df <- rbind(euro2, euro3)
# df <- rbind(df, euro4)
# df <- rbind(df, euro5)
# df <- rbind(df, euro6)

# 
#df <- euro6
df <- rbind(euro6, euro5)
df <- df[1:77,]

df <- df[order(as.Date(df$date, format="%Y-%m-%d")),]
df$date <- sapply(df$date, function(x) as.numeric(as.Date(as.character(x), origin="1970-01-01")))


df$boule_1 <- sapply(df$boule_1, function(x) as.numeric(as.character(x)))
df$boule_2 <- sapply(df$boule_2, function(x) as.numeric(as.character(x)))
df$boule_3 <- sapply(df$boule_3, function(x) as.numeric(as.character(x)))
df$boule_4 <- sapply(df$boule_4, function(x) as.numeric(as.character(x)))
df$boule_5 <- sapply(df$boule_5, function(x) as.numeric(as.character(x)))

df$etoile_1 <- sapply(df$etoile_1, function(x) as.numeric(as.character(x)))
df$etoile_2 <- sapply(df$etoile_2, function(x) as.numeric(as.character(x)))



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
scale_boules <- function(boule1, boule2, boule3, boule4, boule5, boule1_test, boule2_test, boule3_test, boule4_test, boule5_test, feature_range = c(0,1)) {
  x = boule1
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  
  std_boule1 = (boule1 - min(x)) / (max(x) - min(x))
  std_boule2 = (boule2 - min(x)) / (max(x) - min(x))
  std_boule3 = (boule3 - min(x)) / (max(x) - min(x))
  std_boule4 = (boule4 - min(x)) / (max(x) - min(x))
  std_boule5 = (boule5 - min(x)) / (max(x) - min(x))
  
  
  scaled_boule1 = std_boule1 * (fr_max - fr_min) + fr_min
  scaled_boule2 = std_boule2 * (fr_max - fr_min) + fr_min
  scaled_boule3 = std_boule3 * (fr_max - fr_min) + fr_min
  scaled_boule4 = std_boule4 * (fr_max - fr_min) + fr_min
  scaled_boule5 = std_boule5 * (fr_max - fr_min) + fr_min
  
  std_boule1_test = (boule1_test - min(x)) / (max(x) - min(x))
  std_boule2_test = (boule2_test - min(x)) / (max(x) - min(x))
  std_boule3_test = (boule3_test - min(x)) / (max(x) - min(x))
  std_boule4_test = (boule4_test - min(x)) / (max(x) - min(x))
  std_boule5_test = (boule5_test - min(x)) / (max(x) - min(x))
  
  
  scaled_boule1_test = std_boule1_test * (fr_max - fr_min) + fr_min
  scaled_boule2_test = std_boule2_test * (fr_max - fr_min) + fr_min
  scaled_boule3_test = std_boule3_test * (fr_max - fr_min) + fr_min
  scaled_boule4_test = std_boule4_test * (fr_max - fr_min) + fr_min
  scaled_boule5_test = std_boule5_test * (fr_max - fr_min) + fr_min
  
  return( list(scaled_boule1 = as.vector(scaled_boule1), 
               scaled_boule2 = as.vector(scaled_boule2),
               scaled_boule3 = as.vector(scaled_boule3),
               scaled_boule4 = as.vector(scaled_boule4),
               scaled_boule5 = as.vector(scaled_boule5),
               
               scaled_boule1_test = as.vector(scaled_boule1_test), 
               scaled_boule2_test = as.vector(scaled_boule2_test),
               scaled_boule3_test = as.vector(scaled_boule3_test),
               scaled_boule4_test = as.vector(scaled_boule4_test),
               scaled_boule5_test = as.vector(scaled_boule5_test),
               
               scaler= c(min =min(x), max = max(x))) )
}

scale_star <- function(star1, star2, star1_test, star2_test, feature_range = c(0,1)) {
  
  x = star1
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  
  std_star1 = (star1 - min(x)) / (max(x) - min(x))
  std_star2 = (star2 - min(x)) / (max(x) - min(x))
  
  std_star1_test = (star1_test - min(x)) / (max(x) - min(x))
  std_star2_test = (star2_test - min(x)) / (max(x) - min(x))
  
  scaled_star1 = std_star1 * (fr_max - fr_min) + fr_min
  scaled_star2 = std_star2 * (fr_max - fr_min) + fr_min
  
  scaled_star1_test = std_star1_test * (fr_max - fr_min) + fr_min
  scaled_star2_test = std_star2_test * (fr_max - fr_min) + fr_min
  
  
  return( list(scaled_star1 = as.vector(scaled_star1), 
               scaled_star2 = as.vector(scaled_star2),
               scaled_star1_test = as.vector(scaled_star1_test),
               scaled_star2_test = as.vector(scaled_star2_test),
               scaler= c(min =min(x), max = max(x))) )
  
}

scale_date <- function(train, test, feature_range = c(0,1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = (x - min(x)) / (max(x) - min(x))
  std_test = (test - min(x)) / (max(x) - min(x))
  
  scaled_train = std_train * (fr_max - fr_min) + fr_min
  scaled_test = std_test * (fr_max - fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
}

# build_matrix <- function(tseries, overall_timesteps) {
#   t(sapply(1:(length(tseries) - overall_timesteps +1), function(x) 
#     tseries[x:(x + overall_timesteps - 1)]))
# }

build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps +1), function(x) 
    tseries[x:(x + overall_timesteps-1)]))
}

reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}



datas <- df
n_ <- round(nrow(datas) * .95, digits = 0)
#train <- datas[1:(nrow(datas) - 24), ]
train <- datas[1:(nrow(datas)), ] 
test <- datas[(nrow(datas)-68):nrow(datas),] 
row.names(train) <- seq(1:nrow(train))
row.names(test) <- seq(1:nrow(test))

date_train <- train$date
date_test <- test$date

boule1 <- train$boule_1
boule2 <- train$boule_2
boule3 <- train$boule_3
boule4 <- train$boule_4
boule5 <- train$boule_5

boule1_test <- test$boule_1
boule2_test <- test$boule_2
boule3_test <- test$boule_3
boule4_test <- test$boule_4
boule5_test <- test$boule_5

star1 <- train$etoile_1
star2 <- train$etoile_2

star1_test <- test$etoile_1
star2_test <- test$etoile_2

message(">> normalisation des datas")

ScaledDate <- scale_date(as.data.frame(date_train), as.data.frame(date_test), c(0,1))
x_train_date <- ScaledDate$scaled_train[,1]
x_test_date <- ScaledDate$scaled_test[,1]

ScaledBoule <- scale_boules(as.data.frame(boule1), 
                            as.data.frame(boule2),
                            as.data.frame(boule3),
                            as.data.frame(boule4),
                            as.data.frame(boule5), 
                            as.data.frame(boule1_test), 
                            as.data.frame(boule2_test),
                            as.data.frame(boule3_test),
                            as.data.frame(boule4_test),
                            as.data.frame(boule5_test),
                            c(0,1))

x_train_boule1 <- ScaledBoule$scaled_boule1[,1]
x_train_boule2 <- ScaledBoule$scaled_boule2[,1]
x_train_boule3 <- ScaledBoule$scaled_boule3[,1]
x_train_boule4 <- ScaledBoule$scaled_boule4[,1]
x_train_boule5 <- ScaledBoule$scaled_boule5[,1]

x_train_boule1_test <- ScaledBoule$scaled_boule1_test[,1]
x_train_boule2_test <- ScaledBoule$scaled_boule2_test[,1]
x_train_boule3_test <- ScaledBoule$scaled_boule3_test[,1]
x_train_boule4_test <- ScaledBoule$scaled_boule4_test[,1]
x_train_boule5_test <- ScaledBoule$scaled_boule5_test[,1]


ScaledStar <- scale_star(as.data.frame(star1), 
                         as.data.frame(star2),
                         as.data.frame(star1_test),
                         as.data.frame(star2_test),
                         c(0,1))

x_train_star1 <- ScaledStar$scaled_star1[,1]
x_train_star2 <- ScaledStar$scaled_star2[,1]

x_test_star1 <- ScaledStar$scaled_star1_test[,1]
x_test_star2 <- ScaledStar$scaled_star2_test[,1]

train_matrix_date <- build_matrix(x_train_date, n_timesteps + n_predictions)
test_matrix_date <- build_matrix(x_test_date, n_timesteps + n_predictions)

##

train_matrix_boule1 <- build_matrix(x_train_boule1, n_timesteps + n_predictions)
train_matrix_boule2 <- build_matrix(x_train_boule2, n_timesteps + n_predictions)
train_matrix_boule3 <- build_matrix(x_train_boule3, n_timesteps + n_predictions)
train_matrix_boule4 <- build_matrix(x_train_boule4, n_timesteps + n_predictions)
train_matrix_boule5 <- build_matrix(x_train_boule5, n_timesteps + n_predictions)


train_matrix_boule1_test <- build_matrix(x_train_boule1_test, n_timesteps + n_predictions)
train_matrix_boule2_test <- build_matrix(x_train_boule2_test, n_timesteps + n_predictions)
train_matrix_boule3_test <- build_matrix(x_train_boule3_test, n_timesteps + n_predictions)
train_matrix_boule4_test <- build_matrix(x_train_boule4_test, n_timesteps + n_predictions)
train_matrix_boule5_test <- build_matrix(x_train_boule5_test, n_timesteps + n_predictions)

train_matrix_star1 <- build_matrix(x_train_star1, n_timesteps + n_predictions)
train_matrix_star2 <- build_matrix(x_train_star2, n_timesteps + n_predictions)

train_matrix_star1_test <- build_matrix(x_test_star1, n_timesteps + n_predictions)
train_matrix_star2_test <- build_matrix(x_test_star2, n_timesteps + n_predictions)

X_train_date <- train_matrix_date[, 1:n_timesteps]
y_train_date <- train_matrix_date[, (n_timesteps + 1):(n_timesteps * 2)]
X_test_date <- test_matrix_date[, 1:n_timesteps]
y_test_date <- test_matrix_date[, (n_timesteps + 1):(n_timesteps * 2)]

X_train_date <- X_train_date[1:(nrow(X_train_date) %/% batch_size * batch_size),]
y_train_date <- y_train_date[1:(nrow(y_train_date) %/% batch_size * batch_size),]
X_test_date <- X_test_date[1:(nrow(X_test_date) %/% batch_size * batch_size),]
y_test_date <- y_test_date[1:(nrow(y_test_date) %/% batch_size * batch_size),]

X_train_boule1 <- train_matrix_boule1[, 1:n_timesteps]
y_train_boule1 <- train_matrix_boule1[, (n_timesteps + 1):(n_timesteps * 2)]
X_test_boule1 <- train_matrix_boule1_test[, 1:n_timesteps]
y_test_boule1 <- train_matrix_boule1_test[, (n_timesteps + 1):(n_timesteps * 2)]

X_train_boule1 <- X_train_boule1[1:(nrow(X_train_boule1) %/% batch_size * batch_size),]
y_train_boule1 <- y_train_boule1[1:(nrow(y_train_boule1) %/% batch_size * batch_size),]
X_test_boule1 <- X_test_boule1[1:(nrow(X_test_boule1) %/% batch_size * batch_size),]
y_test_boule1 <- y_test_boule1[1:(nrow(y_test_boule1) %/% batch_size * batch_size),]

X_train_boule2 <- train_matrix_boule2[, 1:n_timesteps]
y_train_boule2 <- train_matrix_boule2[, (n_timesteps + 1):(n_timesteps * 2)]
X_test_boule2 <- train_matrix_boule2_test[, 1:n_timesteps]
y_test_boule2 <- train_matrix_boule2_test[, (n_timesteps + 1):(n_timesteps * 2)]

X_train_boule2 <- X_train_boule2[1:(nrow(X_train_boule2) %/% batch_size * batch_size),]
y_train_boule2 <- y_train_boule2[1:(nrow(y_train_boule2) %/% batch_size * batch_size),]
X_test_boule2 <- X_test_boule2[1:(nrow(X_test_boule2) %/% batch_size * batch_size),]
y_test_boule2 <- y_test_boule2[1:(nrow(y_test_boule2) %/% batch_size * batch_size),]

X_train_boule3 <- train_matrix_boule3[, 1:n_timesteps]
y_train_boule3 <- train_matrix_boule3[, (n_timesteps + 1):(n_timesteps * 2)]
X_test_boule3 <- train_matrix_boule3_test[, 1:n_timesteps]
y_test_boule3 <- train_matrix_boule3_test[, (n_timesteps + 1):(n_timesteps * 2)]

X_train_boule3 <- X_train_boule3[1:(nrow(X_train_boule3) %/% batch_size * batch_size),]
y_train_boule3 <- y_train_boule3[1:(nrow(y_train_boule3) %/% batch_size * batch_size),]
X_test_boule3 <- X_test_boule3[1:(nrow(X_test_boule3) %/% batch_size * batch_size),]
y_test_boule3 <- y_test_boule3[1:(nrow(y_test_boule3) %/% batch_size * batch_size),]

X_train_boule4 <- train_matrix_boule4[, 1:n_timesteps]
y_train_boule4 <- train_matrix_boule4[, (n_timesteps + 1):(n_timesteps * 2)]
X_test_boule4 <- train_matrix_boule4_test[, 1:n_timesteps]
y_test_boule4 <- train_matrix_boule4_test[, (n_timesteps + 1):(n_timesteps * 2)]

X_train_boule4 <- X_train_boule4[1:(nrow(X_train_boule4) %/% batch_size * batch_size),]
y_train_boule4 <- y_train_boule4[1:(nrow(y_train_boule4) %/% batch_size * batch_size),]
X_test_boule4 <- X_test_boule4[1:(nrow(X_test_boule4) %/% batch_size * batch_size),]
y_test_boule4 <- y_test_boule4[1:(nrow(y_test_boule4) %/% batch_size * batch_size),]

X_train_boule5 <- train_matrix_boule5[, 1:n_timesteps]
y_train_boule5 <- train_matrix_boule5[, (n_timesteps + 1):(n_timesteps * 2)]
X_test_boule5 <- train_matrix_boule5_test[, 1:n_timesteps]
y_test_boule5 <- train_matrix_boule5_test[, (n_timesteps + 1):(n_timesteps * 2)]

X_train_boule5 <- X_train_boule5[1:(nrow(X_train_boule5) %/% batch_size * batch_size),]
y_train_boule5 <- y_train_boule5[1:(nrow(y_train_boule5) %/% batch_size * batch_size),]
X_test_boule5 <- X_test_boule5[1:(nrow(X_test_boule5) %/% batch_size * batch_size),]
y_test_boule5 <- y_test_boule5[1:(nrow(y_test_boule5) %/% batch_size * batch_size),]

X_train_star1 <- train_matrix_star1[, 1:n_timesteps]
y_train_star1 <- train_matrix_star1[, (n_timesteps + 1):(n_timesteps * 2)]
X_test_star1 <- train_matrix_star1_test[, 1:n_timesteps]
y_test_star1 <- train_matrix_star1_test[, (n_timesteps + 1):(n_timesteps * 2)]

X_train_star1 <- X_train_star1[1:(nrow(X_train_star1) %/% batch_size * batch_size),]
y_train_star1 <- y_train_star1[1:(nrow(y_train_star1) %/% batch_size * batch_size),]
X_test_star1 <- X_test_star1[1:(nrow(X_test_star1) %/% batch_size * batch_size),]
y_test_star1 <- y_test_star1[1:(nrow(y_test_star1) %/% batch_size * batch_size),]

X_train_star2 <- train_matrix_star2[, 1:n_timesteps]
y_train_star2 <- train_matrix_star2[, (n_timesteps + 1):(n_timesteps * 2)]
X_test_star2 <- train_matrix_star2_test[, 1:n_timesteps]
y_test_star2 <- train_matrix_star2_test[, (n_timesteps + 1):(n_timesteps * 2)]

X_train_star2 <- X_train_star2[1:(nrow(X_train_star2) %/% batch_size * batch_size),]
y_train_star2 <- y_train_star2[1:(nrow(y_train_star2) %/% batch_size * batch_size),]
X_test_star2 <- X_test_star2[1:(nrow(X_test_star2) %/% batch_size * batch_size),]
y_test_star2 <- y_test_star2[1:(nrow(y_test_star2) %/% batch_size * batch_size),]

X_train_date <-  reshape_X_3d(X_train_date)
y_train_date <-  reshape_X_3d(y_train_date)

X_train_boule1 <- reshape_X_3d(X_train_boule1)
y_train_boule1 <- reshape_X_3d(y_train_boule1)
X_test_boule1 <- reshape_X_3d(X_test_boule1)
y_test_boule1 <- reshape_X_3d(y_test_boule1)

X_train_boule2 <- reshape_X_3d(X_train_boule2)
y_train_boule2 <- reshape_X_3d(y_train_boule2)
X_test_boule2 <- reshape_X_3d(X_test_boule2)
y_test_boule2 <- reshape_X_3d(y_test_boule2)

X_train_boule3 <- reshape_X_3d(X_train_boule3)
y_train_boule3 <- reshape_X_3d(y_train_boule3)
X_test_boule3 <- reshape_X_3d(X_test_boule3)
y_test_boule3 <- reshape_X_3d(y_test_boule3)

X_train_boule4 <- reshape_X_3d(X_train_boule4)
y_train_boule4 <- reshape_X_3d(y_train_boule4)
X_test_boule4 <- reshape_X_3d(X_test_boule4)
y_test_boule4 <- reshape_X_3d(y_test_boule4)

X_train_boule5 <- reshape_X_3d(X_train_boule5)
y_train_boule5 <- reshape_X_3d(y_train_boule5)
X_test_boule5 <- reshape_X_3d(X_test_boule5)
y_test_boule5 <- reshape_X_3d(y_test_boule5)

X_train_star1 <- reshape_X_3d(X_train_star1)
y_train_star1 <- reshape_X_3d(y_train_star1)
X_test_star1 <- reshape_X_3d(X_test_star1)
y_test_star1 <- reshape_X_3d(y_test_star1)

X_train_star2 <- reshape_X_3d(X_train_star2)
y_train_star2 <- reshape_X_3d(y_train_star2)
X_test_star2 <- reshape_X_3d(X_test_star2)
y_test_star2 <- reshape_X_3d(y_test_star2)


message(">> creation du model")


inputsLSTM_boule1 <- layer_input(shape = c(n_timesteps, n_features), name='lstm_input_boule1')

outputLSTM_boule1 <- inputsLSTM_boule1  %>%
  layer_lstm (units = 100, name='lstm_0_boule1') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout_0_boule1') %>% 
  layer_dense(n_timesteps) %>%
  layer_reshape(c(n_timesteps, 1))%>% 
  layer_dense(1)

modelLSTM_boule1 <- keras_model(
  inputs = inputsLSTM_boule1,
  outputs = outputLSTM_boule1
)

inputsLSTM_boule2 <- layer_input(shape = c(n_timesteps, n_features), name='lstm_input_boule2')

outputLSTM_boule2 <- inputsLSTM_boule2  %>%
  layer_lstm (units = 100, name='lstm_0_boule2') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout_0_boule2') %>% 
  layer_dense(n_timesteps) %>%
  layer_reshape(c(n_timesteps, 1))%>% 
  layer_dense(1) 

modelLSTM_boule2 <- keras_model(
  inputs = inputsLSTM_boule2,
  outputs = outputLSTM_boule2
)

inputsLSTM_boule3 <- layer_input(shape = c(n_timesteps, n_features), name='lstm_input_boule3')

outputLSTM_boule3 <- inputsLSTM_boule3  %>%
  layer_lstm (units = 100, name='lstm_0_boule3') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout_0_boule3') %>% 
  layer_dense(n_timesteps) %>%
  layer_reshape(c(n_timesteps, 1))%>% 
  layer_dense(1) 

modelLSTM_boule3 <- keras_model(
  inputs = inputsLSTM_boule3,
  outputs = outputLSTM_boule3
)

inputsLSTM_boule4 <- layer_input(shape = c(n_timesteps, n_features), name='lstm_input_boule4')

outputLSTM_boule4 <- inputsLSTM_boule4  %>%
  layer_lstm (units = 100, name='lstm_0_boule4') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout_0_boule4') %>% 
  layer_dense(n_timesteps) %>%
  layer_reshape(c(n_timesteps, 1))%>% 
  layer_dense(1) 

modelLSTM_boule4 <- keras_model(
  inputs = inputsLSTM_boule4,
  outputs = outputLSTM_boule4
)

inputsLSTM_boule5 <- layer_input(shape = c(n_timesteps, n_features), name='lstm_input_boule5')

outputLSTM_boule5 <- inputsLSTM_boule5  %>%
  layer_lstm (units = 100, name='lstm_0_boule5') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout_0_boule5') %>% 
  layer_dense(n_timesteps) %>%
  layer_reshape(c(n_timesteps, 1))%>% 
  layer_dense(1) 

modelLSTM_boule5 <- keras_model(
  inputs = inputsLSTM_boule5,
  outputs = outputLSTM_boule5
)

inputsLSTM_star1 <- layer_input(shape = c(n_timesteps, n_features), name='lstm_input_star1')

outputLSTM_star1 <- inputsLSTM_star1  %>%
  layer_lstm (units = 100, name='lstm_0_star1') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout_0_star1') %>% 
  layer_dense(n_timesteps) %>%
  layer_reshape(c(n_timesteps, 1))%>% 
  layer_dense(1) 

modelLSTM_star1 <- keras_model(
  inputs = inputsLSTM_star1,
  outputs = outputLSTM_star1
)

inputsLSTM_star2 <- layer_input(shape = c(n_timesteps, n_features), name='lstm_input_star2')

outputLSTM_star2 <- inputsLSTM_star2  %>%
  layer_lstm (units = 100, name='lstm_0_star2') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout_0_star2') %>% 
  layer_dense(n_timesteps) %>%
  layer_reshape(c(n_timesteps, 1))%>% 
  layer_dense(1)

modelLSTM_star2 <- keras_model(
  inputs = inputsLSTM_star2,
  outputs = outputLSTM_star2
)

inputs_date <- layer_input (shape = c(n_timesteps, n_features), name='date_input') 

output_date  <- inputs_date %>%
  layer_lstm (units = 100, name='lstm_0_date') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout_0_date') %>% 
  layer_dense(n_timesteps) %>%
  layer_reshape(c(n_timesteps, 1)) %>% 
  layer_dense(1) 

model_date <- keras_model(
  inputs = inputs_date,
  outputs = output_date
)



model <- keras_model(
  inputs = c(model_date$input, 
             modelLSTM_boule1$input,
             modelLSTM_boule2$input,
             modelLSTM_boule3$input,
             modelLSTM_boule4$input,
             modelLSTM_boule5$input,
             modelLSTM_star1$input,
             modelLSTM_star2$input),

  outputs = c(model_date$output, 
              modelLSTM_boule1$output,
              modelLSTM_boule2$output,
              modelLSTM_boule3$output,
              modelLSTM_boule4$output,
              modelLSTM_boule5$output,
              modelLSTM_star1$output,
              modelLSTM_star2$output))




adam <- optimizer_adam(lr = 0.0005)

message(">> compilation du model")
model %>% compile(
  loss="mean_squared_error",
  optimizer=optimizer_sgd(),
  metrics="mean_squared_error"
)


message(">> Training")
#model <- load_model_hdf5('tirage44.h5')
history <- model %>% fit(y = list(y_train_date, y_train_boule1, y_train_boule2, y_train_boule3, y_train_boule4, y_train_boule5, y_train_star1, y_train_star2),
                         x = list(X_train_date, X_train_boule1, X_train_boule2, X_train_boule3, X_train_boule4, X_train_boule5, X_train_star1, X_train_star2),
                         epochs = epochs,
                         batch_size=batch_size,
                         verbose = 1,
                         validation_split = 0.1,
                         shuffle=FALSE
)
# 
# plot(history, metrics="loss")

#


#pred_test <- model %>% predict(list(X_test_date,X_test_boule1, X_test_boule2, X_test_boule3, X_test_boule4, X_test_boule5, X_test_star1, X_test_star2), verbose = 1, batch_size=batch_size)
#pred_test <- model %>% predict(list(y_test_date,y_test_boule1, y_test_boule2, y_test_boule3, y_test_boule4, y_test_boule5, y_test_star1, y_test_star2), verbose = 1, batch_size=1)

pred_test <- model %>% predict(list(y_train_date, y_train_boule1, y_train_boule2, y_train_boule3, y_train_boule4, y_train_boule5, y_train_star1, y_train_star2), verbose = 1, batch_size=1)

#pred_test <- model %>% predict(list(X_train_date, X_train_boule1, X_train_boule2, X_train_boule3, X_train_boule4, X_train_boule5, X_train_star1, X_train_star2), verbose = 1, batch_size=batch_size)
date_predict <- unlist(reverse_scaling(as.data.frame(pred_test[1]), ScaledDate$scaler))
boule1 <- unlist(reverse_scaling(as.data.frame(pred_test[2]), ScaledBoule$scaler))
boule2 <- unlist(reverse_scaling(as.data.frame(pred_test[3]), ScaledBoule$scaler))
boule3 <- unlist(reverse_scaling(as.data.frame(pred_test[4]), ScaledBoule$scaler))
boule4 <- unlist(reverse_scaling(as.data.frame(pred_test[5]), ScaledBoule$scaler))
boule5 <- unlist(reverse_scaling(as.data.frame(pred_test[6]), ScaledBoule$scaler))
star1 <- unlist(reverse_scaling(as.data.frame(pred_test[7]), ScaledStar$scaler))
star2 <- unlist(reverse_scaling(as.data.frame(pred_test[8]), ScaledStar$scaler))

view_test <- cbind(date_predict, boule1)
view_test <- cbind(view_test, boule2)
view_test <- cbind(view_test, boule3)
view_test <- cbind(view_test, boule4)
view_test <- cbind(view_test, boule5)
view_test <- cbind(view_test, star1)
view_test <- cbind(view_test, star2)



view_test <- as.data.frame(view_test)
names(view_test)[1] <- 'date'
names(view_test)[2] <- 'x1'
names(view_test)[3] <- 'x2'
names(view_test)[4] <- 'x3'
names(view_test)[5] <- 'x4'
names(view_test)[6] <- 'x5'
names(view_test)[7] <- 'x6'
names(view_test)[8] <- 'x7'

view_test$date <- sapply(view_test$date, function(x) round(x))
view_test$x1 <- sapply(view_test$x1, function(x) round(x))
view_test$x2 <- sapply(view_test$x2, function(x) round(x))
view_test$x3 <- sapply(view_test$x3, function(x) round(x))
view_test$x4 <- sapply(view_test$x4, function(x) round(x))
view_test$x5 <- sapply(view_test$x5, function(x) round(x))
view_test$x6 <- sapply(view_test$x6, function(x) round(x))
view_test$x7 <- sapply(view_test$x7, function(x) round(x))
view_test$date <- sapply(view_test$date, function(x) format(as.Date(as.numeric(as.character(x)), origin="1970-01-01"), "%Y-%m-%d"))
#df$date <- sapply(df$date, function(x) format(as.Date(x, origin="1970-01-01"), "%Y-%m-%d"))

View(view_test)






